#include <Rcpp.h>
#include <RcppParallel.h>
#include <treevolume/kernels.hpp>

#include <array>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <vector>

namespace {

bool same_double(double left, double right) {
  return left == right || (std::isnan(left) && std::isnan(right));
}

struct EvaluationResult {
  double value = std::numeric_limits<double>::quiet_NaN();
  int status = 54;
};

template <typename DiameterFunction>
EvaluationResult gauss_legendre_volume(double lower, double upper,
                                       DiameterFunction diameter,
                                       double segment_size = 1.0,
                                       double area_divisor = 576.0) {
  static const double nodes[] = {
      -0.9061798459386640, -0.5384693101056831, 0.0,
      0.5384693101056831, 0.9061798459386640};
  static const double weights[] = {
      0.2369268850561891, 0.4786286704993665, 0.5688888888888889,
      0.4786286704993665, 0.2369268850561891};
  if (!(lower < upper)) {
    return EvaluationResult{};
  }
  double total = 0.0;
  for (double left = lower; left < upper;) {
    const double right = std::min(left + segment_size, upper);
    const double midpoint = (left + right) / 2.0;
    const double half_width = (right - left) / 2.0;
    for (int node = 0; node < 5; ++node) {
      const EvaluationResult evaluated = diameter(
          midpoint + half_width * nodes[node]);
      if (evaluated.status != 0) {
        return evaluated;
      }
      total += 3.14159265358979323846 * evaluated.value * evaluated.value /
          area_divisor * half_width * weights[node];
    }
    left = right;
  }
  return std::isfinite(total) ? EvaluationResult{total, 0}
                              : EvaluationResult{};
}

template <typename DiameterFunction>
EvaluationResult discover_inverse(double ht, double stump, double target,
                                  DiameterFunction diameter,
                                  double grid_step = 1.0 / (12.0 * 16.0)) {
  const double span = std::max(ht - stump, 0.0);
  const R_xlen_t intervals = std::max<R_xlen_t>(
      1, static_cast<R_xlen_t>(std::ceil(span / grid_step)));
  EvaluationResult left_result = diameter(stump);
  if (left_result.status != 0) {
    return left_result;
  }
  const double first_difference = left_result.value - target;
  double left_height = stump;
  double left_difference = first_difference;
  double highest = std::numeric_limits<double>::quiet_NaN();
  int crossing_count = 0;
  if (left_difference == 0.0) {
    highest = left_height;
    ++crossing_count;
  }
  double final_difference = left_difference;
  for (R_xlen_t interval = 1; interval <= intervals; ++interval) {
    const double right_height = interval == intervals
        ? ht : stump + static_cast<double>(interval) * grid_step;
    const EvaluationResult right_result = diameter(right_height);
    if (right_result.status != 0) {
      return right_result;
    }
    const double right_difference = right_result.value - target;
    if (right_difference == 0.0) {
      highest = std::isfinite(highest)
          ? std::max(highest, right_height) : right_height;
      ++crossing_count;
    }
    if ((left_difference < 0.0 && right_difference > 0.0) ||
        (left_difference > 0.0 && right_difference < 0.0)) {
      double lower = left_height;
      double upper = right_height;
      double lower_difference = left_difference;
      int iteration = 0;
      for (; iteration < 200 && upper - lower > 1e-4; ++iteration) {
        const double midpoint = (lower + upper) / 2.0;
        const EvaluationResult midpoint_result = diameter(midpoint);
        if (midpoint_result.status != 0) {
          return midpoint_result;
        }
        const double midpoint_difference = midpoint_result.value - target;
        if (midpoint_difference == 0.0) {
          lower = midpoint;
          upper = midpoint;
        } else if (lower_difference * midpoint_difference <= 0.0) {
          upper = midpoint;
        } else {
          lower = midpoint;
          lower_difference = midpoint_difference;
        }
      }
      if (upper - lower > 1e-4) {
        return EvaluationResult{
            std::numeric_limits<double>::quiet_NaN(), 103};
      }
      const double root = (lower + upper) / 2.0;
      highest = std::isfinite(highest) ? std::max(highest, root) : root;
      ++crossing_count;
    }
    left_height = right_height;
    left_difference = right_difference;
    final_difference = right_difference;
  }
  if (crossing_count == 0) {
    const int status = final_difference > 0.0 ? 100
        : first_difference < 0.0 ? 101 : 103;
    return EvaluationResult{std::numeric_limits<double>::quiet_NaN(), status};
  }
  return EvaluationResult{highest, crossing_count > 1 ? 102 : 0};
}

EvaluationResult combine_analytic_inverse(
    const EvaluationResult& analytic, const EvaluationResult& discovered) {
  if (analytic.status != 0 && analytic.status != 52 && analytic.status != 102) {
    return EvaluationResult{
        std::numeric_limits<double>::quiet_NaN(), analytic.status};
  }
  if (discovered.status != 0 && discovered.status != 102) {
    return EvaluationResult{
        std::numeric_limits<double>::quiet_NaN(), discovered.status};
  }
  return EvaluationResult{
      analytic.value, discovered.status == 102 ? 102 : analytic.status};
}

std::uint64_t double_bits(double value) {
  if (value == 0.0) {
    return 0;
  }
  if (std::isnan(value)) {
    return UINT64_C(0x7ff8000000000000);
  }
  std::uint64_t bits;
  std::memcpy(&bits, &value, sizeof(bits));
  return bits;
}

using KernelKey = std::array<std::uint64_t, 14>;

struct KernelKeyHash {
  std::size_t operator()(const KernelKey& key) const {
    std::size_t hash = 0;
    for (std::uint64_t value : key) {
      hash ^= static_cast<std::size_t>(value) +
          static_cast<std::size_t>(UINT64_C(0x9e3779b97f4a7c15)) +
          (hash << 6) + (hash >> 2);
    }
    return hash;
  }
};

KernelKey kernel_key_at(
    R_xlen_t index, const Rcpp::NumericVector& dbh,
    const Rcpp::NumericVector& ht, const Rcpp::NumericVector& x1,
    const Rcpp::NumericVector& x2, const Rcpp::NumericVector& bark_ratio,
    const Rcpp::NumericVector& upper_ht1,
    const Rcpp::NumericVector& upper_d1,
    const Rcpp::NumericVector& upper_ht2,
    const Rcpp::NumericVector& upper_d2,
    const Rcpp::IntegerVector& upper_bark,
    const Rcpp::NumericVector& site_index,
    const Rcpp::NumericVector& basal_area,
    const Rcpp::NumericVector& form_class) {
  return KernelKey{{
      double_bits(dbh[index]), double_bits(ht[index]), double_bits(x1[index]),
      double_bits(x2[index]), double_bits(bark_ratio[index]),
      double_bits(upper_ht1[index]), double_bits(upper_d1[index]),
      double_bits(upper_ht2[index]), double_bits(upper_d2[index]),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(upper_bark[index])),
      double_bits(site_index[index]), double_bits(basal_area[index]),
      double_bits(form_class[index]), 0}};
}

using BiomassKey = std::array<std::uint64_t, 21>;

struct BiomassKeyHash {
  std::size_t operator()(const BiomassKey& key) const {
    std::size_t hash = 0;
    for (std::uint64_t value : key) {
      hash ^= static_cast<std::size_t>(value) +
          static_cast<std::size_t>(UINT64_C(0x9e3779b97f4a7c15)) +
          (hash << 6) + (hash >> 2);
    }
    return hash;
  }
};

BiomassKey biomass_key_at(
    R_xlen_t index, const Rcpp::NumericVector& dbh,
    const Rcpp::NumericVector& ht, const Rcpp::IntegerVector& spcd,
    const Rcpp::IntegerVector& division, const Rcpp::IntegerVector& region,
    const Rcpp::IntegerVector& forest,
    const Rcpp::IntegerVector& decay_class, const Rcpp::NumericVector& cull,
    const Rcpp::NumericVector& primary_top,
    const Rcpp::NumericVector& secondary_top,
    const Rcpp::NumericVector& stump,
    const Rcpp::NumericVector& max_log_length,
    const Rcpp::NumericVector& min_log_length,
    const Rcpp::NumericVector& minimum_top_length,
    const Rcpp::NumericVector& merchantable_length,
    const Rcpp::NumericVector& trim,
    const Rcpp::IntegerVector& even_or_odd,
    const Rcpp::IntegerVector& option,
    const Rcpp::IntegerVector& corrected_scribner,
    const Rcpp::IntegerVector& ctype) {
  return BiomassKey{{
      double_bits(dbh[index]), double_bits(ht[index]),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(spcd[index])),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(division[index])),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(region[index])),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(forest[index])),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(decay_class[index])),
      double_bits(cull[index]), double_bits(primary_top[index]),
      double_bits(secondary_top[index]), double_bits(stump[index]),
      double_bits(max_log_length[index]), double_bits(min_log_length[index]),
      double_bits(minimum_top_length[index]),
      double_bits(merchantable_length[index]), double_bits(trim[index]),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(even_or_odd[index])),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(option[index])),
      static_cast<std::uint64_t>(
          static_cast<std::uint32_t>(corrected_scribner[index])),
      static_cast<std::uint64_t>(static_cast<std::uint32_t>(ctype[index])), 0}};
}

struct KernelWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  const RcppParallel::RVector<double> bark_ratio;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::Kernel kernel;
  int operation;

  KernelWorker(const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
               const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
               const Rcpp::NumericVector& bark_ratio_,
               Rcpp::NumericVector& output_,
               Rcpp::IntegerVector& status_, const treevolume::Kernel& kernel_,
               int operation_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), bark_ratio(bark_ratio_),
        output(output_), status(status_), kernel(kernel_), operation(operation_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; ++i) {
      status[i] = 0;
      if (operation == 1) {
        output[i] = kernel.dib(dbh[i], ht[i], x1[i], x2[i], bark_ratio[i]);
      } else if (operation == 2) {
        output[i] = kernel.height_at_dib(
            dbh[i], ht[i], x1[i], x2[i], bark_ratio[i]);
      } else if (operation == 3 || operation == 8) {
        output[i] = kernel.volume(
            dbh[i], ht[i], x1[i], x2[i], bark_ratio[i]);
        if (operation == 8 && std::isfinite(output[i])) {
          output[i] /= bark_ratio[i] * bark_ratio[i];
        }
      } else if (operation == 4) {
        output[i] = kernel.dob(dbh[i], ht[i], x1[i], x2[i], bark_ratio[i]);
      } else if (operation == 6 || operation == 7) {
        const double target = operation == 7 ? x1[i] * bark_ratio[i] : x1[i];
        const double analytic_value = kernel.height_at_dib(
            dbh[i], ht[i], target, 0.0, bark_ratio[i]);
        const EvaluationResult analytic{
            analytic_value, std::isfinite(analytic_value) ? 0 : 54};
        const EvaluationResult discovered = discover_inverse(
            ht[i], x2[i], target, [this, i](double height) {
              const double value = kernel.dib(
                  dbh[i], ht[i], height, 0.0, bark_ratio[i]);
              return std::isfinite(value) ? EvaluationResult{value, 0}
                                          : EvaluationResult{};
            });
        const EvaluationResult result = combine_analytic_inverse(
            analytic, discovered);
        output[i] = result.value;
        status[i] = result.status;
      } else {
        output[i] = std::numeric_limits<double>::quiet_NaN();
        status[i] = 54;
      }
    }
  }
};

struct PublishedTaperWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  const RcppParallel::RVector<double> bark_ratio;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::published_tapers::Form form;
  std::vector<double> coefficients;
  int operation;

  PublishedTaperWorker(
      const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
      const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
      const Rcpp::NumericVector& bark_ratio_, Rcpp::NumericVector& output_,
      Rcpp::IntegerVector& status_, treevolume::published_tapers::Form form_,
      const Rcpp::NumericVector& coefficients_, int operation_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), bark_ratio(bark_ratio_),
        output(output_), status(status_), form(form_),
        coefficients(coefficients_.begin(), coefficients_.end()),
        operation(operation_) {}

  EvaluationResult diameter(std::size_t index, double height) const {
    const double value = treevolume::published_tapers::inside_diameter(
        form, coefficients.data(), coefficients.size(), dbh[index], ht[index],
        height);
    return std::isfinite(value) ? EvaluationResult{value, 0}
                                : EvaluationResult{};
  }

  EvaluationResult inside_volume(std::size_t index) const {
    if (form == treevolume::published_tapers::Form::max_burkhart) {
      const double value = treevolume::published_tapers::max_burkhart_volume(
          coefficients.data(), dbh[index], ht[index], x1[index], x2[index]);
      return std::isfinite(value) ? EvaluationResult{value, 0}
                                  : EvaluationResult{};
    }
    return gauss_legendre_volume(
        x1[index], x2[index], [this, index](double height) {
          return diameter(index, height);
        }, 0.3048, 40000.0);
  }

  void operator()(std::size_t begin, std::size_t end) {
    constexpr double metric_grid_step = 0.0254 / 16.0;
    for (std::size_t index = begin; index < end; ++index) {
      EvaluationResult result;
      if (operation == 1 || operation == 4) {
        result = diameter(index, x1[index]);
        if (operation == 4 && result.status == 0) {
          if (bark_ratio[index] > 0.0 && bark_ratio[index] <= 1.0) {
            result.value /= bark_ratio[index];
          } else {
            result = EvaluationResult{
                std::numeric_limits<double>::quiet_NaN(), 53};
          }
        }
      } else if (operation == 2) {
        if (form == treevolume::published_tapers::Form::max_burkhart) {
          const double value = treevolume::published_tapers::max_burkhart_height(
              coefficients.data(), dbh[index], ht[index], x1[index], 0.0);
          result = std::isfinite(value) ? EvaluationResult{value, 0}
                                        : EvaluationResult{};
        }
      } else if (operation == 3 || operation == 8) {
        result = inside_volume(index);
        if (operation == 8 && result.status == 0) {
          if (bark_ratio[index] > 0.0 && bark_ratio[index] <= 1.0) {
            result.value /= bark_ratio[index] * bark_ratio[index];
          } else {
            result = EvaluationResult{
                std::numeric_limits<double>::quiet_NaN(), 53};
          }
        }
      } else if (operation == 6 || operation == 7) {
        double target = x1[index];
        if (operation == 7) {
          if (bark_ratio[index] > 0.0 && bark_ratio[index] <= 1.0) {
            target *= bark_ratio[index];
          } else {
            output[index] = std::numeric_limits<double>::quiet_NaN();
            status[index] = 53;
            continue;
          }
        }
        const EvaluationResult discovered = discover_inverse(
            ht[index], x2[index], target, [this, index](double height) {
              return diameter(index, height);
            }, metric_grid_step);
        if (form == treevolume::published_tapers::Form::max_burkhart) {
          const double value = treevolume::published_tapers::max_burkhart_height(
              coefficients.data(), dbh[index], ht[index], target, x2[index]);
          result = std::isfinite(value)
              ? combine_analytic_inverse(
                    EvaluationResult{value, 0}, discovered)
              : discovered;
        } else {
          result = discovered;
        }
      }
      output[index] = result.value;
      status[index] = result.status;
    }
  }
};

struct FlewellingWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  const RcppParallel::RVector<double> bark_ratio;
  const RcppParallel::RVector<double> upper_ht1;
  const RcppParallel::RVector<double> upper_d1;
  const RcppParallel::RVector<double> upper_ht2;
  const RcppParallel::RVector<double> upper_d2;
  const RcppParallel::RVector<int> upper_bark;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::flewelling::Equation equation;
  int operation;
  bool nvel_compat;

  FlewellingWorker(
      const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
      const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
      const Rcpp::NumericVector& bark_ratio_,
      const Rcpp::NumericVector& upper_ht1_,
      const Rcpp::NumericVector& upper_d1_,
      const Rcpp::NumericVector& upper_ht2_,
      const Rcpp::NumericVector& upper_d2_,
      const Rcpp::IntegerVector& upper_bark_,
      Rcpp::NumericVector& output_, Rcpp::IntegerVector& status_,
      const treevolume::flewelling::Equation& equation_, int operation_,
      bool nvel_compat_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), bark_ratio(bark_ratio_),
        upper_ht1(upper_ht1_), upper_d1(upper_d1_), upper_ht2(upper_ht2_),
        upper_d2(upper_d2_), upper_bark(upper_bark_), output(output_),
        status(status_), equation(equation_), operation(operation_),
        nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    bool profile_cached = false;
    double cached_dbh = 0.0;
    double cached_ht = 0.0;
    double cached_bark_ratio = 0.0;
    double cached_upper_ht1 = 0.0;
    double cached_upper_d1 = 0.0;
    double cached_upper_ht2 = 0.0;
    double cached_upper_d2 = 0.0;
    int cached_upper_bark = 0;
    treevolume::flewelling::Profile profile;
    treevolume::flewelling::Result initialized;
    for (std::size_t index = begin; index < end; ++index) {
      treevolume::flewelling::Auxiliary auxiliary;
      auxiliary.bark_ratio = bark_ratio[index];
      auxiliary.upper_ht1 = upper_ht1[index];
      auxiliary.upper_d1 = upper_d1[index];
      auxiliary.upper_ht2 = upper_ht2[index];
      auxiliary.upper_d2 = upper_d2[index];
      auxiliary.upper_bark = upper_bark[index];
      treevolume::flewelling::Result result;
      if (operation == 1 || operation == 4) {
        const bool same_profile = profile_cached &&
            same_double(cached_dbh, dbh[index]) &&
            same_double(cached_ht, ht[index]) &&
            same_double(cached_bark_ratio, bark_ratio[index]) &&
            same_double(cached_upper_ht1, upper_ht1[index]) &&
            same_double(cached_upper_d1, upper_d1[index]) &&
            same_double(cached_upper_ht2, upper_ht2[index]) &&
            same_double(cached_upper_d2, upper_d2[index]) &&
            cached_upper_bark == upper_bark[index];
        if (!same_profile) {
          initialized = treevolume::flewelling::initialize_profile(
              equation, dbh[index], ht[index], auxiliary, &profile);
          profile_cached = true;
          cached_dbh = dbh[index];
          cached_ht = ht[index];
          cached_bark_ratio = bark_ratio[index];
          cached_upper_ht1 = upper_ht1[index];
          cached_upper_d1 = upper_d1[index];
          cached_upper_ht2 = upper_ht2[index];
          cached_upper_d2 = upper_d2[index];
          cached_upper_bark = upper_bark[index];
        }
        if (initialized.status != 0) {
          result = initialized;
        } else {
          const double value = operation == 4
              ? treevolume::flewelling::outside_diameter(profile, x1[index])
              : treevolume::flewelling::inside_diameter(profile, x1[index]);
          result = std::isfinite(value)
              ? treevolume::flewelling::Result{value, 0}
              : treevolume::flewelling::Result{
                    std::numeric_limits<double>::quiet_NaN(), 54};
        }
      } else if (operation == 3) {
        result = treevolume::flewelling::smalian_volume(
            equation, dbh[index], ht[index], x1[index], x2[index], auxiliary);
      } else if (operation == 8) {
        treevolume::flewelling::Profile volume_profile;
        result = treevolume::flewelling::initialize_profile(
            equation, dbh[index], ht[index], auxiliary, &volume_profile);
        if (result.status == 0) {
          const EvaluationResult integrated = gauss_legendre_volume(
              x1[index], x2[index], [&volume_profile](double height) {
                const double value = treevolume::flewelling::outside_diameter(
                    volume_profile, height);
                return std::isfinite(value) ? EvaluationResult{value, 0}
                                            : EvaluationResult{};
              });
          result.value = integrated.value;
          result.status = integrated.status;
        }
      } else if (operation == 6 || operation == 7) {
        treevolume::flewelling::Profile inverse_profile;
        result = treevolume::flewelling::initialize_profile(
            equation, dbh[index], ht[index], auxiliary, &inverse_profile);
        if (result.status == 0) {
          const bool outside = operation == 7;
          const bool source_affected =
              inverse_profile.equation.condition_upper ||
              (inverse_profile.equation.jsp >= 22 &&
               inverse_profile.equation.jsp <= 29);
          if (nvel_compat && !outside && source_affected) {
            result = treevolume::flewelling::source_height_at_diameter(
                inverse_profile, x1[index]);
            if (result.status == 0) {
              result.value = std::max(result.value, x2[index]);
            }
          } else {
            const EvaluationResult discovered = discover_inverse(
                ht[index], x2[index], x1[index],
                [&inverse_profile, outside](double height) {
                  const double value = outside
                      ? treevolume::flewelling::outside_diameter(
                            inverse_profile, height)
                      : treevolume::flewelling::inside_diameter(
                            inverse_profile, height);
                  return std::isfinite(value) ? EvaluationResult{value, 0}
                                              : EvaluationResult{};
                });
            result.value = discovered.value;
            result.status = discovered.status;
          }
        }
      } else {
        result.status = 54;
      }
      output[index] = result.value;
      status[index] = result.status;
    }
  }
};

struct ClarkWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  const RcppParallel::RVector<double> bark_ratio;
  const RcppParallel::RVector<double> upper_ht1;
  const RcppParallel::RVector<double> site_index;
  const RcppParallel::RVector<double> basal_area;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::clark::Equation equation;
  int operation;
  bool nvel_compat;

  ClarkWorker(
      const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
      const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
      const Rcpp::NumericVector& bark_ratio_,
      const Rcpp::NumericVector& upper_ht1_,
      const Rcpp::NumericVector& site_index_,
      const Rcpp::NumericVector& basal_area_, Rcpp::NumericVector& output_,
      Rcpp::IntegerVector& status_,
      const treevolume::clark::Equation& equation_, int operation_,
      bool nvel_compat_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), bark_ratio(bark_ratio_),
        upper_ht1(upper_ht1_),
        site_index(site_index_), basal_area(basal_area_), output(output_),
        status(status_), equation(equation_), operation(operation_),
        nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    bool profile_cached = false;
    double cached_dbh = 0.0;
    double cached_ht = 0.0;
    double cached_upper_ht1 = 0.0;
    double cached_site_index = 0.0;
    double cached_basal_area = 0.0;
    treevolume::clark::Profile profile;
    treevolume::clark::Result initialized;
    for (std::size_t index = begin; index < end; ++index) {
      treevolume::clark::Auxiliary auxiliary;
      auxiliary.upper_ht1 = upper_ht1[index];
      auxiliary.site_index = site_index[index];
      auxiliary.basal_area = basal_area[index];
      treevolume::clark::Result result;
      if (operation == 1 || operation == 4) {
        const bool same_profile = profile_cached &&
            same_double(cached_dbh, dbh[index]) &&
            same_double(cached_ht, ht[index]) &&
            same_double(cached_upper_ht1, upper_ht1[index]) &&
            same_double(cached_site_index, site_index[index]) &&
            same_double(cached_basal_area, basal_area[index]);
        if (!same_profile) {
          initialized = treevolume::clark::initialize_profile(
              equation, dbh[index], ht[index], auxiliary, &profile);
          profile_cached = true;
          cached_dbh = dbh[index];
          cached_ht = ht[index];
          cached_upper_ht1 = upper_ht1[index];
          cached_site_index = site_index[index];
          cached_basal_area = basal_area[index];
        }
        if (initialized.status != 0) {
          result = initialized;
        } else {
          const double value = treevolume::clark::inside_diameter(
              equation, profile, x1[index]);
          const double adjusted = operation == 4 &&
              bark_ratio[index] > 0.0 && bark_ratio[index] <= 1.0
              ? value / bark_ratio[index] : value;
          result = operation == 4 &&
              (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0)
              ? treevolume::clark::Result{
                    std::numeric_limits<double>::quiet_NaN(), 53}
              : std::isfinite(adjusted)
              ? treevolume::clark::Result{adjusted, 0}
              : treevolume::clark::Result{
                    std::numeric_limits<double>::quiet_NaN(), 54};
        }
      } else if (operation == 2) {
        result = treevolume::clark::height_at_diameter(
            equation, dbh[index], ht[index], x1[index], auxiliary);
      } else if (operation == 3) {
        result = treevolume::clark::volume(
            equation, dbh[index], ht[index], x1[index], x2[index], auxiliary);
      } else if (operation == 8) {
        result = treevolume::clark::volume(
            equation, dbh[index], ht[index], x1[index], x2[index], auxiliary);
        if (result.status == 0) {
          if (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0) {
            result = treevolume::clark::Result{
                std::numeric_limits<double>::quiet_NaN(), 53};
          } else {
            result.value /= bark_ratio[index] * bark_ratio[index];
          }
        }
      } else if (operation == 9) {
        result = treevolume::clark::driver_total_volume(
            equation, dbh[index], ht[index], x1[index], x2[index], auxiliary);
      } else if (operation == 10) {
        result = treevolume::clark::driver_stump_volume(
            equation, dbh[index], ht[index], x2[index], auxiliary);
      } else if (operation == 11) {
        treevolume::clark::Profile inside_profile;
        treevolume::clark::Profile outside_profile;
        result = treevolume::clark::initialize_profile(
            equation, dbh[index], ht[index], auxiliary, &inside_profile,
            &outside_profile);
        if (result.status == 0) {
          const treevolume::clark::Profile& source_profile =
              equation.region8_on_region9 ? outside_profile : inside_profile;
          result.value = treevolume::clark::region9_height(
              source_profile, x1[index]);
          if (!std::isfinite(result.value)) result.status = 54;
        }
      } else if (operation == 6 || operation == 7) {
        const bool outside = operation == 7;
        if (outside &&
            (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0)) {
          result = treevolume::clark::Result{
              std::numeric_limits<double>::quiet_NaN(), 53};
        } else {
          const double target = outside
              ? x1[index] * bark_ratio[index] : x1[index];
          const bool source_adds_roots = nvel_compat && !outside &&
              treevolume::clark::source_old_region8_adds_roots(
                  equation, dbh[index], ht[index], target, auxiliary);
          if (source_adds_roots) {
            result = treevolume::clark::source_height_at_diameter(
                equation, dbh[index], ht[index], target, auxiliary);
            output[index] = result.value;
            status[index] = result.status;
            continue;
          }
          const treevolume::clark::Result raw =
              treevolume::clark::height_at_diameter(
                  equation, dbh[index], ht[index], target, auxiliary);
          treevolume::clark::Profile inverse_profile;
          const treevolume::clark::Result initialized =
              treevolume::clark::initialize_profile(
                  equation, dbh[index], ht[index], auxiliary, &inverse_profile);
          EvaluationResult discovered{
              std::numeric_limits<double>::quiet_NaN(), initialized.status};
          if (initialized.status == 0) {
            discovered = discover_inverse(
                ht[index], x2[index], target,
                [this, index, &inverse_profile](double height) {
                  const double value = treevolume::clark::inside_diameter(
                      equation, inverse_profile, height);
                  return std::isfinite(value) ? EvaluationResult{value, 0}
                                              : EvaluationResult{};
                });
          }
          const EvaluationResult combined = combine_analytic_inverse(
              EvaluationResult{raw.value, raw.status}, discovered);
          result.value = combined.value;
          result.status = combined.status;
        }
      } else {
        result.status = 54;
      }
      output[index] = result.value;
      status[index] = result.status;
    }
  }
};

struct RegionalWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  const RcppParallel::RVector<double> bark_ratio;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::r10r4::Equation equation;
  int operation;
  bool nvel_compat;

  RegionalWorker(const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
                 const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
                 const Rcpp::NumericVector& bark_ratio_,
                 Rcpp::NumericVector& output_, Rcpp::IntegerVector& status_,
                 const treevolume::r10r4::Equation& equation_, int operation_,
                 bool nvel_compat_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), bark_ratio(bark_ratio_),
        output(output_),
        status(status_), equation(equation_), operation(operation_),
        nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t index = begin; index < end; ++index) {
      treevolume::r10r4::Result result;
      if (operation == 1 || operation == 4) {
        result = treevolume::r10r4::diameter(
            equation, dbh[index], ht[index], x1[index]);
        if (operation == 4 && result.status == 0) {
          if (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0) {
            result = treevolume::r10r4::failure(53);
          } else {
            result.value /= bark_ratio[index];
          }
        }
      } else if (operation == 2) {
        result = treevolume::r10r4::height_at_diameter(
            equation, dbh[index], ht[index], x1[index]);
      } else if (operation == 3) {
        result = treevolume::r10r4::volume(
            equation, dbh[index], ht[index], x1[index], x2[index]);
      } else if (operation == 8) {
        result = treevolume::r10r4::volume(
            equation, dbh[index], ht[index], x1[index], x2[index]);
        if (result.status == 0) {
          if (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0) {
            result = treevolume::r10r4::failure(53);
          } else {
            result.value /= bark_ratio[index] * bark_ratio[index];
          }
        }
      } else if (operation == 9) {
        result = treevolume::r10r4::driver_total_volume(
            equation, dbh[index], ht[index]);
      } else if (operation == 6 || operation == 7) {
        const bool outside = operation == 7;
        if (outside &&
            (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0)) {
          result = treevolume::r10r4::failure(53);
        } else {
          const double target = outside
              ? x1[index] * bark_ratio[index] : x1[index];
          const bool source_r10_profile =
              equation.id.compare(3, 3, "BRU") != 0;
          if (nvel_compat && !outside &&
              equation.family == treevolume::r10r4::Family::r10 &&
              source_r10_profile &&
              (equation.species == 202 || equation.species == 260)) {
            result = treevolume::r10r4::Result{0.0, 0};
            output[index] = result.value;
            status[index] = result.status;
            continue;
          }
          const bool source_cedar_equality = nvel_compat && !outside &&
              equation.family == treevolume::r10r4::Family::r10 &&
              source_r10_profile &&
              ((equation.species == 42 && dbh[index] == 38.01) ||
               (equation.species == 242 && dbh[index] == 56.01));
          const treevolume::r10r4::Result raw =
              treevolume::r10r4::height_at_diameter(
                  equation, dbh[index], ht[index], target,
                  source_cedar_equality);
          if (source_cedar_equality) {
            result = raw;
            output[index] = result.value;
            status[index] = result.status;
            continue;
          }
          const EvaluationResult discovered = discover_inverse(
              ht[index], x2[index], target, [this, index](double height) {
                const treevolume::r10r4::Result value =
                    treevolume::r10r4::diameter(
                        equation, dbh[index], ht[index], height);
                return EvaluationResult{value.value, value.status};
              });
          const EvaluationResult combined = combine_analytic_inverse(
              EvaluationResult{raw.value, raw.status}, discovered);
          result.value = combined.value;
          result.status = combined.status;
        }
      } else {
        result = treevolume::r10r4::failure(54);
      }
      output[index] = result.value;
      status[index] = result.status;
    }
  }
};

struct SmallTaperWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  const RcppParallel::RVector<double> bark_ratio;
  const RcppParallel::RVector<double> upper_ht1;
  const RcppParallel::RVector<double> upper_d1;
  const RcppParallel::RVector<double> form_class;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::smalltapers::Equation equation;
  int operation;
  bool nvel_compat;

  SmallTaperWorker(
      const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
      const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
      const Rcpp::NumericVector& bark_ratio_,
      const Rcpp::NumericVector& upper_ht1_,
      const Rcpp::NumericVector& upper_d1_,
      const Rcpp::NumericVector& form_class_,
      Rcpp::NumericVector& output_, Rcpp::IntegerVector& status_,
      const treevolume::smalltapers::Equation& equation_, int operation_,
      bool nvel_compat_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), bark_ratio(bark_ratio_),
        upper_ht1(upper_ht1_),
        upper_d1(upper_d1_), form_class(form_class_), output(output_),
        status(status_), equation(equation_), operation(operation_),
        nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    bool inverse_cached = false;
    double cached_dbh = 0.0;
    double cached_ht = 0.0;
    double cached_upper_ht1 = 0.0;
    double cached_upper_d1 = 0.0;
    double cached_form_class = 0.0;
    std::vector<double> cached_heights;
    std::vector<double> cached_values;
    treevolume::smalltapers::Result cached_sample;
    for (std::size_t index = begin; index < end; ++index) {
      treevolume::smalltapers::Auxiliary auxiliary;
      auxiliary.upper_ht1 = upper_ht1[index];
      auxiliary.upper_d1 = upper_d1[index];
      auxiliary.form_class = form_class[index];
      treevolume::smalltapers::Result result;
      if (operation == 1 || operation == 4) {
        result = treevolume::smalltapers::diameter(
            equation, dbh[index], ht[index], x1[index], auxiliary);
        if (operation == 4 && result.status == 0) {
          if (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0) {
            result = treevolume::smalltapers::Result{NAN, 53};
          } else {
            result.value /= bark_ratio[index];
          }
        }
      } else if (operation == 2) {
        result = treevolume::smalltapers::nvel_height(
            equation, dbh[index], ht[index], x1[index], auxiliary);
      } else if (operation == 5) {
        const bool same_profile = inverse_cached &&
            same_double(cached_dbh, dbh[index]) &&
            same_double(cached_ht, ht[index]) &&
            same_double(cached_upper_ht1, upper_ht1[index]) &&
            same_double(cached_upper_d1, upper_d1[index]) &&
            same_double(cached_form_class, form_class[index]);
        if (!same_profile) {
          cached_sample = treevolume::smalltapers::sample_profile(
              equation, dbh[index], ht[index], auxiliary,
              &cached_heights, &cached_values);
          inverse_cached = true;
          cached_dbh = dbh[index];
          cached_ht = ht[index];
          cached_upper_ht1 = upper_ht1[index];
          cached_upper_d1 = upper_d1[index];
          cached_form_class = form_class[index];
        }
        result = cached_sample.status == 0
            ? treevolume::smalltapers::inverse_from_samples(
                  equation, dbh[index], ht[index], x1[index], auxiliary,
                  cached_heights, cached_values)
            : cached_sample;
      } else if (operation == 3) {
        result = treevolume::smalltapers::volume(
            equation, dbh[index], ht[index], x1[index], x2[index], auxiliary);
      } else if (operation == 8) {
        result = treevolume::smalltapers::volume(
            equation, dbh[index], ht[index], x1[index], x2[index], auxiliary);
        if (result.status == 0) {
          if (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0) {
            result = treevolume::smalltapers::Result{NAN, 53};
          } else {
            result.value /= bark_ratio[index] * bark_ratio[index];
          }
        }
      } else if (operation == 6 || operation == 7) {
        const bool outside = operation == 7;
        if (outside &&
            (!(bark_ratio[index] > 0.0) || bark_ratio[index] > 1.0)) {
          result = treevolume::smalltapers::Result{NAN, 53};
        } else {
          const double target = outside
              ? x1[index] * bark_ratio[index] : x1[index];
          const treevolume::smalltapers::Result raw =
              treevolume::smalltapers::nvel_height(
                  equation, dbh[index], ht[index], target, auxiliary);
          const bool source_value_valid = raw.status == 0 &&
              std::isfinite(raw.value);
          const bool source_r12_inverse = source_value_valid && !outside &&
              equation.family == treevolume::smalltapers::Family::r12;
          const bool source_rounded_inverse = source_value_valid &&
              raw.value > x2[index] && raw.value <= ht[index];
          if (nvel_compat &&
              (source_r12_inverse || source_rounded_inverse)) {
            result = raw;
            output[index] = result.value;
            status[index] = result.status;
            continue;
          }
          std::vector<double> heights;
          std::vector<double> values;
          const treevolume::smalltapers::Result sampled =
              treevolume::smalltapers::sample_profile(
                  equation, dbh[index], ht[index], auxiliary,
                  &heights, &values);
          treevolume::smalltapers::Result discovered{NAN, sampled.status};
          if (sampled.status == 0) {
            discovered = treevolume::smalltapers::inverse_from_samples(
                equation, dbh[index], ht[index], target, auxiliary,
                heights, values);
          }
          result = discovered;
        }
      } else {
        result.status = 54;
      }
      output[index] = result.value;
      status[index] = result.status;
    }
  }
};

struct NsvbWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<double> x1;
  const RcppParallel::RVector<double> x2;
  RcppParallel::RVector<double> output;
  RcppParallel::RVector<int> status;
  treevolume::nsvb::Equation equation;
  int operation;
  bool nvel_compat;

  NsvbWorker(const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
             const Rcpp::NumericVector& x1_, const Rcpp::NumericVector& x2_,
             Rcpp::NumericVector& output_, Rcpp::IntegerVector& status_,
             const treevolume::nsvb::Equation& equation_, int operation_,
             bool nvel_compat_)
      : dbh(dbh_), ht(ht_), x1(x1_), x2(x2_), output(output_),
        status(status_), equation(equation_), operation(operation_),
        nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t index = begin; index < end; ++index) {
      treevolume::nsvb::Result result;
      if (operation == 1 || operation == 4) {
        if (operation == 4 && nvel_compat) {
          result = treevolume::nsvb::Result{0.0, 0};
        } else {
          result = treevolume::nsvb::profile_value(
              equation, dbh[index], ht[index], x1[index], operation == 4);
        }
      } else if (operation == 2) {
        result = treevolume::nsvb::inverse_value(
            equation, dbh[index], ht[index], x1[index]);
      } else if (operation == 3) {
        result = treevolume::nsvb::volume_value(
            equation, dbh[index], ht[index], x1[index], x2[index]);
      } else if (operation == 8) {
        const EvaluationResult integrated = gauss_legendre_volume(
            x1[index], x2[index], [this, index](double height) {
              const treevolume::nsvb::Result value =
                  treevolume::nsvb::profile_value(
                      equation, dbh[index], ht[index], height, true);
              return EvaluationResult{value.value, value.status};
            });
        result.value = integrated.value;
        result.status = integrated.status;
      } else if (operation == 6 || operation == 7) {
        const bool outside = operation == 7;
        const EvaluationResult discovered = discover_inverse(
            ht[index], x2[index], x1[index],
            [this, index, outside](double height) {
              const treevolume::nsvb::Result value =
                  treevolume::nsvb::profile_value(
                      equation, dbh[index], ht[index], height, outside);
              return EvaluationResult{value.value, value.status};
            });
        if (outside) {
          result.value = discovered.value;
          result.status = discovered.status;
        } else {
          const treevolume::nsvb::Result raw =
              treevolume::nsvb::inverse_value(
                  equation, dbh[index], ht[index], x1[index]);
          const EvaluationResult combined = combine_analytic_inverse(
              EvaluationResult{raw.value, raw.status}, discovered);
          result.value = combined.value;
          result.status = combined.status;
        }
      } else {
        result.status = 54;
      }
      output[index] = result.value;
      status[index] = result.status;
    }
  }
};

struct NsvbBiomassWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> dbh;
  const RcppParallel::RVector<double> ht;
  const RcppParallel::RVector<int> spcd;
  const RcppParallel::RVector<int> division;
  const RcppParallel::RVector<int> region;
  const RcppParallel::RVector<int> forest;
  const RcppParallel::RVector<int> decay_class;
  const RcppParallel::RVector<double> cull;
  const RcppParallel::RVector<double> primary_top;
  const RcppParallel::RVector<double> secondary_top;
  const RcppParallel::RVector<double> stump;
  const RcppParallel::RVector<double> max_log_length;
  const RcppParallel::RVector<double> min_log_length;
  const RcppParallel::RVector<double> minimum_top_length;
  const RcppParallel::RVector<double> merchantable_length;
  const RcppParallel::RVector<double> trim;
  const RcppParallel::RVector<int> even_or_odd;
  const RcppParallel::RVector<int> option;
  const RcppParallel::RVector<int> corrected_scribner;
  const RcppParallel::RVector<int> ctype;
  RcppParallel::RMatrix<double> output;
  RcppParallel::RMatrix<double> volume;
  RcppParallel::RVector<int> n_logs_primary;
  RcppParallel::RVector<int> n_logs_secondary;
  RcppParallel::RVector<int> status;
  const std::vector<int>* rows;

  NsvbBiomassWorker(
      const Rcpp::NumericVector& dbh_, const Rcpp::NumericVector& ht_,
      const Rcpp::IntegerVector& spcd_, const Rcpp::IntegerVector& division_,
      const Rcpp::IntegerVector& region_, const Rcpp::IntegerVector& forest_,
      const Rcpp::IntegerVector& decay_class_, const Rcpp::NumericVector& cull_,
      const Rcpp::NumericVector& primary_top_,
      const Rcpp::NumericVector& secondary_top_,
      const Rcpp::NumericVector& stump_,
      const Rcpp::NumericVector& max_log_length_,
      const Rcpp::NumericVector& min_log_length_,
      const Rcpp::NumericVector& minimum_top_length_,
      const Rcpp::NumericVector& merchantable_length_,
      const Rcpp::NumericVector& trim_, const Rcpp::IntegerVector& even_or_odd_,
      const Rcpp::IntegerVector& option_,
      const Rcpp::IntegerVector& corrected_scribner_,
      const Rcpp::IntegerVector& ctype_,
      Rcpp::NumericMatrix& output_,
      Rcpp::NumericMatrix& volume_, Rcpp::IntegerVector& n_logs_primary_,
      Rcpp::IntegerVector& n_logs_secondary_, Rcpp::IntegerVector& status_,
      const std::vector<int>* rows_)
      : dbh(dbh_), ht(ht_), spcd(spcd_), division(division_), region(region_),
        forest(forest_), decay_class(decay_class_), cull(cull_),
        primary_top(primary_top_), secondary_top(secondary_top_), stump(stump_),
        max_log_length(max_log_length_), min_log_length(min_log_length_),
        minimum_top_length(minimum_top_length_),
        merchantable_length(merchantable_length_), trim(trim_),
        even_or_odd(even_or_odd_), option(option_),
        corrected_scribner(corrected_scribner_), ctype(ctype_), output(output_),
        volume(volume_),
        n_logs_primary(n_logs_primary_), n_logs_secondary(n_logs_secondary_),
        status(status_), rows(rows_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t work = begin; work < end; ++work) {
      const std::size_t index = rows == nullptr
          ? work : static_cast<std::size_t>((*rows)[work]);
      treevolume::nsvb::Equation equation;
      equation.species = spcd[index];
      equation.exact_division = division[index];
      equation.division = division[index] >= 1000
          ? 1000 + ((division[index] - 1000) / 10) * 10
          : (division[index] / 10) * 10;
      treevolume::nsvb::BiomassOptions options;
      options.region = region[index];
      options.forest = forest[index];
      options.decay_class = decay_class[index];
      options.cull = cull[index];
      options.primary_top = primary_top[index];
      options.secondary_top = secondary_top[index];
      options.stump = stump[index];
      options.max_log_length = max_log_length[index];
      options.min_log_length = min_log_length[index];
      options.minimum_top_length = minimum_top_length[index];
      options.merchantable_length = merchantable_length[index];
      options.trim = trim[index];
      options.even_or_odd = even_or_odd[index];
      options.option = option[index];
      options.corrected_scribner = corrected_scribner[index];
      options.ctype = ctype[index];
      const treevolume::nsvb::BiomassResult result = treevolume::nsvb::biomass(
          equation, dbh[index], ht[index], spcd[index], options);
      for (int column = 0; column < 30; ++column) {
        output(index, column) = result.value[column];
      }
      for (int column = 0; column < 15; ++column) {
        volume(index, column) = result.volume[column];
      }
      n_logs_primary[index] = result.n_logs_primary;
      n_logs_secondary[index] = result.n_logs_secondary;
      status[index] = result.status;
    }
  }
};

struct NsvbBiomassScatterWorker : public RcppParallel::Worker {
  RcppParallel::RMatrix<double> output;
  RcppParallel::RMatrix<double> volume;
  RcppParallel::RVector<int> n_logs_primary;
  RcppParallel::RVector<int> n_logs_secondary;
  RcppParallel::RVector<int> status;
  const std::vector<int>& representatives;
  const std::vector<int>& map;

  NsvbBiomassScatterWorker(
      Rcpp::NumericMatrix& output_, Rcpp::NumericMatrix& volume_,
      Rcpp::IntegerVector& n_logs_primary_,
      Rcpp::IntegerVector& n_logs_secondary_, Rcpp::IntegerVector& status_,
      const std::vector<int>& representatives_, const std::vector<int>& map_)
      : output(output_), volume(volume_), n_logs_primary(n_logs_primary_),
        n_logs_secondary(n_logs_secondary_), status(status_),
        representatives(representatives_), map(map_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t index = begin; index < end; ++index) {
      const std::size_t source = static_cast<std::size_t>(
          representatives[static_cast<std::size_t>(map[index])]);
      if (source == index) {
        continue;
      }
      for (int column = 0; column < 30; ++column) {
        output(index, column) = output(source, column);
      }
      for (int column = 0; column < 15; ++column) {
        volume(index, column) = volume(source, column);
      }
      n_logs_primary[index] = n_logs_primary[source];
      n_logs_secondary[index] = n_logs_secondary[source];
      status[index] = status[source];
    }
  }
};

struct ProfileGridWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> lower;
  const RcppParallel::RVector<double> upper;
  const RcppParallel::RVector<double> step;
  const RcppParallel::RVector<int> tree_status;
  const std::vector<R_xlen_t>& offsets;
  RcppParallel::RVector<int> tree;
  RcppParallel::RVector<double> height;
  RcppParallel::RVector<int> status;

  ProfileGridWorker(
      const Rcpp::NumericVector& lower_, const Rcpp::NumericVector& upper_,
      const Rcpp::NumericVector& step_, const Rcpp::IntegerVector& tree_status_,
      const std::vector<R_xlen_t>& offsets_, Rcpp::IntegerVector& tree_,
      Rcpp::NumericVector& height_, Rcpp::IntegerVector& status_)
      : lower(lower_), upper(upper_), step(step_), tree_status(tree_status_),
        offsets(offsets_), tree(tree_), height(height_), status(status_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t index = begin; index < end; ++index) {
      const R_xlen_t first = offsets[index];
      const R_xlen_t last = offsets[index + 1];
      const bool valid = tree_status[index] == 0 || tree_status[index] == 52 ||
          tree_status[index] == 102;
      for (R_xlen_t row = first; row < last; ++row) {
        tree[row] = static_cast<int>(index + 1);
        status[row] = tree_status[index];
        if (valid) {
          const R_xlen_t position = row - first;
          height[row] = row + 1 == last
              ? upper[index] : lower[index] + position * step[index];
        } else {
          height[row] = NA_REAL;
        }
      }
    }
  }
};

treevolume::KernelFunction select_operation(const treevolume::Kernel& kernel,
                                            int operation) {
  if (operation == 1) {
    return kernel.dib;
  }
  if (operation == 2 || operation == 6 || operation == 7) {
    return kernel.height_at_dib;
  }
  if (operation == 3) {
    return kernel.volume;
  }
  if (operation == 4) {
    return kernel.dob;
  }
  if (operation == 8) {
    return kernel.volume;
  }
  return nullptr;
}

}  // namespace

// [[Rcpp::export]]
bool tv_cpp_kernel_exists(const std::string& kernel) {
  return treevolume::kernel_exists(kernel);
}

// [[Rcpp::export]]
Rcpp::List tv_cpp_duplicate_map_impl(
    Rcpp::NumericVector dbh, Rcpp::NumericVector ht,
    Rcpp::NumericVector x1, Rcpp::NumericVector x2,
    Rcpp::NumericVector bark_ratio, Rcpp::NumericVector upper_ht1,
    Rcpp::NumericVector upper_d1, Rcpp::NumericVector upper_ht2,
    Rcpp::NumericVector upper_d2, Rcpp::IntegerVector upper_bark,
    Rcpp::NumericVector site_index, Rcpp::NumericVector basal_area,
    Rcpp::NumericVector form_class) {
  const R_xlen_t size = dbh.size();
  if (size < 32) {
    return Rcpp::List::create();
  }
  const R_xlen_t sample_size = std::min<R_xlen_t>(size, 1024);
  std::unordered_map<KernelKey, int, KernelKeyHash> sample;
  sample.reserve(static_cast<std::size_t>(sample_size));
  for (R_xlen_t index = 0; index < sample_size; ++index) {
    sample.emplace(kernel_key_at(
        index, dbh, ht, x1, x2, bark_ratio, upper_ht1, upper_d1, upper_ht2,
        upper_d2, upper_bark, site_index, basal_area, form_class), 0);
  }
  if (sample.size() * 2 > static_cast<std::size_t>(sample_size)) {
    return Rcpp::List::create();
  }

  std::unordered_map<KernelKey, int, KernelKeyHash> dictionary;
  dictionary.reserve(static_cast<std::size_t>(sample.size()) * 2);
  std::vector<int> unique;
  unique.reserve(static_cast<std::size_t>(sample.size()) * 2);
  Rcpp::IntegerVector map(size);
  for (R_xlen_t index = 0; index < size; ++index) {
    const KernelKey key = kernel_key_at(
        index, dbh, ht, x1, x2, bark_ratio, upper_ht1, upper_d1, upper_ht2,
        upper_d2, upper_bark, site_index, basal_area, form_class);
    auto found = dictionary.find(key);
    if (found == dictionary.end()) {
      const int position = static_cast<int>(unique.size());
      dictionary.emplace(key, position);
      unique.push_back(static_cast<int>(index));
      map[index] = position + 1;
    } else {
      map[index] = found->second + 1;
    }
  }
  if (unique.size() * 2 > static_cast<std::size_t>(size)) {
    return Rcpp::List::create();
  }
  Rcpp::IntegerVector unique_rows(unique.size());
  for (std::size_t index = 0; index < unique.size(); ++index) {
    unique_rows[index] = unique[index] + 1;
  }
  return Rcpp::List::create(
      Rcpp::Named("unique") = unique_rows, Rcpp::Named("map") = map);
}

// [[Rcpp::export]]
Rcpp::List tv_cpp_profile_grid_impl(
    Rcpp::NumericVector lower, Rcpp::NumericVector upper,
    Rcpp::NumericVector step, Rcpp::IntegerVector tree_status, int threads) {
  const R_xlen_t size = lower.size();
  if (upper.size() != size || step.size() != size ||
      tree_status.size() != size) {
    Rcpp::stop("compiled profile-grid inputs must have equal lengths");
  }
  if (threads < 1) {
    Rcpp::stop("threads must be at least one");
  }
  std::vector<R_xlen_t> offsets(static_cast<std::size_t>(size) + 1, 0);
  for (R_xlen_t index = 0; index < size; ++index) {
    const bool valid = tree_status[index] == 0 || tree_status[index] == 52 ||
        tree_status[index] == 102;
    R_xlen_t count = 1;
    if (valid) {
      const double raw_count = std::ceil(
          (upper[index] - lower[index]) / step[index]);
      if (!std::isfinite(raw_count) || raw_count < 0.0 ||
          raw_count > static_cast<double>(R_XLEN_T_MAX - offsets[index] - 1)) {
        Rcpp::stop("compiled profile grid is too large");
      }
      count = static_cast<R_xlen_t>(raw_count) + 1;
    }
    offsets[index + 1] = offsets[index] + count;
  }
  const R_xlen_t total = offsets[static_cast<std::size_t>(size)];
  Rcpp::IntegerVector tree(total);
  Rcpp::NumericVector height(total);
  Rcpp::IntegerVector status(total);
  ProfileGridWorker worker(
      lower, upper, step, tree_status, offsets, tree, height, status);
  RcppParallel::parallelFor(
      0, static_cast<std::size_t>(size), worker, 1, threads);
  return Rcpp::List::create(
      Rcpp::Named("tree") = tree, Rcpp::Named("native_h") = height,
      Rcpp::Named("status") = status);
}

// [[Rcpp::export]]
Rcpp::List tv_cpp_kernel_eval_impl(const std::string& kernel_name, int operation,
                                   Rcpp::NumericVector dbh,
                                   Rcpp::NumericVector ht,
                                   Rcpp::NumericVector x1,
                                   Rcpp::NumericVector x2,
                                   Rcpp::NumericVector bark_ratio,
                                   Rcpp::NumericVector upper_ht1,
                                   Rcpp::NumericVector upper_d1,
                                   Rcpp::NumericVector upper_ht2,
                                   Rcpp::NumericVector upper_d2,
                                   Rcpp::IntegerVector upper_bark,
                                   Rcpp::NumericVector site_index,
                                   Rcpp::NumericVector basal_area,
                                   Rcpp::NumericVector form_class,
                                   Rcpp::NumericVector coefficients,
                                   bool nvel_compat,
                                   int threads) {
  const R_xlen_t size = dbh.size();
  if (ht.size() != size || x1.size() != size || x2.size() != size ||
      bark_ratio.size() != size || upper_ht1.size() != size ||
      upper_d1.size() != size || upper_ht2.size() != size ||
      upper_d2.size() != size || upper_bark.size() != size ||
      site_index.size() != size || basal_area.size() != size ||
      form_class.size() != size) {
    Rcpp::stop("compiled kernel inputs must have equal lengths");
  }
  if (threads < 1) {
    Rcpp::stop("threads must be at least one");
  }
  Rcpp::NumericVector output(size);
  Rcpp::IntegerVector status(size);
  static const std::string flewelling_prefix = "flewelling:";
  static const std::string clark_prefix = "clark:";
  static const std::string nsvb_prefix = "nsvb:";
  static const std::string small_prefix = "smalltapers:";
  static const std::string published_prefix = "published:";
  if (kernel_name.compare(0, flewelling_prefix.size(), flewelling_prefix) == 0) {
    treevolume::flewelling::Equation equation;
    if (!treevolume::flewelling::parse_equation(
            kernel_name.substr(flewelling_prefix.size()), &equation)) {
      Rcpp::stop("unknown compiled kernel");
    }
    FlewellingWorker worker(
        dbh, ht, x1, x2, bark_ratio, upper_ht1, upper_d1, upper_ht2,
        upper_d2, upper_bark, output, status, equation, operation,
        nvel_compat);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  } else if (kernel_name.compare(0, clark_prefix.size(), clark_prefix) == 0) {
    treevolume::clark::Equation equation;
    if (!treevolume::clark::parse_equation(
            kernel_name.substr(clark_prefix.size()), &equation)) {
      Rcpp::stop("unknown compiled kernel");
    }
    ClarkWorker worker(
        dbh, ht, x1, x2, bark_ratio, upper_ht1, site_index, basal_area,
        output, status,
        equation, operation, nvel_compat);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  } else if (kernel_name.compare(0, 4, "r10:") == 0 ||
             kernel_name.compare(0, 6, "r4mat:") == 0) {
    treevolume::r10r4::Equation equation;
    if (!treevolume::r10r4::parse_equation(kernel_name, &equation)) {
      Rcpp::stop("unknown compiled kernel");
    }
    RegionalWorker worker(
        dbh, ht, x1, x2, bark_ratio, output, status, equation, operation,
        nvel_compat);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  } else if (kernel_name.compare(0, small_prefix.size(), small_prefix) == 0) {
    treevolume::smalltapers::Equation equation;
    if (!treevolume::smalltapers::parse_equation(
            kernel_name.substr(small_prefix.size()), &equation)) {
      Rcpp::stop("unknown compiled kernel");
    }
    SmallTaperWorker worker(
        dbh, ht, x1, x2, bark_ratio, upper_ht1, upper_d1, form_class, output,
        status, equation, operation, nvel_compat);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  } else if (kernel_name.compare(0, nsvb_prefix.size(), nsvb_prefix) == 0) {
    treevolume::nsvb::Equation equation;
    if (!treevolume::nsvb::parse_equation(
            kernel_name.substr(nsvb_prefix.size()), &equation)) {
      Rcpp::stop("unknown compiled kernel");
    }
    NsvbWorker worker(
        dbh, ht, x1, x2, output, status, equation, operation, nvel_compat);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  } else if (kernel_name.compare(
                 0, published_prefix.size(), published_prefix) == 0) {
    const treevolume::published_tapers::Form form =
        treevolume::published_tapers::parse_form(
            kernel_name.substr(published_prefix.size()));
    if (!treevolume::published_tapers::finite_coefficients(
            form, coefficients.begin(), coefficients.size())) {
      Rcpp::stop("published taper coefficients are invalid");
    }
    PublishedTaperWorker worker(
        dbh, ht, x1, x2, bark_ratio, output, status, form, coefficients,
        operation);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  } else {
    const treevolume::Kernel* kernel = treevolume::find_kernel(kernel_name);
    if (kernel == nullptr) {
      Rcpp::stop("unknown compiled kernel");
    }
    treevolume::KernelFunction function = select_operation(*kernel, operation);
    if (function == nullptr) {
      Rcpp::stop("compiled kernel does not provide the requested operation");
    }
    KernelWorker worker(
        dbh, ht, x1, x2, bark_ratio, output, status, *kernel, operation);
    RcppParallel::parallelFor(0, static_cast<std::size_t>(size), worker, 1,
                              threads);
  }
  return Rcpp::List::create(Rcpp::Named("value") = output,
                            Rcpp::Named("status") = status);
}

// [[Rcpp::export]]
Rcpp::List tv_cpp_profile_eval_impl(
    const std::string& kernel_name, Rcpp::IntegerVector tree,
    Rcpp::NumericVector height, Rcpp::IntegerVector initial_status,
    Rcpp::NumericVector dbh, Rcpp::NumericVector ht,
    Rcpp::NumericVector lower, Rcpp::NumericVector bark_ratio,
    Rcpp::NumericVector upper_ht1, Rcpp::NumericVector upper_d1,
    Rcpp::NumericVector upper_ht2, Rcpp::NumericVector upper_d2,
    Rcpp::IntegerVector upper_bark, Rcpp::NumericVector site_index,
    Rcpp::NumericVector basal_area, Rcpp::NumericVector form_class,
    Rcpp::NumericVector coefficients,
    bool nvel_compat,
    int threads) {
  const R_xlen_t size = tree.size();
  if (height.size() != size || initial_status.size() != size) {
    Rcpp::stop("compiled profile-row inputs must have equal lengths");
  }
  const R_xlen_t tree_count = dbh.size();
  if (ht.size() != tree_count || lower.size() != tree_count ||
      bark_ratio.size() != tree_count || upper_ht1.size() != tree_count ||
      upper_d1.size() != tree_count || upper_ht2.size() != tree_count ||
      upper_d2.size() != tree_count || upper_bark.size() != tree_count ||
      site_index.size() != tree_count || basal_area.size() != tree_count ||
      form_class.size() != tree_count) {
    Rcpp::stop("compiled profile-tree inputs must have equal lengths");
  }
  Rcpp::NumericVector row_dbh(size);
  Rcpp::NumericVector row_ht(size);
  Rcpp::NumericVector row_lower(size);
  Rcpp::NumericVector row_bark_ratio(size);
  Rcpp::NumericVector row_upper_ht1(size);
  Rcpp::NumericVector row_upper_d1(size);
  Rcpp::NumericVector row_upper_ht2(size);
  Rcpp::NumericVector row_upper_d2(size);
  Rcpp::IntegerVector row_upper_bark(size);
  Rcpp::NumericVector row_site_index(size);
  Rcpp::NumericVector row_basal_area(size);
  Rcpp::NumericVector row_form_class(size);
  Rcpp::NumericVector zero(size);
  for (R_xlen_t row = 0; row < size; ++row) {
    const int source = tree[row] - 1;
    if (source < 0 || source >= tree_count) {
      Rcpp::stop("compiled profile tree index is out of range");
    }
    row_dbh[row] = dbh[source];
    row_ht[row] = ht[source];
    row_lower[row] = lower[source];
    row_bark_ratio[row] = bark_ratio[source];
    row_upper_ht1[row] = upper_ht1[source];
    row_upper_d1[row] = upper_d1[source];
    row_upper_ht2[row] = upper_ht2[source];
    row_upper_d2[row] = upper_d2[source];
    row_upper_bark[row] = upper_bark[source];
    row_site_index[row] = site_index[source];
    row_basal_area[row] = basal_area[source];
    row_form_class[row] = form_class[source];
  }
  const Rcpp::List inside = tv_cpp_kernel_eval_impl(
      kernel_name, 1, row_dbh, row_ht, height, zero, row_bark_ratio,
      row_upper_ht1, row_upper_d1, row_upper_ht2, row_upper_d2, row_upper_bark,
      row_site_index, row_basal_area, row_form_class, coefficients,
      nvel_compat, threads);
  const Rcpp::List outside = tv_cpp_kernel_eval_impl(
      kernel_name, 4, row_dbh, row_ht, height, zero, row_bark_ratio,
      row_upper_ht1, row_upper_d1, row_upper_ht2, row_upper_d2, row_upper_bark,
      row_site_index, row_basal_area, row_form_class, coefficients,
      nvel_compat, threads);
  const Rcpp::List inside_volume = tv_cpp_kernel_eval_impl(
      kernel_name, 3, row_dbh, row_ht, row_lower, height, row_bark_ratio,
      row_upper_ht1, row_upper_d1, row_upper_ht2, row_upper_d2, row_upper_bark,
      row_site_index, row_basal_area, row_form_class, coefficients,
      nvel_compat, threads);
  const Rcpp::List outside_volume = tv_cpp_kernel_eval_impl(
      kernel_name, 8, row_dbh, row_ht, row_lower, height, row_bark_ratio,
      row_upper_ht1, row_upper_d1, row_upper_ht2, row_upper_d2, row_upper_bark,
      row_site_index, row_basal_area, row_form_class, coefficients,
      nvel_compat, threads);
  Rcpp::NumericVector inside_value = inside["value"];
  Rcpp::NumericVector outside_value = outside["value"];
  Rcpp::NumericVector inside_volume_value = inside_volume["value"];
  Rcpp::NumericVector outside_volume_value = outside_volume["value"];
  const Rcpp::IntegerVector inside_status = inside["status"];
  const Rcpp::IntegerVector outside_status = outside["status"];
  const Rcpp::IntegerVector inside_volume_status = inside_volume["status"];
  const Rcpp::IntegerVector outside_volume_status = outside_volume["status"];
  Rcpp::IntegerVector status = Rcpp::clone(initial_status);
  auto merge_status = [&status](R_xlen_t row, int returned) {
    const int current = status[row];
    const bool model_before_numeric = current >= 100 && current < 150 &&
        returned >= 50 && returned < 100 && returned != 52;
    if (returned != 0 &&
        (current == 0 || model_before_numeric || current == 52)) {
      status[row] = returned;
    }
  };
  for (R_xlen_t row = 0; row < size; ++row) {
    merge_status(row, inside_status[row]);
    merge_status(row, outside_status[row]);
    const bool at_lower = height[row] == row_lower[row];
    if (at_lower &&
        (status[row] == 0 || status[row] == 52 || status[row] == 102)) {
      inside_volume_value[row] = 0.0;
      outside_volume_value[row] = 0.0;
    } else {
      merge_status(row, inside_volume_status[row]);
      merge_status(row, outside_volume_status[row]);
    }
    if (status[row] != 0 && status[row] != 52 && status[row] != 102) {
      inside_value[row] = NA_REAL;
      outside_value[row] = NA_REAL;
      inside_volume_value[row] = NA_REAL;
      outside_volume_value[row] = NA_REAL;
    }
  }
  return Rcpp::List::create(
      Rcpp::Named("dib") = inside_value,
      Rcpp::Named("dob") = outside_value,
      Rcpp::Named("cum_volume_ib") = inside_volume_value,
      Rcpp::Named("cum_volume_ob") = outside_volume_value,
      Rcpp::Named("status") = status);
}

// [[Rcpp::export]]
Rcpp::List tv_cpp_nsvb_biomass_impl(
    Rcpp::NumericVector dbh, Rcpp::NumericVector ht,
    Rcpp::IntegerVector spcd, Rcpp::IntegerVector division,
    Rcpp::IntegerVector region, Rcpp::IntegerVector forest,
    Rcpp::IntegerVector decay_class, Rcpp::NumericVector cull,
    Rcpp::NumericVector primary_top, Rcpp::NumericVector secondary_top,
    Rcpp::NumericVector stump, Rcpp::NumericVector max_log_length,
    Rcpp::NumericVector min_log_length, Rcpp::NumericVector minimum_top_length,
    Rcpp::NumericVector merchantable_length, Rcpp::NumericVector trim,
    Rcpp::IntegerVector even_or_odd, Rcpp::IntegerVector option,
    Rcpp::IntegerVector corrected_scribner, Rcpp::IntegerVector ctype,
    int threads) {
  const R_xlen_t size = dbh.size();
  if (ht.size() != size || spcd.size() != size || division.size() != size ||
      region.size() != size || forest.size() != size ||
      decay_class.size() != size || cull.size() != size ||
      primary_top.size() != size || secondary_top.size() != size ||
      stump.size() != size || max_log_length.size() != size ||
      min_log_length.size() != size || minimum_top_length.size() != size ||
      merchantable_length.size() != size || trim.size() != size ||
      even_or_odd.size() != size || option.size() != size ||
      corrected_scribner.size() != size || ctype.size() != size) {
    Rcpp::stop("compiled NSVB inputs must have equal lengths");
  }
  if (threads < 1) {
    Rcpp::stop("threads must be at least one");
  }
  Rcpp::NumericMatrix output(size, 30);
  Rcpp::NumericMatrix volume(size, 15);
  Rcpp::IntegerVector n_logs_primary(size);
  Rcpp::IntegerVector n_logs_secondary(size);
  Rcpp::IntegerVector status(size);
  std::vector<int> representatives;
  std::vector<int> map;
  bool deduplicate = false;
  if (size >= 32) {
    const R_xlen_t sample_size = std::min<R_xlen_t>(size, 1024);
    std::unordered_map<BiomassKey, int, BiomassKeyHash> sample;
    sample.reserve(static_cast<std::size_t>(sample_size));
    for (R_xlen_t index = 0; index < sample_size; ++index) {
      sample.emplace(biomass_key_at(
          index, dbh, ht, spcd, division, region, forest, decay_class, cull,
          primary_top, secondary_top, stump, max_log_length, min_log_length,
          minimum_top_length, merchantable_length, trim, even_or_odd, option,
          corrected_scribner, ctype), 0);
    }
    if (sample.size() * 2 <= static_cast<std::size_t>(sample_size)) {
      std::unordered_map<BiomassKey, int, BiomassKeyHash> dictionary;
      dictionary.reserve(static_cast<std::size_t>(sample.size()) * 2);
      representatives.reserve(static_cast<std::size_t>(sample.size()) * 2);
      map.resize(static_cast<std::size_t>(size));
      for (R_xlen_t index = 0; index < size; ++index) {
        const BiomassKey key = biomass_key_at(
            index, dbh, ht, spcd, division, region, forest, decay_class, cull,
            primary_top, secondary_top, stump, max_log_length, min_log_length,
            minimum_top_length, merchantable_length, trim, even_or_odd, option,
            corrected_scribner, ctype);
        auto found = dictionary.find(key);
        if (found == dictionary.end()) {
          const int position = static_cast<int>(representatives.size());
          dictionary.emplace(key, position);
          representatives.push_back(static_cast<int>(index));
          map[static_cast<std::size_t>(index)] = position;
        } else {
          map[static_cast<std::size_t>(index)] = found->second;
        }
      }
      deduplicate = representatives.size() * 2 <=
          static_cast<std::size_t>(size);
    }
  }
  NsvbBiomassWorker worker(
      dbh, ht, spcd, division, region, forest, decay_class, cull, primary_top,
      secondary_top, stump, max_log_length, min_log_length,
      minimum_top_length, merchantable_length, trim, even_or_odd, option,
      corrected_scribner, ctype, output, volume, n_logs_primary,
      n_logs_secondary, status, deduplicate ? &representatives : nullptr);
  const std::size_t work_size = deduplicate
      ? representatives.size() : static_cast<std::size_t>(size);
  RcppParallel::parallelFor(0, work_size, worker, 1, threads);
  if (deduplicate) {
    NsvbBiomassScatterWorker scatter(
        output, volume, n_logs_primary, n_logs_secondary, status,
        representatives, map);
    RcppParallel::parallelFor(
        0, static_cast<std::size_t>(size), scatter, 1, threads);
  }
  return Rcpp::List::create(Rcpp::Named("value") = output,
                            Rcpp::Named("volume") = volume,
                            Rcpp::Named("n_logs_primary") = n_logs_primary,
                            Rcpp::Named("n_logs_secondary") = n_logs_secondary,
                            Rcpp::Named("status") = status);
}

// [[Rcpp::export]]
Rcpp::NumericVector tv_cpp_nsvb_carbon_fraction_impl(
    Rcpp::IntegerVector spcd, bool raw) {
  Rcpp::NumericVector output(spcd.size());
  for (R_xlen_t index = 0; index < spcd.size(); ++index) {
    output[index] = treevolume::nsvb::carbon_fraction(spcd[index], raw);
  }
  return output;
}

namespace {

struct DivisionRing {
  std::size_t begin;
  std::size_t end;
  int division;
  bool hole;
  double xmin;
  double xmax;
  double ymin;
  double ymax;
};

struct DivisionShape {
  std::size_t ring_begin;
  std::size_t ring_end;
  int division;
  double xmin;
  double xmax;
  double ymin;
  double ymax;
};

bool point_on_segment(double x, double y, double x1, double y1,
                      double x2, double y2) {
  const double scale = std::max(
      1.0, std::max(std::max(std::abs(x1), std::abs(y1)),
                    std::max(std::abs(x2), std::abs(y2))));
  const double tolerance = 1e-12 * scale * scale;
  const double cross = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1);
  if (std::abs(cross) > tolerance) return false;
  const double coordinate_tolerance = 1e-12 * scale;
  return x >= std::min(x1, x2) - coordinate_tolerance &&
      x <= std::max(x1, x2) + coordinate_tolerance &&
      y >= std::min(y1, y2) - coordinate_tolerance &&
      y <= std::max(y1, y2) + coordinate_tolerance;
}

struct DivisionPointWorker : public RcppParallel::Worker {
  const RcppParallel::RVector<double> point_x;
  const RcppParallel::RVector<double> point_y;
  const RcppParallel::RVector<double> polygon_x;
  const RcppParallel::RVector<double> polygon_y;
  const std::vector<DivisionRing>& rings;
  const std::vector<DivisionShape>& shapes;
  RcppParallel::RVector<int> output;

  DivisionPointWorker(
      const Rcpp::NumericVector& point_x_value,
      const Rcpp::NumericVector& point_y_value,
      const Rcpp::NumericVector& polygon_x_value,
      const Rcpp::NumericVector& polygon_y_value,
      const std::vector<DivisionRing>& ring_value,
      const std::vector<DivisionShape>& shape_value,
      Rcpp::IntegerVector output_value)
      : point_x(point_x_value), point_y(point_y_value),
        polygon_x(polygon_x_value), polygon_y(polygon_y_value),
        rings(ring_value), shapes(shape_value), output(output_value) {}

  bool inside_ring(double x, double y, const DivisionRing& ring,
                   bool* boundary) const {
    bool inside = false;
    std::size_t previous = ring.end - 1;
    for (std::size_t current = ring.begin; current < ring.end; ++current) {
      const double x1 = polygon_x[previous];
      const double y1 = polygon_y[previous];
      const double x2 = polygon_x[current];
      const double y2 = polygon_y[current];
      if (point_on_segment(x, y, x1, y1, x2, y2)) {
        *boundary = true;
        return false;
      }
      const bool crosses = ((y1 > y) != (y2 > y)) &&
          (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1);
      if (crosses) inside = !inside;
      previous = current;
    }
    return inside;
  }

  bool inside_shape(double x, double y, const DivisionShape& shape) const {
    bool inside = false;
    for (std::size_t index = shape.ring_begin;
         index < shape.ring_end; ++index) {
      const DivisionRing& ring = rings[index];
      if (x < ring.xmin || x > ring.xmax ||
          y < ring.ymin || y > ring.ymax) {
        continue;
      }
      bool boundary = false;
      const bool ring_inside = inside_ring(x, y, ring, &boundary);
      if (boundary) return true;
      if (ring_inside) inside = !inside;
    }
    return inside;
  }

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t point = begin; point < end; ++point) {
      const double x = point_x[point];
      const double y = point_y[point];
      output[point] = NA_INTEGER;
      if (!std::isfinite(x) || !std::isfinite(y)) continue;
      for (const DivisionShape& shape : shapes) {
        if (x < shape.xmin || x > shape.xmax ||
            y < shape.ymin || y > shape.ymax) {
          continue;
        }
        if (inside_shape(x, y, shape)) {
          output[point] = shape.division;
          break;
        }
      }
    }
  }
};

}  // namespace

// [[Rcpp::export]]
Rcpp::IntegerVector tv_cpp_nsvb_division_xy_impl(
    Rcpp::NumericVector x, Rcpp::NumericVector y,
    Rcpp::IntegerVector division, Rcpp::IntegerVector ring_id,
    Rcpp::LogicalVector hole, Rcpp::NumericVector polygon_x,
    Rcpp::NumericVector polygon_y, int threads) {
  if (x.size() != y.size()) {
    Rcpp::stop("compiled coordinate inputs must have equal lengths");
  }
  const R_xlen_t polygon_size = division.size();
  if (ring_id.size() != polygon_size || hole.size() != polygon_size ||
      polygon_x.size() != polygon_size || polygon_y.size() != polygon_size) {
    Rcpp::stop("compiled polygon columns must have equal lengths");
  }
  if (threads < 1) {
    Rcpp::stop("threads must be at least one");
  }
  std::vector<DivisionRing> rings;
  for (R_xlen_t begin = 0; begin < polygon_size;) {
    const int current_ring = ring_id[begin];
    const int current_division = division[begin];
    const int current_hole = hole[begin];
    if (current_ring == NA_INTEGER || current_division == NA_INTEGER ||
        current_hole == NA_LOGICAL) {
      Rcpp::stop("compiled polygon identifiers must not be missing");
    }
    R_xlen_t end = begin + 1;
    double xmin = polygon_x[begin];
    double xmax = polygon_x[begin];
    double ymin = polygon_y[begin];
    double ymax = polygon_y[begin];
    if (!std::isfinite(xmin) || !std::isfinite(ymin)) {
      Rcpp::stop("compiled polygon coordinates must be finite");
    }
    while (end < polygon_size && ring_id[end] == current_ring) {
      if (division[end] != current_division || hole[end] != current_hole ||
          !std::isfinite(polygon_x[end]) || !std::isfinite(polygon_y[end])) {
        Rcpp::stop("compiled polygon ring metadata is inconsistent");
      }
      xmin = std::min(xmin, polygon_x[end]);
      xmax = std::max(xmax, polygon_x[end]);
      ymin = std::min(ymin, polygon_y[end]);
      ymax = std::max(ymax, polygon_y[end]);
      ++end;
    }
    if (end - begin < 4) {
      Rcpp::stop("compiled polygon rings must have at least four vertices");
    }
    rings.push_back(DivisionRing{
        static_cast<std::size_t>(begin), static_cast<std::size_t>(end),
        current_division, current_hole != 0, xmin, xmax, ymin, ymax});
    begin = end;
  }

  std::vector<DivisionShape> shapes;
  for (std::size_t begin = 0; begin < rings.size();) {
    const int current_division = rings[begin].division;
    std::size_t end = begin + 1;
    double xmin = rings[begin].xmin;
    double xmax = rings[begin].xmax;
    double ymin = rings[begin].ymin;
    double ymax = rings[begin].ymax;
    while (end < rings.size() && rings[end].division == current_division) {
      xmin = std::min(xmin, rings[end].xmin);
      xmax = std::max(xmax, rings[end].xmax);
      ymin = std::min(ymin, rings[end].ymin);
      ymax = std::max(ymax, rings[end].ymax);
      ++end;
    }
    shapes.push_back(DivisionShape{
        begin, end, current_division, xmin, xmax, ymin, ymax});
    begin = end;
  }

  Rcpp::IntegerVector output(x.size(), NA_INTEGER);
  DivisionPointWorker worker(
      x, y, polygon_x, polygon_y, rings, shapes, output);
  RcppParallel::parallelFor(
      0, static_cast<std::size_t>(x.size()), worker, 1, threads);
  return output;
}
