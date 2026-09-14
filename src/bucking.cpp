#include <Rcpp.h>
#include <RcppParallel.h>

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iomanip>
#include <limits>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace {

using ProfileClock = std::chrono::steady_clock;

double profile_seconds(ProfileClock::time_point begin,
                       ProfileClock::time_point end) {
  return std::chrono::duration<double>(end - begin).count();
}

inline bool finite_limit(double value) {
  return !Rcpp::NumericVector::is_na(value) && std::isfinite(value);
}

inline bool overlaps(double start, double end, double from, double to) {
  return from < end && to > start;
}

inline bool parity_ok(double length, int parity) {
  if (parity == 0) return true;
  const double rounded = std::round(length);
  if (length != rounded) return false;
  const long integer = static_cast<long>(rounded);
  return parity == 1 ? integer % 2 == 0 : integer % 2 != 0;
}

int nvel_numlog(int option, int even_or_odd, double merchant_length,
                double maximum_length, double minimum_length, double trim) {
  int number = static_cast<int>(merchant_length / (maximum_length + trim));
  const double leftover = merchant_length -
    (maximum_length + trim) * static_cast<double>(number);
  if (!(number > 0 || leftover >= minimum_length)) return 0;
  if (option < 20) {
    if (leftover >= trim + 0.5) ++number;
  } else if (option == 21 || option == 22) {
    const double threshold = even_or_odd == 1 ? trim + 0.5 : trim + 1.0;
    if (leftover >= threshold) ++number;
  } else if (option == 23) {
    if (leftover >= trim + minimum_length) ++number;
  } else if (option == 24) {
    if (leftover >= (maximum_length + trim) / 4.0) ++number;
  }
  return std::min(number, 20);
}

std::vector<double> nvel_segments(int option, int even_or_odd,
                                  double merchant_length,
                                  double maximum_length,
                                  double minimum_length, double trim) {
  const int number = nvel_numlog(
    option, even_or_odd, merchant_length, maximum_length, minimum_length, trim
  );
  if (number == 0) return {};
  double length = merchant_length - static_cast<double>(number) * trim;
  length = even_or_odd == 1
    ? std::trunc(length + 0.5)
    : std::trunc((length + 1.0) / 2.0) * 2.0;
  length = std::min(length, static_cast<double>(number) * maximum_length);
  std::vector<double> logs(static_cast<std::size_t>(number), 0.0);
  if (number == 1) {
    if (option == 24) {
      logs[0] = length < maximum_length * 0.25
        ? 0.0
        : length <= maximum_length * 0.75
        ? maximum_length / 2.0
        : maximum_length;
    } else if (length >= minimum_length) {
      logs[0] = std::min(length, maximum_length);
    }
  } else if (option < 20) {
    const int average = static_cast<int>(length / static_cast<double>(number));
    double leftover = length - static_cast<double>(average * number);
    std::fill(logs.begin(), logs.end(), static_cast<double>(average));
    if (average > (average / 2) * 2) {
      for (int i = 1; i <= number; ++i) {
        if (number - 2 * i + 1 >= 1) {
          logs[static_cast<std::size_t>(i - 1)] += 1.0;
          logs[static_cast<std::size_t>(number - i)] -= 1.0;
        }
      }
    }
    if (leftover > 0 && number > (number / 2) * 2) {
      for (int i = 0; i < number && leftover > 0; ++i) {
        const int log_length = static_cast<int>(
          logs[static_cast<std::size_t>(i)]
        );
        if (log_length > (log_length / 2) * 2) {
          logs[static_cast<std::size_t>(i)] += 1.0;
          leftover -= 1.0;
        }
      }
    }
    int iterations = 0;
    while (leftover > 0 && iterations <= 500) {
      for (int i = 0; i < number && leftover > 0; ++i) {
        if (logs[static_cast<std::size_t>(i)] >= maximum_length) continue;
        const double next = i == number - 1
          ? logs.back()
          : logs[static_cast<std::size_t>(i + 1)];
        int target = -1;
        if (logs[static_cast<std::size_t>(i)] == logs.back()) target = i;
        else if (logs[static_cast<std::size_t>(i)] > next) target = i + 1;
        if (target >= 0) {
          const double addition = leftover >= 2 ? 2.0 : 1.0;
          logs[static_cast<std::size_t>(target)] += addition;
          leftover -= addition;
        }
      }
      ++iterations;
    }
  } else {
    const double leftover = length -
      std::trunc(maximum_length) * static_cast<double>(number - 1);
    std::fill(logs.begin(), logs.end(), maximum_length);
    if (option == 21) {
      if (leftover >= maximum_length / 2.0) {
        logs.back() = leftover;
      } else {
        logs.back() = std::trunc((maximum_length + leftover) / 2.0);
        logs[static_cast<std::size_t>(number - 2)] =
          maximum_length + leftover - logs.back();
        const double previous = logs[static_cast<std::size_t>(number - 2)];
        if (logs.back() == previous &&
            static_cast<int>(logs.back()) % 2 == 1) {
          logs.back() -= 1.0;
          logs[static_cast<std::size_t>(number - 2)] += 1.0;
        }
      }
    } else if (option == 22) {
      logs.back() = std::trunc((maximum_length + leftover) / 2.0);
      logs[static_cast<std::size_t>(number - 2)] =
        maximum_length + leftover - logs.back();
      if (logs.back() < minimum_length) {
        logs.back() = 0.0;
        logs[static_cast<std::size_t>(number - 2)] = maximum_length;
      } else {
        const double previous = logs[static_cast<std::size_t>(number - 2)];
        if (logs.back() == previous &&
            static_cast<int>(logs.back()) % 2 == 1) {
          logs.back() -= 1.0;
          logs[static_cast<std::size_t>(number - 2)] += 1.0;
        }
      }
    } else if (option == 23) {
      logs.back() = leftover >= minimum_length ? leftover : 0.0;
    } else if (option == 24) {
      logs.back() = leftover < maximum_length * 0.25
        ? 0.0
        : leftover <= maximum_length * 0.75
        ? std::trunc(maximum_length * 0.5 + 0.5)
        : maximum_length;
    }
  }
  logs.erase(std::remove(logs.begin(), logs.end(), 0.0), logs.end());
  return logs;
}

struct ProductData {
  std::vector<int> priority;
  std::vector<int> length_mode;
  std::vector<int> length_offsets;
  std::vector<int> length_ticks;
  std::vector<int> min_tick;
  std::vector<int> max_tick;
  std::vector<int> step_tick;
  std::vector<int> parity;
  std::vector<int> segmentation_policy;
  std::vector<double> trim;
  std::vector<double> min_top;
  std::vector<double> min_sed;
  std::vector<double> max_sed;
  std::vector<double> min_led;
  std::vector<double> max_led;
  std::vector<int> diameter_basis;
  std::vector<int> max_sweep;
  std::vector<int> max_crook;
  std::vector<double> max_defect_pct;
  std::vector<int> max_logs_per_segment;
  std::vector<int> allow_lower_products;
  std::vector<int> pulp_product;
  std::vector<int> scale_rule;
  std::vector<int> measurement_quantity;
  std::vector<int> scale_unit;
  std::vector<int> scale_basis;
  std::vector<int> diameter_round;
  std::vector<int> length_round;
  std::vector<int> volume_round;
  std::vector<double> cord_fraction;
  std::vector<double> price;
  std::vector<double> price_quantity;
  std::vector<std::string> id;
  std::vector<double> scribner_factor;
  std::vector<int> scribner_exception;
  double intl_quadratic;
  double intl_linear;
  double intl_adjustment;

  explicit ProductData(const Rcpp::List& products)
    : priority(Rcpp::as<std::vector<int>>(products["priority"])),
      length_mode(Rcpp::as<std::vector<int>>(products["length_mode"])),
      length_offsets(Rcpp::as<std::vector<int>>(products["length_offsets"])),
      length_ticks(Rcpp::as<std::vector<int>>(products["length_ticks"])),
      min_tick(Rcpp::as<std::vector<int>>(products["min_tick"])),
      max_tick(Rcpp::as<std::vector<int>>(products["max_tick"])),
      step_tick(Rcpp::as<std::vector<int>>(products["step_tick"])),
      parity(Rcpp::as<std::vector<int>>(products["parity"])),
      segmentation_policy(
        Rcpp::as<std::vector<int>>(products["segmentation_policy"])
      ),
      trim(Rcpp::as<std::vector<double>>(products["trim"])),
      min_top(Rcpp::as<std::vector<double>>(products["min_top"])),
      min_sed(Rcpp::as<std::vector<double>>(products["min_sed"])),
      max_sed(Rcpp::as<std::vector<double>>(products["max_sed"])),
      min_led(Rcpp::as<std::vector<double>>(products["min_led"])),
      max_led(Rcpp::as<std::vector<double>>(products["max_led"])),
      diameter_basis(Rcpp::as<std::vector<int>>(products["diameter_basis"])),
      max_sweep(Rcpp::as<std::vector<int>>(products["max_sweep"])),
      max_crook(Rcpp::as<std::vector<int>>(products["max_crook"])),
      max_defect_pct(
        Rcpp::as<std::vector<double>>(products["max_defect_pct"])
      ),
      max_logs_per_segment(Rcpp::as<std::vector<int>>(products["max_logs_per_segment"])),
      allow_lower_products(
        Rcpp::as<std::vector<int>>(products["allow_lower_products"])
      ),
      pulp_product(Rcpp::as<std::vector<int>>(products["pulp_product"])),
      scale_rule(Rcpp::as<std::vector<int>>(products["scale_rule"])),
      measurement_quantity(Rcpp::as<std::vector<int>>(products["measurement_quantity"])),
      scale_unit(Rcpp::as<std::vector<int>>(products["scale_unit"])),
      scale_basis(Rcpp::as<std::vector<int>>(products["scale_basis"])),
      diameter_round(
        Rcpp::as<std::vector<int>>(products["diameter_round"])
      ),
      length_round(Rcpp::as<std::vector<int>>(products["length_round"])),
      volume_round(Rcpp::as<std::vector<int>>(products["volume_round"])),
      cord_fraction(Rcpp::as<std::vector<double>>(products["cord_fraction"])),
      price(Rcpp::as<std::vector<double>>(products["price"])),
      price_quantity(
        Rcpp::as<std::vector<double>>(products["price_quantity"])
      ),
      intl_quadratic(Rcpp::as<double>(products["intl_quadratic"])),
      intl_linear(Rcpp::as<double>(products["intl_linear"])),
      intl_adjustment(Rcpp::as<double>(products["intl_adjustment"])) {
    const Rcpp::CharacterVector identifiers = products["product"];
    id.reserve(identifiers.size());
    for (R_xlen_t i = 0; i < identifiers.size(); ++i) {
      id.push_back(Rcpp::as<std::string>(identifiers[i]));
    }
    scribner_factor = Rcpp::as<std::vector<double>>(
      products["scribner_factor"]
    );
    scribner_exception = Rcpp::as<std::vector<int>>(
      products["scribner_exception"]
    );
  }

  int size() const {
    return static_cast<int>(priority.size());
  }
};

struct ProfileView {
  const Rcpp::IntegerVector offsets;
  const Rcpp::NumericVector height;
  const Rcpp::NumericVector dib;
  const Rcpp::NumericVector dob;
  const Rcpp::NumericVector cum_ib;
  const Rcpp::NumericVector cum_ob;

  ProfileView(const Rcpp::IntegerVector& offsets_,
              const Rcpp::NumericVector& height_,
              const Rcpp::NumericVector& dib_,
              const Rcpp::NumericVector& dob_,
              const Rcpp::NumericVector& cum_ib_,
              const Rcpp::NumericVector& cum_ob_)
    : offsets(offsets_), height(height_), dib(dib_), dob(dob_),
      cum_ib(cum_ib_), cum_ob(cum_ob_) {}

  int find(int tree, double value) const {
    int left = offsets[tree];
    int right = offsets[tree + 1];
    while (left < right) {
      const int middle = left + (right - left) / 2;
      if (height[middle] < value) left = middle + 1;
      else right = middle;
    }
    if (left < offsets[tree + 1] && height[left] == value) return left;
    return -1;
  }
};

enum FailureReason {
  feasible = 0,
  diameter_failure = 1,
  other_failure = 2
};

struct Candidate {
  int kind = 0;
  double length = 0;
  double end = 0;
  double located = 0;
  double rot_pct = 0;
  int start_profile = -1;
  int end_profile = -1;
};

struct Arc {
  int product = 0;
  int stage = 0;
  int priority = 0;
  Candidate candidate;
  double reward = 0;
  double physical_net = 0;
};

struct Path {
  bool valid = true;
  std::vector<Arc> arcs;
  double objective = 0;
  double physical_net = 0;
  int residual_cause = 0;
  double residual_from = 0;
};

struct PieceRow {
  int tree;
  int log;
  int segment;
  int stage;
  int product;
  int kind;
  double start;
  double nominal_end;
  double end;
  double nominal_length;
  double physical_length;
  double trim;
  double led_ib;
  double sed_ib;
  double led_ob;
  double sed_ob;
  double gross_ib;
  double gross_ob;
  double located;
  double rot_pct;
  double gross_scale;
};

struct ResidualRow {
  int tree;
  int segment;
  int cause;
  double from;
  double to;
  double volume;
};

struct TreeResult {
  std::vector<PieceRow> logs;
  std::vector<ResidualRow> residuals;
  int status = 0;
};

double round_dimension(double value, int operation, int dimension, int units) {
  if (operation <= 1) return value;
  const bool truncate = operation == 2 || operation == 5 ||
    operation == 7 || operation == 9 || operation == 11;
  if (dimension == 0) {
    if (operation >= 2 && operation <= 4) {
      const double native = units == 1 ? value : value / 2.54;
      const double increment = operation == 4 ? 0.5 : 1.0;
      const double rounded = truncate
        ? std::floor(native / increment) * increment
        : std::floor(native / increment + 0.5) * increment;
      return units == 1 ? rounded : rounded * 2.54;
    }
    const double native = units == 2 ? value : value * 2.54;
    const double rounded = truncate ? std::floor(native) :
      std::floor(native + 0.5);
    return units == 2 ? rounded : rounded / 2.54;
  }
  if (dimension == 1) {
    if (operation == 7 || operation == 8) {
      const double native = units == 1 ? value : value / 0.3048;
      const double rounded = truncate ? std::floor(native) :
        std::floor(native + 0.5);
      return units == 1 ? rounded : rounded * 0.3048;
    }
    const double native = units == 2 ? value : value * 0.3048;
    const double increment = 0.1;
    const double rounded = truncate
      ? std::floor(native / increment) * increment
      : std::floor(native / increment + 0.5) * increment;
    return units == 2 ? rounded : rounded / 0.3048;
  }
  const double increment = operation == 13 ? 10.0 : 1.0;
  return truncate
    ? std::floor(value / increment) * increment
    : std::floor(value / increment + 0.5) * increment;
}

double scribner(const ProductData& products, double diameter,
                double length, bool corrected) {
  if (diameter < 1) return 0;
  const int rounded_diameter = std::min(static_cast<int>(diameter), 120);
  int index = rounded_diameter;
  if (rounded_diameter > 5 && rounded_diameter <= 11) {
    if (length > 15 && length < 32) index = rounded_diameter + 115;
    if (length > 31 && length < 41) index = rounded_diameter + 121;
  }
  const double factor = products.scribner_factor[
    static_cast<std::size_t>(index - 1)
  ];
  if (!corrected) return std::trunc(length * factor + 0.5);
  double volume = std::trunc((length * factor + 5.0) / 10.0);
  const double key = length * 1000.0 + std::min(diameter, 120.0);
  for (int encoded : products.scribner_exception) {
    if (static_cast<double>(encoded / 10) == key) {
      volume += encoded % 2 == 1 ? 1.0 : -1.0;
      break;
    }
  }
  return volume * 10.0;
}

double international(const ProductData& products, double diameter,
                     double length) {
  if (diameter < 4) return 0;
  const int segments = static_cast<int>(length / 4.0);
  const double fraction = length / 4.0 - static_cast<double>(segments);
  double volume = 0;
  for (int j = 1; j <= segments; ++j) {
    const double sed = diameter + static_cast<double>(segments - j) / 2.0;
    volume += (products.intl_quadratic * sed * sed -
      products.intl_linear * sed) * products.intl_adjustment;
  }
  if (fraction > 0) {
    volume += fraction * (products.intl_quadratic * diameter * diameter -
      products.intl_linear * diameter) * products.intl_adjustment;
  }
  if (volume < 7.5) return 5;
  const int tens = static_cast<int>(volume / 10.0);
  const int remainder = static_cast<int>(
    (volume / 10.0 - static_cast<double>(tens)) * 100.0
  );
  if (remainder < 25) return tens * 10.0;
  if (remainder >= 75) return (tens + 1) * 10.0;
  return tens * 10.0 + 5.0;
}

class BuckingCore {
 public:
  const ProductData products;
  const double quantum;
  const int units;
  const int objective;
  const Rcpp::IntegerVector tree_status;
  const Rcpp::IntegerVector chain_offsets;
  const Rcpp::IntegerVector chain_product;
  const Rcpp::IntegerVector segment_offsets;
  const Rcpp::NumericVector segment_lo;
  const Rcpp::NumericVector segment_hi;
  const Rcpp::IntegerVector boundary_offsets;
  const Rcpp::NumericVector boundaries;
  const ProfileView profile;
  const Rcpp::IntegerVector defect_offsets;
  const Rcpp::NumericVector defect_from;
  const Rcpp::NumericVector defect_to;
  const Rcpp::IntegerVector defect_effect;
  const Rcpp::NumericVector defect_percent;
  const Rcpp::IntegerVector defect_category_rank;
  const Rcpp::NumericVector weight_factor;
  const int n_product;

  BuckingCore(
      const Rcpp::List& products_, double quantum_, int units_, int objective_,
      const Rcpp::IntegerVector& tree_status_,
      const Rcpp::IntegerVector& chain_offsets_,
      const Rcpp::IntegerVector& chain_product_,
      const Rcpp::IntegerVector& segment_offsets_,
      const Rcpp::NumericVector& segment_lo_,
      const Rcpp::NumericVector& segment_hi_,
      const Rcpp::IntegerVector& boundary_offsets_,
      const Rcpp::NumericVector& boundaries_,
      const Rcpp::IntegerVector& profile_offsets_,
      const Rcpp::NumericVector& profile_height_,
      const Rcpp::NumericVector& dib_, const Rcpp::NumericVector& dob_,
      const Rcpp::NumericVector& cum_ib_, const Rcpp::NumericVector& cum_ob_,
      const Rcpp::IntegerVector& defect_offsets_,
      const Rcpp::NumericVector& defect_from_,
      const Rcpp::NumericVector& defect_to_,
      const Rcpp::IntegerVector& defect_effect_,
      const Rcpp::NumericVector& defect_percent_,
      const Rcpp::IntegerVector& defect_category_rank_,
      const Rcpp::NumericVector& weight_factor_)
    : products(products_), quantum(quantum_), units(units_),
      objective(objective_), tree_status(tree_status_),
      chain_offsets(chain_offsets_), chain_product(chain_product_),
      segment_offsets(segment_offsets_), segment_lo(segment_lo_),
      segment_hi(segment_hi_), boundary_offsets(boundary_offsets_),
      boundaries(boundaries_),
      profile(profile_offsets_, profile_height_, dib_, dob_, cum_ib_, cum_ob_),
      defect_offsets(defect_offsets_), defect_from(defect_from_),
      defect_to(defect_to_), defect_effect(defect_effect_),
      defect_percent(defect_percent_),
      defect_category_rank(defect_category_rank_),
      weight_factor(weight_factor_), n_product(products.size()) {}

  FailureReason assess(int tree, int product, double start, double end,
                       Candidate& candidate) const {
    const int start_at = profile.find(tree, start);
    const int end_at = profile.find(tree, end);
    if (start_at < 0 || end_at < 0) return other_failure;
    const double led = products.diameter_basis[product] == 0
      ? profile.dib[start_at]
      : profile.dob[start_at];
    const double sed = products.diameter_basis[product] == 0
      ? profile.dib[end_at]
      : profile.dob[end_at];
    if (!std::isfinite(led) || !std::isfinite(sed)) return other_failure;
    if (led < products.min_led[product] ||
        sed < products.min_sed[product] ||
        (finite_limit(products.max_led[product]) &&
         led > products.max_led[product]) ||
        (finite_limit(products.max_sed[product]) &&
         sed > products.max_sed[product])) {
      return diameter_failure;
    }

    int sweep = -1;
    int crook = -1;
    std::vector<double> cells{start, end};
    for (int defect = defect_offsets[tree];
         defect < defect_offsets[tree + 1]; ++defect) {
      if (!overlaps(
          start, end, defect_from[defect], defect_to[defect]
      )) continue;
      const int effect = defect_effect[defect];
      if ((effect == 2 || effect == 7) &&
          products.pulp_product[product] == 0) {
        return other_failure;
      }
      if (effect == 3) return other_failure;
      if (effect == 5) {
        sweep = std::max(sweep, defect_category_rank[defect]);
      }
      if (effect == 6) {
        crook = std::max(crook, defect_category_rank[defect]);
      }
      if (effect == 4) {
        if (defect_from[defect] > start && defect_from[defect] < end) {
          cells.push_back(defect_from[defect]);
        }
        if (defect_to[defect] > start && defect_to[defect] < end) {
          cells.push_back(defect_to[defect]);
        }
      }
    }
    if ((products.max_sweep[product] >= 0 &&
         sweep > products.max_sweep[product]) ||
        (products.max_crook[product] >= 0 &&
         crook > products.max_crook[product])) {
      return other_failure;
    }
    std::sort(cells.begin(), cells.end());
    cells.erase(std::unique(cells.begin(), cells.end()), cells.end());
    double located = 0;
    for (std::size_t cell = 0; cell + 1 < cells.size(); ++cell) {
      const double lower = cells[cell];
      const double upper = cells[cell + 1];
      const double midpoint = (lower + upper) / 2.0;
      double rate = 0;
      for (int defect = defect_offsets[tree];
           defect < defect_offsets[tree + 1]; ++defect) {
        if (defect_effect[defect] == 4 &&
            defect_from[defect] <= midpoint &&
            defect_to[defect] > midpoint) {
          rate = std::max(rate, defect_percent[defect]);
        }
      }
      if (rate > 0) {
        const int lower_at = profile.find(tree, lower);
        const int upper_at = profile.find(tree, upper);
        if (lower_at < 0 || upper_at < 0) return other_failure;
        located += (profile.cum_ib[upper_at] - profile.cum_ib[lower_at]) *
          rate / 100.0;
      }
    }
    const double gross = profile.cum_ib[end_at] - profile.cum_ib[start_at];
    const double rot_pct = gross > 0 ? 100.0 * located / gross : 0;
    if (finite_limit(products.max_defect_pct[product]) &&
        rot_pct > products.max_defect_pct[product]) {
      return other_failure;
    }
    candidate.start_profile = start_at;
    candidate.end_profile = end_at;
    candidate.located = located;
    candidate.rot_pct = rot_pct;
    return feasible;
  }

  void consider(int tree, int product, double start, double length, int kind,
                double explicit_end, double segment_end,
                std::vector<Candidate>& candidates, bool* has_length,
                bool* non_diameter) const {
    if (!(length > 0)) return;
    const double end = std::isfinite(explicit_end)
      ? explicit_end
      : start + length + products.trim[product];
    if (end > segment_end) return;
    if (has_length != nullptr) *has_length = true;
    Candidate candidate;
    candidate.kind = kind;
    candidate.length = length;
    candidate.end = end;
    const FailureReason reason = assess(tree, product, start, end, candidate);
    if (reason != feasible) {
      if (non_diameter != nullptr && reason != diameter_failure) {
        *non_diameter = true;
      }
      return;
    }
    candidates.push_back(candidate);
  }

  double nvel_stage_end(int tree, int product, double start,
                        double segment_end) const {
    const int start_at = profile.find(tree, start);
    if (start_at < 0) return start;
    const double led = products.diameter_basis[product] == 0
      ? profile.dib[start_at]
      : profile.dob[start_at];
    if (!std::isfinite(led) || led < products.min_led[product] ||
        (finite_limit(products.max_led[product]) &&
         led > products.max_led[product])) {
      return start;
    }
    double best = start;
    const int key = tree * n_product + product;
    for (int at = boundary_offsets[key]; at < boundary_offsets[key + 1]; ++at) {
      const double boundary = boundaries[at];
      if (!(boundary > start) || boundary > segment_end) continue;
      const int end_at = profile.find(tree, boundary);
      if (end_at < 0) continue;
      const double sed = products.diameter_basis[product] == 0
        ? profile.dib[end_at]
        : profile.dob[end_at];
      if (!std::isfinite(sed) || sed < products.min_sed[product] ||
          (finite_limit(products.max_sed[product]) &&
           sed > products.max_sed[product])) {
        continue;
      }
      best = std::max(best, boundary);
    }
    return best;
  }

  std::vector<Candidate> enumerate(
      int tree, int product, double start, double segment_end,
      int count = 0, double stage_start = NA_REAL,
      bool* has_length = nullptr, bool* non_diameter = nullptr) const {
    std::vector<Candidate> candidates;
    if (products.segmentation_policy[product] != 0) {
      if (!std::isfinite(stage_start)) stage_start = start;
      const double stage_end = nvel_stage_end(
        tree, product, stage_start, segment_end
      );
      const double maximum = products.max_tick[product] * quantum;
      const double minimum = products.min_tick[product] * quantum;
      const int even_or_odd = products.parity[product] == 1 ? 2 : 1;
      const std::vector<double> logs = nvel_segments(
        products.segmentation_policy[product], even_or_odd,
        stage_end - stage_start, maximum, minimum, products.trim[product]
      );
      if (count >= 0 && count < static_cast<int>(logs.size())) {
        consider(
          tree, product, start, logs[static_cast<std::size_t>(count)], 0,
          NA_REAL, segment_end, candidates, has_length, non_diameter
        );
      }
      return candidates;
    }
    if (products.length_mode[product] == 0) {
      for (int at = products.length_offsets[product];
           at < products.length_offsets[product + 1]; ++at) {
        consider(
          tree, product, start, products.length_ticks[at] * quantum, 0,
          NA_REAL, segment_end, candidates, has_length, non_diameter
        );
      }
    } else {
      const int available = static_cast<int>(std::floor(
        (segment_end - start - products.trim[product]) / quantum
      ));
      int largest = available;
      if (products.max_tick[product] >= 0) {
        largest = std::min(largest, products.max_tick[product]);
      }
      for (int tick = products.min_tick[product]; tick <= largest;
           tick += products.step_tick[product]) {
        const double length = tick * quantum;
        if (!parity_ok(length, products.parity[product])) continue;
        consider(
          tree, product, start, length, 0, NA_REAL, segment_end,
          candidates, has_length, non_diameter
        );
      }
    }
    if (finite_limit(products.min_top[product])) {
      const int key = tree * n_product + product;
      double upper = std::numeric_limits<double>::infinity();
      if (products.length_mode[product] == 0 &&
          products.length_offsets[product] <
            products.length_offsets[product + 1]) {
        upper = products.length_ticks[
          products.length_offsets[product + 1] - 1
        ] * quantum;
      } else if (products.max_tick[product] >= 0) {
        upper = products.max_tick[product] * quantum;
      }
      for (int at = boundary_offsets[key]; at < boundary_offsets[key + 1]; ++at) {
        const double boundary = boundaries[at];
        if (boundary <= start || boundary > segment_end) continue;
        const double length = boundary - start - products.trim[product];
        if (length < products.min_top[product] || length > upper) continue;
        consider(
          tree, product, start, length, 1, boundary, segment_end,
          candidates, has_length, non_diameter
        );
      }
    }
    std::sort(candidates.begin(), candidates.end(), [](const Candidate& left,
                                                        const Candidate& right) {
      if (left.end != right.end) return left.end < right.end;
      if (left.kind != right.kind) return left.kind < right.kind;
      return left.length < right.length;
    });
    candidates.erase(
      std::unique(
        candidates.begin(), candidates.end(),
        [](const Candidate& left, const Candidate& right) {
          return left.end == right.end && left.kind == right.kind &&
            left.length == right.length;
        }
      ),
      candidates.end()
    );
    return candidates;
  }

  double minimum_physical(int product) const {
    double minimum = std::numeric_limits<double>::infinity();
    if (products.length_mode[product] == 0 &&
        products.length_offsets[product] <
          products.length_offsets[product + 1]) {
      minimum = products.length_ticks[
        products.length_offsets[product]
      ] * quantum + products.trim[product];
    } else if (products.length_mode[product] == 1) {
      minimum = products.min_tick[product] * quantum + products.trim[product];
    }
    if (finite_limit(products.min_top[product])) {
      minimum = std::min(
        minimum, products.min_top[product] + products.trim[product]
      );
    }
    return minimum;
  }

  int remainder_cause(int tree, int chain_begin, int stage,
                      double cursor, double segment_end) const {
    double minimum = std::numeric_limits<double>::infinity();
    bool any_length = false;
    bool any_non_diameter = false;
    bool any_feasible = false;
    const int chain_size = chain_offsets[tree + 1] - chain_begin;
    for (int current = stage; current < chain_size; ++current) {
      const int product = chain_product[chain_begin + current];
      minimum = std::min(minimum, minimum_physical(product));
      const std::vector<Candidate> candidates = enumerate(
        tree, product, cursor, segment_end, 0, cursor,
        &any_length, &any_non_diameter
      );
      if (!candidates.empty()) any_feasible = true;
      if (products.allow_lower_products[product] == 0) break;
    }
    if (segment_end - cursor < minimum) return 1;
    if (any_length && !any_feasible && !any_non_diameter) return 2;
    return 3;
  }

  double gross_scale(int tree, int product, const Candidate& candidate) const {
    const double start = profile.height[candidate.start_profile];
    const double nominal_end = start + candidate.length;
    const int nominal_at = profile.find(tree, nominal_end);
    if (nominal_at < 0) return NA_REAL;
    const int basis = products.scale_basis[product];
    const double cubic = basis == 0
      ? profile.cum_ib[nominal_at] - profile.cum_ib[candidate.start_profile]
      : profile.cum_ob[nominal_at] - profile.cum_ob[candidate.start_profile];
    const int rule = products.scale_rule[product];
    double gross = 0;
    if (rule == 10) {
      gross = cubic;
    } else if (rule == 8) {
      const double length = round_dimension(
        candidate.length, products.length_round[product], 1, units
      );
      const double led_raw = basis == 0
        ? profile.dib[candidate.start_profile]
        : profile.dob[candidate.start_profile];
      const double sed_raw = basis == 0
        ? profile.dib[nominal_at]
        : profile.dob[nominal_at];
      const double led = round_dimension(
        led_raw, products.diameter_round[product], 0, units
      );
      const double sed = round_dimension(
        sed_raw, products.diameter_round[product], 0, units
      );
      const double divisor = units == 1 ? 12.0 : 100.0;
      const double led_area = std::acos(-1.0) *
        std::pow(led / divisor, 2.0) / 4.0;
      const double sed_area = std::acos(-1.0) *
        std::pow(sed / divisor, 2.0) / 4.0;
      gross = length * (led_area + sed_area) / 2.0;
    } else if (rule == 9) {
      const double length = round_dimension(
        candidate.length, products.length_round[product], 1, units
      );
      const double midpoint = start + candidate.length / 2.0;
      const int midpoint_at = profile.find(tree, midpoint);
      if (midpoint_at < 0) return NA_REAL;
      const double raw = basis == 0
        ? profile.dib[midpoint_at]
        : profile.dob[midpoint_at];
      const double diameter = round_dimension(
        raw, products.diameter_round[product], 0, units
      );
      const double divisor = units == 1 ? 12.0 : 100.0;
      gross = length * std::acos(-1.0) *
        std::pow(diameter / divisor, 2.0) / 4.0;
    } else if (rule == 7) {
      const double raw = profile.dib[nominal_at];
      const double sed = round_dimension(
        raw, products.diameter_round[product], 0, units
      );
      const double sed_in = units == 1 ? sed : sed / 2.54;
      const double raw_length = round_dimension(
        candidate.length, products.length_round[product], 1, units
      );
      const double length_ft = units == 1 ? raw_length : raw_length / 0.3048;
      gross = std::pow(std::max(sed_in - 4.0, 0.0), 2.0) *
        length_ft / 16.0;
    } else if (rule == 6) {
      const double sed = round_dimension(
        profile.dib[nominal_at], products.diameter_round[product], 0, units
      );
      const double sed_in = std::floor(
        (units == 1 ? sed : sed / 2.54) + 0.5
      );
      const double length_ft = units == 1
        ? candidate.length
        : candidate.length / 0.3048;
      gross = international(products, sed_in, length_ft);
    } else {
      const bool corrected = rule <= 2;
      const int convention = rule % 3;
      const int option = convention == 0 ? 12 : 22;
      const int even_or_odd = convention == 0 ? 1 : 2;
      const double maximum = convention == 2 ? 40.0 : 20.0;
      const double length_ft = units == 1
        ? candidate.length
        : candidate.length / 0.3048;
      const std::vector<double> segments = nvel_segments(
        option, even_or_odd, length_ft, maximum, 2.0, 0.0
      );
      double cumulative = 0;
      for (double segment : segments) {
        cumulative += segment;
        const double height = start + cumulative *
          (units == 1 ? 1.0 : 0.3048);
        const int at = profile.find(tree, height);
        if (at < 0) return NA_REAL;
        const double modeled = round_dimension(
          profile.dib[at], products.diameter_round[product], 0, units
        );
        const double diameter = std::floor(
          (units == 1 ? modeled : modeled / 2.54) + 0.5
        );
        gross += scribner(products, diameter, segment, corrected);
      }
    }
    gross = round_dimension(
      gross, products.volume_round[product], 2, units
    );
    const int quantity = products.measurement_quantity[product];
    if (quantity == 1) {
      const int target = products.scale_unit[product];
      if (units == 1 && target == 1) gross *= 0.028316846592;
      if (units == 2 && target == 0) gross /= 0.028316846592;
    } else if (quantity == 2) {
      gross *= weight_factor[tree * n_product + product];
    } else if (quantity == 3) {
      const double solid_ft3 = units == 1
        ? gross
        : gross / 0.028316846592;
      gross = solid_ft3 / (128.0 * products.cord_fraction[product]);
    }
    return gross;
  }

  Arc make_arc(int tree, int product, int stage,
               const Candidate& candidate) const {
    Arc arc;
    arc.product = product;
    arc.stage = stage;
    arc.priority = products.priority[product];
    arc.candidate = candidate;
    const double gross = profile.cum_ib[candidate.end_profile] -
      profile.cum_ib[candidate.start_profile];
    arc.physical_net = gross - candidate.located;
    if (objective == 1) {
      const double ratio = gross > 0 ? candidate.located / gross : 0;
      const double sale = gross_scale(tree, product, candidate) * (1.0 - ratio);
      arc.reward = sale * products.price[product] /
        products.price_quantity[product];
    } else {
      arc.reward = arc.physical_net;
    }
    return arc;
  }

  std::string serialized_arc(const Arc& arc) const {
    std::ostringstream stream;
    stream << std::hexfloat
           << profile.height[arc.candidate.start_profile] << '|'
           << arc.stage << '|' << arc.candidate.end << '|'
           << arc.candidate.kind << '|' << arc.candidate.length << '|'
           << products.id[arc.product];
    return stream.str();
  }

  void evaluate_path(Path& path) const {
    path.objective = 0;
    double located = 0;
    for (const Arc& arc : path.arcs) {
      path.objective += arc.reward;
      located += arc.candidate.located;
    }
    path.physical_net = 0;
    if (!path.arcs.empty()) {
      const Candidate& first = path.arcs.front().candidate;
      const Candidate& last = path.arcs.back().candidate;
      path.physical_net = profile.cum_ib[last.end_profile] -
        profile.cum_ib[first.start_profile] - located;
    }
    if (objective == 0) {
      path.objective = path.physical_net;
    }
    if (!std::isfinite(path.objective) || !std::isfinite(path.physical_net)) {
      path.valid = false;
    }
  }

  Path prepend(const Arc& arc, const Path& suffix) const {
    Path result = suffix;
    result.arcs.insert(result.arcs.begin(), arc);
    evaluate_path(result);
    return result;
  }

  bool better(const Path& left, const Path& right) const {
    if (!left.valid) return false;
    if (!right.valid) return true;
    if (left.objective != right.objective) {
      return left.objective > right.objective;
    }
    if (left.physical_net != right.physical_net) {
      return left.physical_net > right.physical_net;
    }
    if (left.arcs.size() != right.arcs.size()) {
      return left.arcs.size() < right.arcs.size();
    }
    for (std::size_t i = 0; i < left.arcs.size(); ++i) {
      if (left.arcs[i].priority != right.arcs[i].priority) {
        return left.arcs[i].priority < right.arcs[i].priority;
      }
    }
    for (std::size_t i = 0; i < left.arcs.size(); ++i) {
      if (left.arcs[i].candidate.length != right.arcs[i].candidate.length) {
        return left.arcs[i].candidate.length >
          right.arcs[i].candidate.length;
      }
    }
    for (std::size_t i = 0; i < left.arcs.size(); ++i) {
      if (left.arcs[i].candidate.end != right.arcs[i].candidate.end) {
        return left.arcs[i].candidate.end < right.arcs[i].candidate.end;
      }
    }
    for (std::size_t i = 0; i < left.arcs.size(); ++i) {
      const std::string left_key = serialized_arc(left.arcs[i]);
      const std::string right_key = serialized_arc(right.arcs[i]);
      if (left_key != right_key) return left_key < right_key;
    }
    return false;
  }

  void append_piece(TreeResult& result, int tree, int segment_number,
                    int log_number, const Arc& arc) const {
    const Candidate& candidate = arc.candidate;
    const int start_at = candidate.start_profile;
    const int end_at = candidate.end_profile;
    const double start = profile.height[start_at];
    result.logs.push_back(PieceRow{
      tree, log_number, segment_number, arc.stage + 1, arc.product,
      candidate.kind, start, start + candidate.length, candidate.end,
      candidate.length, candidate.length + products.trim[arc.product],
      products.trim[arc.product], profile.dib[start_at], profile.dib[end_at],
      profile.dob[start_at], profile.dob[end_at],
      profile.cum_ib[end_at] - profile.cum_ib[start_at],
      profile.cum_ob[end_at] - profile.cum_ob[start_at],
      candidate.located, candidate.rot_pct,
      gross_scale(tree, arc.product, candidate)
    });
  }

  void append_residual(TreeResult& result, int tree, int segment_number,
                       int cause, double from, double to) const {
    if (!(from < to)) return;
    const int from_at = profile.find(tree, from);
    const int to_at = profile.find(tree, to);
    const double volume = from_at >= 0 && to_at >= 0
      ? profile.cum_ib[to_at] - profile.cum_ib[from_at]
      : NA_REAL;
    result.residuals.push_back(ResidualRow{
      tree, segment_number, cause, from, to, volume
    });
  }
};

struct StateKey {
  int node;
  int stage;
  int count;

  bool operator==(const StateKey& other) const {
    return node == other.node && stage == other.stage && count == other.count;
  }
};

struct StateHash {
  std::size_t operator()(const StateKey& state) const {
    std::size_t value = static_cast<std::size_t>(state.node);
    value ^= static_cast<std::size_t>(state.stage + 0x9e3779b9) +
      (value << 6) + (value >> 2);
    value ^= static_cast<std::size_t>(state.count + 0x9e3779b9) +
      (value << 6) + (value >> 2);
    return value;
  }
};

class DynamicProgram {
 public:
  const BuckingCore& core;
  const int tree;
  const int chain_begin;
  const int chain_size;
  const double segment_end;
  std::unordered_map<StateKey, Path, StateHash> memo;

  DynamicProgram(const BuckingCore& core_, int tree_, double segment_end_)
    : core(core_), tree(tree_), chain_begin(core.chain_offsets[tree_]),
      chain_size(core.chain_offsets[tree_ + 1] - chain_begin),
      segment_end(segment_end_) {}

  Path terminal(int node, int stage) const {
    Path path;
    const double cursor = core.profile.height[node];
    if (cursor < segment_end) {
      path.residual_from = cursor;
      path.residual_cause = core.remainder_cause(
        tree, chain_begin, std::min(stage, chain_size - 1),
        cursor, segment_end
      );
    }
    return path;
  }

  Path solve_nvel(int node, int stage) {
    const int product = core.chain_product[chain_begin + stage];
    const double stage_start = core.profile.height[node];
    const int limit = core.products.max_logs_per_segment[product];
    std::vector<Arc> prefix;
    int cursor_node = node;
    int count = 0;
    while (core.profile.height[cursor_node] < segment_end &&
           (limit < 0 || count < limit)) {
      const std::vector<Candidate> candidates = core.enumerate(
        tree, product, core.profile.height[cursor_node], segment_end,
        count, stage_start
      );
      if (candidates.empty()) break;
      const Arc arc = core.make_arc(tree, product, stage, candidates.front());
      if (!std::isfinite(arc.reward) || !std::isfinite(arc.physical_net)) {
        Path invalid;
        invalid.valid = false;
        return invalid;
      }
      prefix.push_back(arc);
      cursor_node = arc.candidate.end_profile;
      ++count;
    }
    Path suffix;
    if (core.profile.height[cursor_node] >= segment_end) {
      suffix = Path();
    } else if (core.products.allow_lower_products[product] != 0) {
      suffix = solve(cursor_node, stage + 1, 0);
    } else {
      suffix = terminal(cursor_node, stage);
    }
    if (!suffix.valid) return suffix;
    prefix.insert(prefix.end(), suffix.arcs.begin(), suffix.arcs.end());
    suffix.arcs = std::move(prefix);
    core.evaluate_path(suffix);
    return suffix;
  }

  Path solve(int node, int stage, int count) {
    if (core.profile.height[node] >= segment_end) return Path();
    if (stage >= chain_size) return terminal(node, chain_size - 1);
    const int product = core.chain_product[chain_begin + stage];
    if (core.products.segmentation_policy[product] != 0) {
      return solve_nvel(node, stage);
    }
    const int normalized_count = core.products.max_logs_per_segment[product] < 0
      ? 0
      : count;
    const StateKey key{node, stage, normalized_count};
    const auto known = memo.find(key);
    if (known != memo.end()) return known->second;
    const int limit = core.products.max_logs_per_segment[product];
    std::vector<Candidate> candidates;
    if (limit < 0 || normalized_count < limit) {
      candidates = core.enumerate(
        tree, product, core.profile.height[node], segment_end
      );
    }
    Path best;
    best.valid = false;
    if (!candidates.empty()) {
      for (const Candidate& candidate : candidates) {
        const Arc arc = core.make_arc(tree, product, stage, candidate);
        if (!std::isfinite(arc.reward) || !std::isfinite(arc.physical_net)) {
          continue;
        }
        const int next_count = limit < 0 ? 0 : normalized_count + 1;
        const Path suffix = solve(
          candidate.end_profile, stage, next_count
        );
        const Path path = core.prepend(arc, suffix);
        if (core.better(path, best)) best = path;
      }
    } else if (core.products.allow_lower_products[product] != 0) {
      best = solve(node, stage + 1, 0);
    } else {
      best = terminal(node, stage);
    }
    memo.emplace(key, best);
    return best;
  }
};

class BuckingWorker : public RcppParallel::Worker {
 public:
  const BuckingCore& core;
  const int algorithm;
  std::vector<TreeResult>& results;

  BuckingWorker(const BuckingCore& core_, int algorithm_,
                std::vector<TreeResult>& results_)
    : core(core_), algorithm(algorithm_), results(results_) {}

  void cascade_tree(std::size_t tree) const {
    TreeResult& result = results[tree];
    int log_number = 0;
    const int chain_begin = core.chain_offsets[tree];
    const int chain_size = core.chain_offsets[tree + 1] - chain_begin;
    if (chain_size == 0) return;
    int segment_number = 0;
    for (int segment = core.segment_offsets[tree];
         segment < core.segment_offsets[tree + 1]; ++segment) {
      ++segment_number;
      double cursor = core.segment_lo[segment];
      const double segment_end = core.segment_hi[segment];
      int stage = 0;
      int count = 0;
      double stage_start = cursor;
      while (cursor < segment_end && stage < chain_size) {
        const int product = core.chain_product[chain_begin + stage];
        const int limit = core.products.max_logs_per_segment[product];
        std::vector<Candidate> candidates;
        if (limit < 0 || count < limit) {
          candidates = core.enumerate(
            static_cast<int>(tree), product, cursor, segment_end,
            count, stage_start
          );
        }
        if (!candidates.empty()) {
          const Candidate* best = &candidates.front();
          for (const Candidate& candidate : candidates) {
            if (candidate.length > best->length ||
                (candidate.length == best->length &&
                 (candidate.end < best->end ||
                  (candidate.end == best->end &&
                   candidate.kind < best->kind)))) {
              best = &candidate;
            }
          }
          ++log_number;
          core.append_piece(
            result, static_cast<int>(tree), segment_number, log_number,
            core.make_arc(static_cast<int>(tree), product, stage, *best)
          );
          cursor = best->end;
          ++count;
        } else if (core.products.allow_lower_products[product] != 0) {
          ++stage;
          count = 0;
          stage_start = cursor;
        } else {
          break;
        }
      }
      if (cursor < segment_end) {
        core.append_residual(
          result, static_cast<int>(tree), segment_number,
          core.remainder_cause(
            static_cast<int>(tree), chain_begin,
            std::min(stage, chain_size - 1), cursor, segment_end
          ),
          cursor, segment_end
        );
      }
    }
  }

  void dp_tree(std::size_t tree) const {
    TreeResult& result = results[tree];
    int log_number = 0;
    int segment_number = 0;
    for (int segment = core.segment_offsets[tree];
         segment < core.segment_offsets[tree + 1]; ++segment) {
      ++segment_number;
      const double start = core.segment_lo[segment];
      const double segment_end = core.segment_hi[segment];
      const int start_at = core.profile.find(static_cast<int>(tree), start);
      if (start_at < 0) {
        result.status = 412;
        continue;
      }
      DynamicProgram solver(core, static_cast<int>(tree), segment_end);
      const Path path = solver.solve(start_at, 0, 0);
      if (!path.valid) {
        result.status = 412;
        continue;
      }
      for (const Arc& arc : path.arcs) {
        ++log_number;
        core.append_piece(
          result, static_cast<int>(tree), segment_number, log_number, arc
        );
      }
      if (path.residual_cause != 0) {
        core.append_residual(
          result, static_cast<int>(tree), segment_number,
          path.residual_cause, path.residual_from, segment_end
        );
      }
    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t tree = begin; tree < end; ++tree) {
      if (core.tree_status[tree] != 0) continue;
      if (algorithm == 0) cascade_tree(tree);
      else dp_tree(tree);
    }
  }
};

struct SimplePlanTree {
  std::vector<double> heights;
  int status = 0;
};

class SimplePlanWorker : public RcppParallel::Worker {
 public:
  const ProductData& products;
  const double quantum;
  const Rcpp::IntegerVector tree_status;
  const Rcpp::IntegerVector chain_offsets;
  const Rcpp::IntegerVector chain_product;
  const Rcpp::NumericVector stump;
  const Rcpp::NumericVector top;
  const Rcpp::NumericVector ht;
  const std::vector<std::size_t>& representative;
  std::vector<SimplePlanTree>& results;

  SimplePlanWorker(
      const ProductData& products_, double quantum_,
      const Rcpp::IntegerVector& tree_status_,
      const Rcpp::IntegerVector& chain_offsets_,
      const Rcpp::IntegerVector& chain_product_,
      const Rcpp::NumericVector& stump_, const Rcpp::NumericVector& top_,
      const Rcpp::NumericVector& ht_,
      const std::vector<std::size_t>& representative_,
      std::vector<SimplePlanTree>& results_)
    : products(products_), quantum(quantum_), tree_status(tree_status_),
      chain_offsets(chain_offsets_), chain_product(chain_product_),
      stump(stump_), top(top_), ht(ht_), representative(representative_),
      results(results_) {}

  void add_ordinary(int product, double start, double segment_end,
                    std::vector<double>* additions,
                    std::vector<double>* scale_points) const {
    const double trim = products.trim[product];
    if (products.length_mode[product] == 0) {
      for (int at = products.length_offsets[product];
           at < products.length_offsets[product + 1]; ++at) {
        const double length = products.length_ticks[at] * quantum;
        const double nominal_end = start + length;
        const double end = nominal_end + trim;
        if (end <= segment_end && end > start) {
          additions->push_back(end);
          scale_points->push_back(start);
          scale_points->push_back(nominal_end);
          scale_points->push_back(end);
        }
      }
      return;
    }
    int largest = static_cast<int>(std::floor(
      (segment_end - start - trim) / quantum
    ));
    if (products.max_tick[product] >= 0) {
      largest = std::min(largest, products.max_tick[product]);
    }
    for (int tick = products.min_tick[product]; tick <= largest;
         tick += products.step_tick[product]) {
      const double length = tick * quantum;
      if (!parity_ok(length, products.parity[product])) continue;
      const double nominal_end = start + length;
      const double end = nominal_end + trim;
      if (end <= segment_end && end > start) {
        additions->push_back(end);
        scale_points->push_back(start);
        scale_points->push_back(nominal_end);
        scale_points->push_back(end);
      }
    }
  }

  void add_boundary(int product, double start, double segment_end,
                    std::vector<double>* additions,
                    std::vector<double>* scale_points) const {
    if (!finite_limit(products.min_top[product])) return;
    const double length = segment_end - start - products.trim[product];
    double upper = std::numeric_limits<double>::infinity();
    if (products.length_mode[product] == 0 &&
        products.length_offsets[product] < products.length_offsets[product + 1]) {
      upper = products.length_ticks[
        products.length_offsets[product + 1] - 1
      ] * quantum;
    } else if (products.max_tick[product] >= 0) {
      upper = products.max_tick[product] * quantum;
    }
    if (!(length > 0.0) || length < products.min_top[product] ||
        length > upper) return;
    additions->push_back(segment_end);
    scale_points->push_back(start);
    scale_points->push_back(start + length);
    scale_points->push_back(segment_end);
  }

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t tree = begin; tree < end; ++tree) {
      if (representative[tree] != tree) continue;
      SimplePlanTree& result = results[tree];
      result.status = tree_status[tree];
      std::vector<double>& heights = result.heights;
      heights.push_back(0.0);
      if (std::isfinite(stump[tree])) heights.push_back(stump[tree]);
      if (std::isfinite(top[tree])) heights.push_back(top[tree]);
      if (std::isfinite(ht[tree])) heights.push_back(ht[tree]);
      if (result.status != 0 || !(top[tree] > stump[tree])) {
        std::sort(heights.begin(), heights.end());
        heights.erase(std::unique(heights.begin(), heights.end()), heights.end());
        continue;
      }
      std::vector<double> nodes{stump[tree]};
      std::vector<double> scale_points;
      std::size_t cursor = 0;
      while (cursor < nodes.size()) {
        const double start = nodes[cursor];
        std::vector<double> additions;
        for (int at = chain_offsets[tree]; at < chain_offsets[tree + 1]; ++at) {
          const int product = chain_product[at];
          add_ordinary(product, start, top[tree], &additions, &scale_points);
          add_boundary(product, start, top[tree], &additions, &scale_points);
        }
        additions.erase(
          std::remove_if(
            additions.begin(), additions.end(),
            [start, this, tree](double value) {
              return !(value > start) || value > top[tree];
            }
          ),
          additions.end()
        );
        if (!additions.empty()) {
          nodes.insert(nodes.end(), additions.begin(), additions.end());
          std::sort(nodes.begin(), nodes.end());
          nodes.erase(std::unique(nodes.begin(), nodes.end()), nodes.end());
          if (nodes.size() > 50000) {
            result.status = 412;
            break;
          }
        }
        ++cursor;
      }
      heights.insert(heights.end(), nodes.begin(), nodes.end());
      heights.insert(heights.end(), scale_points.begin(), scale_points.end());
      heights.erase(
        std::remove_if(
          heights.begin(), heights.end(),
          [this, tree](double value) {
            return !std::isfinite(value) || value < 0.0 || value > ht[tree];
          }
        ),
        heights.end()
      );
      std::sort(heights.begin(), heights.end());
      heights.erase(std::unique(heights.begin(), heights.end()), heights.end());
    }
  }
};

}  // namespace

// [[Rcpp::export]]
Rcpp::List mc_simple_plan_cpp(
    Rcpp::List products, double quantum, Rcpp::IntegerVector tree_status,
    Rcpp::IntegerVector chain_offsets, Rcpp::IntegerVector chain_product,
    Rcpp::NumericVector stump, Rcpp::NumericVector top,
    Rcpp::NumericVector ht, int threads) {
  const std::size_t n_tree = tree_status.size();
  if (stump.size() != static_cast<R_xlen_t>(n_tree) ||
      top.size() != static_cast<R_xlen_t>(n_tree) ||
      ht.size() != static_cast<R_xlen_t>(n_tree) ||
      chain_offsets.size() != static_cast<R_xlen_t>(n_tree + 1)) {
    Rcpp::stop("Simple-plan columns have inconsistent sizes.");
  }
  const ProductData product_data(products);
  auto bits = [](double value) {
    std::uint64_t output = 0;
    std::memcpy(&output, &value, sizeof(output));
    return output;
  };
  auto hash_mix = [](std::uint64_t hash, std::uint64_t value) {
    return hash ^ (value + UINT64_C(0x9e3779b97f4a7c15) +
      (hash << 6) + (hash >> 2));
  };
  auto equal_geometry = [&](std::size_t left, std::size_t right) {
    if (tree_status[left] != tree_status[right] ||
        stump[left] != stump[right] || top[left] != top[right] ||
        ht[left] != ht[right] ||
        chain_offsets[left + 1] - chain_offsets[left] !=
          chain_offsets[right + 1] - chain_offsets[right]) return false;
    const int count = chain_offsets[left + 1] - chain_offsets[left];
    for (int offset = 0; offset < count; ++offset) {
      if (chain_product[chain_offsets[left] + offset] !=
          chain_product[chain_offsets[right] + offset]) return false;
    }
    return true;
  };
  std::vector<std::size_t> representative(n_tree);
  std::unordered_map<std::uint64_t, std::vector<std::size_t>> geometry;
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    std::uint64_t hash = UINT64_C(0xcbf29ce484222325);
    hash = hash_mix(hash, static_cast<std::uint64_t>(tree_status[tree]));
    hash = hash_mix(hash, bits(stump[tree]));
    hash = hash_mix(hash, bits(top[tree]));
    hash = hash_mix(hash, bits(ht[tree]));
    for (int at = chain_offsets[tree]; at < chain_offsets[tree + 1]; ++at) {
      hash = hash_mix(hash, static_cast<std::uint64_t>(chain_product[at] + 1));
    }
    std::vector<std::size_t>& candidates = geometry[hash];
    representative[tree] = tree;
    for (std::size_t candidate : candidates) {
      if (equal_geometry(tree, candidate)) {
        representative[tree] = candidate;
        break;
      }
    }
    if (representative[tree] == tree) candidates.push_back(tree);
  }
  std::vector<SimplePlanTree> results(n_tree);
  SimplePlanWorker worker(
    product_data, quantum, tree_status, chain_offsets, chain_product,
    stump, top, ht, representative, results
  );
  RcppParallel::parallelFor(0, n_tree, worker, 1, threads);

  Rcpp::IntegerVector output_status(n_tree), segment_offsets(n_tree + 1),
    profile_offsets(n_tree + 1);
  std::size_t segment_count = 0;
  std::size_t profile_count = 0;
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    const SimplePlanTree& plan = results[representative[tree]];
    output_status[tree] = plan.status;
    if (plan.status == 0 && top[tree] > stump[tree]) ++segment_count;
    segment_offsets[tree + 1] = static_cast<int>(segment_count);
    profile_count += plan.heights.size();
    profile_offsets[tree + 1] = static_cast<int>(profile_count);
  }
  Rcpp::NumericVector segment_lo(segment_count), segment_hi(segment_count),
    profile_height(profile_count);
  std::size_t segment_at = 0;
  std::size_t profile_at = 0;
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    const SimplePlanTree& plan = results[representative[tree]];
    if (plan.status == 0 && top[tree] > stump[tree]) {
      segment_lo[segment_at] = stump[tree];
      segment_hi[segment_at] = top[tree];
      ++segment_at;
    }
    for (double value : plan.heights) {
      profile_height[profile_at++] = value;
    }
  }
  const std::size_t n_product = static_cast<std::size_t>(product_data.size());
  Rcpp::IntegerVector boundary_offsets(n_tree * n_product + 1);
  std::size_t boundary_count = 0;
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    for (std::size_t product = 0; product < n_product; ++product) {
      bool active = false;
      for (int at = chain_offsets[tree]; at < chain_offsets[tree + 1]; ++at) {
        if (chain_product[at] == static_cast<int>(product)) {
          active = true;
          break;
        }
      }
      if (active && output_status[tree] == 0) ++boundary_count;
      boundary_offsets[tree * n_product + product + 1] =
        static_cast<int>(boundary_count);
    }
  }
  Rcpp::NumericVector boundaries(boundary_count);
  std::size_t boundary_at = 0;
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    for (std::size_t product = 0; product < n_product; ++product) {
      const int key = static_cast<int>(tree * n_product + product);
      if (boundary_offsets[key + 1] > boundary_offsets[key]) {
        boundaries[boundary_at++] = top[tree];
      }
    }
  }
  return Rcpp::List::create(
    Rcpp::_["status"] = output_status,
    Rcpp::_["segment_offsets"] = segment_offsets,
    Rcpp::_["segment_lo"] = segment_lo,
    Rcpp::_["segment_hi"] = segment_hi,
    Rcpp::_["boundary_offsets"] = boundary_offsets,
    Rcpp::_["boundaries"] = boundaries,
    Rcpp::_["profile_offsets"] = profile_offsets,
    Rcpp::_["profile_height"] = profile_height
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector mc_profile_lookup_cpp(
    Rcpp::IntegerVector offsets, Rcpp::NumericVector height,
    Rcpp::IntegerVector tree, Rcpp::NumericVector target,
    Rcpp::NumericVector value) {
  if (tree.size() != target.size() || height.size() != value.size()) {
    Rcpp::stop("Profile lookup columns have inconsistent sizes.");
  }
  Rcpp::NumericVector result(tree.size(), NA_REAL);
  for (R_xlen_t row = 0; row < tree.size(); ++row) {
    const int current = tree[row] - 1;
    if (current < 0 || current + 1 >= offsets.size()) {
      Rcpp::stop("Profile lookup tree index is out of range.");
    }
    int left = offsets[current];
    int right = offsets[current + 1];
    while (left < right) {
      const int middle = left + (right - left) / 2;
      if (height[middle] < target[row]) left = middle + 1;
      else right = middle;
    }
    if (left < offsets[current + 1] && height[left] == target[row]) {
      result[row] = value[left];
    }
  }
  return result;
}

// [[Rcpp::export]]
Rcpp::List mc_buck_cpp(
    Rcpp::List products, double quantum, int units, int algorithm,
    int objective, Rcpp::IntegerVector tree_status,
    Rcpp::IntegerVector chain_offsets, Rcpp::IntegerVector chain_product,
    Rcpp::IntegerVector segment_offsets, Rcpp::NumericVector segment_lo,
    Rcpp::NumericVector segment_hi, Rcpp::IntegerVector boundary_offsets,
    Rcpp::NumericVector boundaries, Rcpp::IntegerVector profile_offsets,
    Rcpp::NumericVector profile_height, Rcpp::NumericVector dib,
    Rcpp::NumericVector dob, Rcpp::NumericVector cum_ib,
    Rcpp::NumericVector cum_ob, Rcpp::IntegerVector defect_offsets,
    Rcpp::NumericVector defect_from, Rcpp::NumericVector defect_to,
    Rcpp::IntegerVector defect_effect, Rcpp::NumericVector defect_percent,
    Rcpp::IntegerVector defect_category_rank,
    Rcpp::NumericVector weight_factor, int threads) {
  const ProfileClock::time_point begin = ProfileClock::now();
  const std::size_t n_tree = tree_status.size();
  std::vector<TreeResult> results(n_tree);
  const BuckingCore core(
    products, quantum, units, objective, tree_status, chain_offsets,
    chain_product, segment_offsets, segment_lo, segment_hi, boundary_offsets,
    boundaries, profile_offsets, profile_height, dib, dob, cum_ib, cum_ob,
    defect_offsets, defect_from, defect_to, defect_effect, defect_percent,
    defect_category_rank, weight_factor
  );
  const ProfileClock::time_point decoded = ProfileClock::now();
  BuckingWorker worker(core, algorithm, results);
  RcppParallel::parallelFor(0, n_tree, worker, 1, threads);
  const ProfileClock::time_point bucked = ProfileClock::now();

  std::size_t n_piece = 0;
  std::size_t n_residual = 0;
  Rcpp::IntegerVector output_status(n_tree);
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    n_piece += results[tree].logs.size();
    n_residual += results[tree].residuals.size();
    output_status[tree] = results[tree].status;
  }
  Rcpp::IntegerVector log_tree(n_piece), log_number(n_piece),
    log_segment(n_piece), log_stage(n_piece), log_product(n_piece),
    log_kind(n_piece);
  Rcpp::NumericVector start(n_piece), nominal_end(n_piece), cut_end(n_piece),
    nominal_length(n_piece), physical_length(n_piece), trim(n_piece),
    led_ib(n_piece), sed_ib(n_piece), led_ob(n_piece), sed_ob(n_piece),
    gross_ib(n_piece), gross_ob(n_piece), located(n_piece), rot_pct(n_piece),
    gross_scale(n_piece);
  std::size_t at = 0;
  for (const TreeResult& result : results) {
    for (const PieceRow& row : result.logs) {
      log_tree[at] = row.tree + 1;
      log_number[at] = row.log;
      log_segment[at] = row.segment;
      log_stage[at] = row.stage;
      log_product[at] = row.product + 1;
      log_kind[at] = row.kind;
      start[at] = row.start;
      nominal_end[at] = row.nominal_end;
      cut_end[at] = row.end;
      nominal_length[at] = row.nominal_length;
      physical_length[at] = row.physical_length;
      trim[at] = row.trim;
      led_ib[at] = row.led_ib;
      sed_ib[at] = row.sed_ib;
      led_ob[at] = row.led_ob;
      sed_ob[at] = row.sed_ob;
      gross_ib[at] = row.gross_ib;
      gross_ob[at] = row.gross_ob;
      located[at] = row.located;
      rot_pct[at] = row.rot_pct;
      gross_scale[at] = row.gross_scale;
      ++at;
    }
  }
  Rcpp::IntegerVector residual_tree(n_residual), residual_segment(n_residual),
    residual_cause(n_residual);
  Rcpp::NumericVector residual_from(n_residual), residual_to(n_residual),
    residual_volume(n_residual);
  at = 0;
  for (const TreeResult& result : results) {
    for (const ResidualRow& row : result.residuals) {
      residual_tree[at] = row.tree + 1;
      residual_segment[at] = row.segment;
      residual_cause[at] = row.cause;
      residual_from[at] = row.from;
      residual_to[at] = row.to;
      residual_volume[at] = row.volume;
      ++at;
    }
  }
  Rcpp::DataFrame log_output = Rcpp::DataFrame::create(
    Rcpp::_["tree"] = log_tree, Rcpp::_["log"] = log_number,
    Rcpp::_["segment"] = log_segment, Rcpp::_["stage"] = log_stage,
    Rcpp::_["product_index"] = log_product,
    Rcpp::_["length_kind"] = log_kind,
    Rcpp::_["start_height"] = start,
    Rcpp::_["nominal_end_height"] = nominal_end,
    Rcpp::_["end_height"] = cut_end,
    Rcpp::_["nominal_length"] = nominal_length,
    Rcpp::_["physical_length"] = physical_length,
    Rcpp::_["trim"] = trim,
    Rcpp::_["led_ib"] = led_ib, Rcpp::_["sed_ib"] = sed_ib,
    Rcpp::_["led_ob"] = led_ob, Rcpp::_["sed_ob"] = sed_ob,
    Rcpp::_["log_gross_cubic_ib"] = gross_ib,
    Rcpp::_["log_gross_cubic_ob"] = gross_ob,
    Rcpp::_["located_deduction_cubic_ib"] = located,
    Rcpp::_["rot_pct"] = rot_pct
  );
  const ProfileClock::time_point assembled = ProfileClock::now();
  return Rcpp::List::create(
    Rcpp::_["logs"] = log_output,
    Rcpp::_["native_gross_scale"] = gross_scale,
    Rcpp::_["residuals"] = Rcpp::DataFrame::create(
      Rcpp::_["tree"] = residual_tree,
      Rcpp::_["segment"] = residual_segment,
      Rcpp::_["cause"] = residual_cause,
      Rcpp::_["from"] = residual_from,
      Rcpp::_["to"] = residual_to,
      Rcpp::_["cubic_ib"] = residual_volume
    ),
    Rcpp::_["status"] = output_status,
    Rcpp::_["timing"] = Rcpp::NumericVector::create(
      Rcpp::_["input_decode"] = profile_seconds(begin, decoded),
      Rcpp::_["parallel_bucking"] = profile_seconds(decoded, bucked),
      Rcpp::_["columnar_assembly"] = profile_seconds(bucked, assembled)
    )
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector mc_nvel_log_scale_cpp(
    Rcpp::List products, Rcpp::IntegerVector product_index,
    Rcpp::NumericVector diameter, Rcpp::NumericVector length) {
  if (product_index.size() != diameter.size() ||
      product_index.size() != length.size()) {
    Rcpp::stop("Native log-scale columns have inconsistent sizes.");
  }
  const ProductData product_data(products);
  Rcpp::NumericVector output(product_index.size());
  for (R_xlen_t row = 0; row < product_index.size(); ++row) {
    const int product = product_index[row] - 1;
    if (product < 0 || product >= product_data.size()) {
      Rcpp::stop("Native log-scale product_index is out of range.");
    }
    if (!std::isfinite(diameter[row]) || !std::isfinite(length[row]) ||
        diameter[row] < 0 || length[row] < 0) {
      Rcpp::stop("Native log-scale measurements must be finite and nonnegative.");
    }
    const int rule = product_data.scale_rule[product];
    if (rule == 6) {
      output[row] = international(product_data, diameter[row], length[row]);
    } else if (rule >= 0 && rule <= 5) {
      output[row] = scribner(
        product_data, diameter[row], length[row], rule <= 2
      );
    } else {
      Rcpp::stop("Native log-scale products must use Scribner or International.");
    }
  }
  return output;
}

// [[Rcpp::export]]
Rcpp::NumericVector mc_round_dimension_cpp(
    Rcpp::NumericVector value, Rcpp::IntegerVector operation,
    Rcpp::IntegerVector dimension, Rcpp::IntegerVector units) {
  const R_xlen_t size = value.size();
  if (operation.size() != size || dimension.size() != size ||
      units.size() != size) {
    Rcpp::stop("Native rounding columns have inconsistent sizes.");
  }
  Rcpp::NumericVector output(size);
  for (R_xlen_t row = 0; row < size; ++row) {
    output[row] = round_dimension(
      value[row], operation[row], dimension[row], units[row]
    );
  }
  return output;
}
