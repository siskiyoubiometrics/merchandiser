#ifndef TREEVOLUME_SMALLTAPERS_HPP
#define TREEVOLUME_SMALLTAPERS_HPP

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include <treevolume/form_class_coefficients.hpp>
#include <treevolume/smalltapers_coefficients.hpp>

namespace treevolume {
namespace smalltapers {

namespace coefficient = smalltapers_coefficients;
namespace form_class = form_class_coefficients;

constexpr double kPiNvel = 3.1416;
constexpr double kSmalian = .00272708;

enum class Family { r1, r2, r5, r12, blm, behre };

struct Equation {
  Family family = Family::r1;
  std::string id;
  int species = 0;
  int forest = 0;
  bool three_point = false;
};

struct Auxiliary {
  double upper_ht1 = std::numeric_limits<double>::quiet_NaN();
  double upper_d1 = std::numeric_limits<double>::quiet_NaN();
  double form_class = std::numeric_limits<double>::quiet_NaN();
};

struct Result {
  double value = std::numeric_limits<double>::quiet_NaN();
  int status = 0;
};

inline int parse_digits(const std::string& value, std::size_t start,
                        std::size_t count) {
  if (value.size() < start + count) return -1;
  int result = 0;
  for (std::size_t index = start; index < start + count; ++index) {
    if (value[index] < '0' || value[index] > '9') return -1;
    result = result * 10 + value[index] - '0';
  }
  return result;
}

inline bool valid_r1_species(int species) {
  return species == 17 || species == 19 || species == 70 || species == 73 ||
      species == 108 || species == 119 || species == 122 || species == 202;
}

inline int r2_species_group(const Equation& equation) {
  if (equation.species == 746) return 0;
  if (equation.species == 108) return 1;
  if (equation.species == 122 && equation.forest == 3) return 2;
  if (equation.species == 122) return 3;
  if (equation.species == 15) return 4;
  if (equation.species == 19) return 5;
  if (equation.species == 93) return 6;
  if (equation.species == 202) return 7;
  return -1;
}

inline int r5_species_group(int species) {
  if (species == 202) return 0;
  if (species == 122) return 1;
  if (species == 117) return 2;
  if (species == 15) return 3;
  if (species == 20) return 4;
  if (species == 81) return 5;
  if (species == 116) return 6;
  if (species == 108) return 7;
  if (species == 211) return 8;
  return -1;
}

inline int r12_species_group(const Equation& equation) {
  if (equation.species == 301) return 0;
  if (equation.species == 671) return 1;
  if (equation.species == 510 && equation.id.compare(0, 3, "H00") == 0) return 2;
  if (equation.species == 510 && equation.id.compare(0, 3, "H01") == 0) return 3;
  return -1;
}

inline bool parse_equation(const std::string& id, Equation* equation) {
  if (id.size() != 10 || equation == nullptr || id[6] != 'W') return false;
  Equation parsed;
  parsed.id = id;
  parsed.species = parse_digits(id, 7, 3);
  parsed.forest = parse_digits(id, 1, 2);
  if (parsed.species < 0) return false;
  const std::string mnemonic = id.substr(3, 3);
  if (mnemonic == "JB2" && valid_r1_species(parsed.species)) {
    parsed.family = Family::r1;
  } else if ((mnemonic == "CZ2" || mnemonic == "CZ3")) {
    parsed.family = Family::r2;
    parsed.three_point = mnemonic == "CZ3";
    if (parsed.species == 122 && parsed.forest != 0 && parsed.forest != 3) {
      return false;
    }
    if (r2_species_group(parsed) < 0) return false;
  } else if (mnemonic == "WO2" && r5_species_group(parsed.species) >= 0) {
    parsed.family = Family::r5;
  } else if (mnemonic == "SN2" && r12_species_group(parsed) >= 0) {
    parsed.family = Family::r12;
  } else if (mnemonic == "BEH" && id[0] == 'B') {
    parsed.family = Family::blm;
  } else if (mnemonic == "BEH" &&
             (id.compare(0, 3, "616") == 0 ||
              id.compare(0, 3, "628") == 0 || id.compare(0, 3, "632") == 0)) {
    parsed.family = Family::behre;
  } else {
    return false;
  }
  *equation = parsed;
  return true;
}

inline int r1_forest_group(int forest) {
  int national_forest = forest;
  if (national_forest <= 1 || national_forest > 20) national_forest = 99;
  for (int index = 0; index < 20; ++index) {
    if (coefficient::r1_nnf[index] == national_forest) {
      return coefficient::r1_inf[index] - 1;
    }
  }
  return 9;
}

inline double r1_diameter(const Equation& equation, double dbh, double ht,
                          double height) {
  const int group = r1_forest_group(equation.forest);
  const double d_over_h = (dbh / 12.0) / ht;
  const double b1 = coefficient::r1_f1[group] +
      coefficient::r1_f2[group] * d_over_h;
  const double b2 = coefficient::r1_f3[group] +
      coefficient::r1_f4[group] * d_over_h;
  const double ratio = height / ht;
  double squared = b1 * (ratio - 1.0) + b2 * (ratio * ratio - 1.0);
  if (ratio <= coefficient::r1_a1[group]) {
    squared += coefficient::r1_b3[group] *
        std::pow(coefficient::r1_a1[group] - ratio, 2.0);
  }
  if (ratio <= coefficient::r1_a2[group]) {
    squared += coefficient::r1_b4[group] *
        std::pow(coefficient::r1_a2[group] - ratio, 2.0);
  }
  return squared >= 0.0 ? dbh * std::sqrt(squared)
                        : std::numeric_limits<double>::quiet_NaN();
}

inline double r2_two_point(const Equation& equation, double dbh, double ht,
                           double height) {
  const int group = r2_species_group(equation);
  height = std::min(height, ht);
  const double ratio = height / ht;
  double squared = coefficient::r2_mbb[group][0] * (ratio - 1.0) +
      coefficient::r2_mbb[group][1] * (ratio * ratio - 1.0);
  if (ratio < coefficient::r2_mba[group][0]) {
    squared += coefficient::r2_mbb[group][2] *
        std::pow(coefficient::r2_mba[group][0] - ratio, 2.0);
  }
  if (ratio < coefficient::r2_mba[group][1]) {
    squared += coefficient::r2_mbb[group][3] *
        std::pow(coefficient::r2_mba[group][1] - ratio, 2.0);
  }
  const double uncorrected = squared < 0.0 ? 0.0 : dbh * std::sqrt(squared);
  return uncorrected * (coefficient::r2_czc[group][0] +
      coefficient::r2_czc[group][1] * dbh +
      coefficient::r2_czc[group][2] * height * height);
}

inline double r2_three_point_base(const Equation& equation, double dbh,
                                  double ht, double height,
                                  const Auxiliary& auxiliary,
                                  bool clamp_negative = true) {
  const int group = r2_species_group(equation);
  const double inside_bh = coefficient::r2_edbh[group][1] * dbh +
      coefficient::r2_edbh[group][0];
  const double d30_inside = auxiliary.upper_d1 *
      (1.0 - (1.0 - inside_bh / dbh) /
       (2.0 - auxiliary.upper_d1 / dbh));
  const double r1 = height / ht;
  const double r2 = 4.5 / ht;
  const double r3 = auxiliary.upper_ht1 / ht;
  const double a1 = coefficient::r2_mba[group][0];
  const double a2 = coefficient::r2_mba[group][1];
  const double p1 = coefficient::r2_3pb[group][0];
  const double p2 = coefficient::r2_3pb[group][1];
  double b1 = std::pow(inside_bh / dbh, 2.0) * (r3 * r3 - 1.0) +
      std::pow(d30_inside / dbh, 2.0) * (1.0 - r2 * r2);
  if (r3 < a1) b1 += p1 * (r2 * r2 - 1.0) * std::pow(a1 - r3, 2.0);
  if (r2 < a1) b1 += p1 * (1.0 - r3 * r3) * std::pow(a1 - r2, 2.0);
  if (r3 < a2) b1 += p2 * (r2 * r2 - 1.0) * std::pow(a2 - r3, 2.0);
  if (r2 < a2) b1 += p2 * (1.0 - r3 * r3) * std::pow(a2 - r2, 2.0);
  b1 /= (1.0 - r2) * (1.0 - r3 * r3) -
      (1.0 - r2 * r2) * (1.0 - r3);
  double b2 = b1 * (r3 - 1.0) - std::pow(d30_inside / dbh, 2.0);
  if (r3 < a1) b2 += p1 * std::pow(a1 - r3, 2.0);
  if (r3 < a2) b2 += p2 * std::pow(a2 - r3, 2.0);
  b2 /= 1.0 - r3 * r3;
  double squared = b1 * (r1 - 1.0) + b2 * (r1 * r1 - 1.0);
  if (r1 < a1) squared += p1 * std::pow(a1 - r1, 2.0);
  if (r1 < a2) squared += p2 * std::pow(a2 - r1, 2.0);
  squared *= dbh * dbh;
  return squared > 0.0 || !clamp_negative ? std::sqrt(squared) : 0.0;
}

inline double r2_three_point(const Equation& equation, double dbh, double ht,
                             double height, const Auxiliary& auxiliary) {
  // TOP6LEN uses the same one-tenth-foot binary search as MERLEN.
  double top6 = 0.0;
  if (auxiliary.upper_d1 < 6.0) {
    top6 = auxiliary.upper_ht1;
  } else {
    int first = 1;
    int last = static_cast<int>(ht + .5) * 10;
    while (first != last) {
      const int half = (first + last + 1) / 2;
      const double candidate = static_cast<double>(half) / 10.0;
      const double diameter = r2_three_point_base(
          equation, dbh, ht, candidate, auxiliary);
      const int rounded = static_cast<int>((diameter + .005) * 10.0);
      if (60 <= rounded) first = half;
      else last = half - 1;
    }
    top6 = static_cast<double>(first) / 10.0;
  }
  if (top6 > 0.0 && height >= top6) {
    const int group = r2_species_group(equation);
    const double inside_bh = coefficient::r2_edbh[group][1] * dbh +
        coefficient::r2_edbh[group][0];
    const double d30_inside = auxiliary.upper_d1 *
        (1.0 - (1.0 - inside_bh / dbh) /
         (2.0 - auxiliary.upper_d1 / dbh));
    const double start = d30_inside < 6.0 ? d30_inside : 6.0;
    return start * (ht - height) / (ht - top6);
  }
  // R2TAP clamps negative squared diameters only when TOP6 is unset.
  // Below an established TOP6, its square root can be nonfinite.
  return r2_three_point_base(equation, dbh, ht, height, auxiliary, top6 <= 0.0);
}

inline double r5_diameter(const Equation& equation, double dbh, double ht,
                          double height) {
  const int group = r5_species_group(equation.species);
  if (height > ht) return 0.0;
  if (height >= 4.499) {
    const double c1 = coefficient::r5_wkc[group][0];
    double term2 = coefficient::r5_wkc[group][2] +
        coefficient::r5_wkc[group][3] * dbh +
        coefficient::r5_wkc[group][4] * ht;
    if (group == 3 && term2 > -1.0) term2 = -1.0;
    const double term3 = std::pow((height - 1.0) / (ht - 1.0),
                                  coefficient::r5_wkc[group][1]);
    const double term4 = std::log(
        1.0 - term3 * (1.0 - std::exp(c1 / term2)));
    return dbh * (c1 - term2 * term4);
  }
  return (1.0 - coefficient::r5_wkb[group][0]) * dbh *
      std::exp(coefficient::r5_wkb[group][1] * (4.5 - height));
}

inline int blm_profile(const Equation& equation) {
  const int species = equation.species;
  if (species == 202 && equation.forest == 1) return 0;
  if (species == 202 && equation.forest == 2) return 1;
  if (species == 122 && (equation.forest == 0 || equation.forest == 1)) return 2;
  if (species == 117) return 3;
  if (species == 119) return 4;
  if (species == 15 && equation.forest == 1) return 5;
  if (species == 17) return 5;
  if (species == 21 || species == 11 || species == 22) return 6;
  if (species == 260 || species == 263) return 7;
  if (species == 81 || species == 242 || species == 73) return 8;
  return 9;
}

template <std::size_t SpeciesCount, std::size_t ValueCount>
inline double form_class_from_table(
    int species, int size_class, const int (&species_values)[SpeciesCount],
    const int (&form_values)[ValueCount]) {
  static_assert(ValueCount == SpeciesCount * 5,
                "form-class table must have five DBH classes");
  for (std::size_t index = 0; index < SpeciesCount; ++index) {
    if (species_values[index] == species) {
      return form_values[(size_class - 1) * SpeciesCount + index];
    }
  }
  return 80.0;
}

inline double default_form_class(const Equation& equation, double dbh) {
  int size_class = static_cast<int>((dbh - 1.0) / 10.0 + 1.0);
  size_class = std::max(1, size_class);
  if (dbh > 40.9) size_class = 5;
  const int forest = equation.forest;
  const int species = equation.species;
  if (forest == 4) {
    return form_class_from_table(
        species, size_class, form_class::bm_species, form_class::bm_malh);
  }
  if (forest == 7) {
    return form_class_from_table(
        species, size_class, form_class::bm_species, form_class::bm_ocho);
  }
  if (forest == 14) {
    return form_class_from_table(
        species, size_class, form_class::bm_species, form_class::bm_umat);
  }
  if (forest == 16) {
    return form_class_from_table(
        species, size_class, form_class::bm_species, form_class::bm_wlwh);
  }
  if (forest == 10) {
    return form_class_from_table(
        species, size_class, form_class::ca_species, form_class::ca_rogr);
  }
  if (forest == 11) {
    return form_class_from_table(
        species, size_class, form_class::ca_species, form_class::ca_sisk);
  }
  if (forest == 8) {
    return form_class_from_table(
        species, size_class, form_class::ec_species, form_class::ec_okan);
  }
  if (forest == 17) {
    return form_class_from_table(
        species, size_class, form_class::ec_species, form_class::ec_wena);
  }
  if (forest == 21) {
    return form_class_from_table(
        species, size_class, form_class::ni_species, form_class::ni_colv);
  }
  if (forest == 9) {
    return form_class_from_table(
        species, size_class, form_class::pn_species, form_class::pn_olym);
  }
  if (forest == 12) {
    return form_class_from_table(
        species, size_class, form_class::pn_species, form_class::pn_sius);
  }
  if (forest == 1) {
    return form_class_from_table(
        species, size_class, form_class::so_species, form_class::so_desh);
  }
  if (forest == 2) {
    return form_class_from_table(
        species, size_class, form_class::so_species, form_class::so_frem);
  }
  if (forest == 20) {
    return form_class_from_table(
        species, size_class, form_class::so_species, form_class::so_wine);
  }
  if (forest == 3) {
    return form_class_from_table(
        species, size_class, form_class::wc_species, form_class::wc_gifp);
  }
  if (forest == 5) {
    return form_class_from_table(
        species, size_class, form_class::wc_species, form_class::wc_mbsn);
  }
  if (forest == 6) {
    return form_class_from_table(
        species, size_class, form_class::wc_species, form_class::wc_mthd);
  }
  if (forest == 15) {
    return form_class_from_table(
        species, size_class, form_class::wc_species, form_class::wc_umpq);
  }
  if (forest == 18) {
    return form_class_from_table(
        species, size_class, form_class::wc_species, form_class::wc_will);
  }
  return 80.0;
}

inline double behre_diameter(const Equation& equation, double dbh, double ht,
                             double height, double form_class) {
  if (equation.family == Family::blm) {
    const int profile = blm_profile(equation);
    const double d17 = std::floor(dbh * form_class / 100.0 + .5);
    const double hbutt = ht - 17.8;
    if (hbutt <= 0.0) return 0.0;
    const double htdib = ht - height;
    const double a = coefficient::blm_ht[profile][0] +
        coefficient::blm_ht[profile][1] * dbh +
        coefficient::blm_ht[profile][2] * ht +
        coefficient::blm_ht[profile][3] * dbh * ht;
    return d17 * (htdib / hbutt) /
        (a * (htdib / hbutt) + (1.0 - a));
  }
  const double segment_length =
      equation.id.compare(0, 3, "632") == 0 ? 32.6 : 16.3;
  const double h1 = ht - segment_length - 1.0;
  if (h1 <= 0.0) return 0.0;
  const double ratio = (ht - height) / h1;
  return dbh * form_class / 100.0 * ratio / (.62 * ratio + .38);
}

inline double r12_diameter(double dbh, double height) {
  if (height >= 4.5) return 0.0;
  const double actual_height = height < .0001 ? 1.0 : height;
  return dbh * .91979 + dbh * .12152 *
      (4.5 - actual_height) / (actual_height + 1.0);
}

inline Result diameter(const Equation& equation, double dbh, double ht,
                       double height, const Auxiliary& auxiliary) {
  if (equation.family == Family::r2 && equation.three_point) {
    if (!std::isfinite(auxiliary.upper_ht1) ||
        !std::isfinite(auxiliary.upper_d1) || auxiliary.upper_ht1 <= 0.0 ||
        auxiliary.upper_d1 <= 0.0) return {NAN, 309};
    if (auxiliary.upper_ht1 <= 4.5 || auxiliary.upper_ht1 > .95 * ht) {
      return {NAN, 310};
    }
  }
  double value = NAN;
  if (equation.family == Family::r1) value = r1_diameter(equation, dbh, ht, height);
  else if (equation.family == Family::r2 && !equation.three_point) {
    value = r2_two_point(equation, dbh, ht, height);
  } else if (equation.family == Family::r2) {
    value = r2_three_point(equation, dbh, ht, height, auxiliary);
  } else if (equation.family == Family::r5) value = r5_diameter(equation, dbh, ht, height);
  else if (equation.family == Family::r12) value = r12_diameter(dbh, height);
  else {
    const double form_class = std::isfinite(auxiliary.form_class)
        ? auxiliary.form_class : default_form_class(equation, dbh);
    value = behre_diameter(equation, dbh, ht, height, form_class);
  }
  return std::isfinite(value) ? Result{value, 0} : Result{NAN, 54};
}

inline Result sample_profile(const Equation& equation, double dbh, double ht,
                             const Auxiliary& auxiliary,
                             std::vector<double>* heights,
                             std::vector<double>* values) {
  constexpr double step = 1.0 / 192.0;
  const int intervals = std::max(1, static_cast<int>(
      std::ceil(std::max(ht - 1.0, 0.0) / step)));
  heights->resize(static_cast<std::size_t>(intervals + 1));
  values->resize(static_cast<std::size_t>(intervals + 1));
  for (int interval = 0; interval <= intervals; ++interval) {
    const double height = interval == intervals
        ? ht : std::min(ht, 1.0 + interval * step);
    Result evaluated = diameter(equation, dbh, ht, height, auxiliary);
    if (evaluated.status != 0) return evaluated;
    (*heights)[static_cast<std::size_t>(interval)] = height;
    (*values)[static_cast<std::size_t>(interval)] = evaluated.value;
  }
  return {0.0, 0};
}

inline Result inverse_from_samples(
    const Equation& equation, double dbh, double ht, double target,
    const Auxiliary& auxiliary, const std::vector<double>& heights,
    const std::vector<double>& values) {
  constexpr double tolerance = 1e-4;
  if (heights.empty() || heights.size() != values.size()) return {NAN, 54};
  const double stump_value = values.front();
  double previous_height = heights.front();
  double previous_error = stump_value - target;
  double result = NAN;
  int crossing_count = 0;
  if (previous_error == 0.0) {
    result = previous_height;
    ++crossing_count;
  }
  const double tip_value = values.back();
  for (std::size_t interval = 1; interval < heights.size(); ++interval) {
    const double height = heights[interval];
    const double current_error = values[interval] - target;
    if (current_error == 0.0) {
      result = height;
      ++crossing_count;
    }
    if ((previous_error < 0.0 && current_error > 0.0) ||
        (previous_error > 0.0 && current_error < 0.0)) {
      double lower = previous_height;
      double upper = height;
      double lower_error = previous_error;
      for (int iteration = 0; iteration < 200 && upper - lower > tolerance;
           ++iteration) {
        const double midpoint = (lower + upper) / 2.0;
        Result middle = diameter(equation, dbh, ht, midpoint, auxiliary);
        if (middle.status != 0) return middle;
        const double middle_error = middle.value - target;
        if (middle_error == 0.0) {
          lower = midpoint;
          upper = midpoint;
        } else if (lower_error * middle_error <= 0.0) {
          upper = midpoint;
        } else {
          lower = midpoint;
          lower_error = middle_error;
        }
      }
      const double root = (lower + upper) / 2.0;
      result = std::isfinite(result) ? std::max(result, root) : root;
      ++crossing_count;
    }
    previous_height = height;
    previous_error = current_error;
  }
  if (crossing_count == 0) {
    if (target < tip_value) return {NAN, 100};
    if (target > stump_value) return {NAN, 101};
    return {NAN, 103};
  }
  return {result, crossing_count > 1 ? 102 : 0};
}

inline Result inverse(const Equation& equation, double dbh, double ht,
                      double target, const Auxiliary& auxiliary) {
  std::vector<double> heights;
  std::vector<double> values;
  Result sampled = sample_profile(
      equation, dbh, ht, auxiliary, &heights, &values);
  if (sampled.status != 0) return sampled;
  return inverse_from_samples(
      equation, dbh, ht, target, auxiliary, heights, values);
}

inline Result nvel_height(const Equation& equation, double dbh, double ht,
                          double target, const Auxiliary& auxiliary) {
  if (equation.family == Family::r12) return {0.0, 0};
  if (equation.family == Family::r1) {
    return inverse(equation, dbh, ht, target, auxiliary);
  }
  if (equation.family == Family::r2 && equation.three_point) {
    if (!std::isfinite(auxiliary.upper_ht1) ||
        !std::isfinite(auxiliary.upper_d1) || auxiliary.upper_ht1 <= 0.0 ||
        auxiliary.upper_d1 <= 0.0) return {NAN, 309};
    if (auxiliary.upper_ht1 <= 4.5 || auxiliary.upper_ht1 > .95 * ht) {
      return {NAN, 310};
    }
  }
  const int top = static_cast<int>(
      std::floor((target + .005) * 10.0 + .5));
  int first = 1;
  int last = static_cast<int>(ht + .5) * 10;
  while (first != last) {
    const int half = (first + last + 1) / 2;
    const double height = static_cast<double>(half) / 10.0;
    Result evaluated = diameter(equation, dbh, ht, height, auxiliary);
    if (evaluated.status != 0) return evaluated;
    const int rounded = static_cast<int>((evaluated.value + .005) * 10.0);
    if (top <= rounded) first = half;
    else last = half - 1;
  }
  return {static_cast<double>(first) / 10.0, 0};
}

inline double r1_total(const Equation& equation, double dbh, double ht) {
  if (equation.species == 19) return 0.0;
  if (equation.species != 108) {
    int group = -1;
    if (equation.species == 119) group = 1;
    else if (equation.species == 70 || equation.species == 73) group = 2;
    else if (equation.species == 202) group = 3;
    else if (equation.species == 17) group = 4;
    else if (equation.species == 122) group = 5;
    if (group < 0) return 0.0;
    return .01 * coefficient::r1_fa[group][0] *
        std::pow(dbh, coefficient::r1_fa[group][1]) *
        std::pow(ht, coefficient::r1_fa[group][2]);
  }
  const int group = r1_forest_group(equation.forest);
  const double d_over_h = (dbh / 12.0) / ht;
  const double b1 = coefficient::r1_f1[group] + coefficient::r1_f2[group] * d_over_h;
  const double b2 = coefficient::r1_f3[group] + coefficient::r1_f4[group] * d_over_h;
  const double cff = b2 / 3.0 + b1 / 2.0 - b1 - b2 +
      coefficient::r1_b3[group] / 3.0 * std::pow(coefficient::r1_a1[group], 3.0) +
      coefficient::r1_b4[group] / 3.0 * std::pow(coefficient::r1_a2[group], 3.0);
  return .005454154 * cff * dbh * dbh * ht;
}

inline int blm_taper_equation(const Equation& equation) {
  const int species = equation.species;
  const int forest = equation.forest;
  if (species == 202 && forest >= 1 && forest <= 4) return forest;
  if (species == 211 || (species == 202 && forest == 5)) {
    return species == 211 ? 5 : 6;
  }
  if (species == 122) {
    if (forest == 1) return 10;
    if (forest == 0) return 11;
    return 56;
  }
  if (species == 116) return 12;
  if (species == 117) return 13;
  if (species == 119) return 14;
  if (species == 108) return 15;
  if (species == 231) return 20;
  if (species == 631) return 21;
  if (species == 351) return 22;
  if (species == 998) return 23;
  if (species == 312) return 24;
  if (species == 361) return 25;
  if (species == 431) return 26;
  if (species == 542) return 27;
  if (species == 747) return 28;
  if (species == 800) return 29;
  if (species == 15) {
    if (forest == 1) return 30;
    if (forest == 0 || forest == 2) return 31;
    return 56;
  }
  if (species == 21) return 32;
  if (species == 17) return 33;
  if (species == 11) return 34;
  if (species == 22) return 35;
  if (species == 93) return 41;
  if (species == 98) return 42;
  if (species == 260 || species == 263) return 48;
  if (species == 81) return 51;
  if (species == 42) return 52;
  if (species == 41) return 53;
  if (species == 242) return 54;
  if (species == 73) return 55;
  return 56;
}

inline double blm_inside_bark_dbh(const Equation& equation, double dbh) {
  const int taper = blm_taper_equation(equation);
  if (taper == 1 || taper == 2 || taper == 3 || taper == 5 || taper == 35) {
    return .903563 * std::pow(dbh, .989388);
  }
  if (taper == 11 || taper == 12) {
    return .809427 * std::pow(dbh, 1.016866);
  }
  if (taper == 13 || taper == 14) return .859045 * dbh;
  if (taper == 15) return dbh - (.3147 + .0274 * dbh);
  if (taper == 20 || taper == 25) return -.03425 + .98155 * dbh;
  if (taper == 21) return -4.36852 + .95354 * dbh + .18307 * 4.5;
  if (taper == 22 || taper == 23 || taper == 24 || taper == 26 ||
      taper == 27) {
    return .39534 + .90182 * dbh;
  }
  if (taper == 28 || taper == 29) return -.78034 + .95956 * dbh;
  if (taper == 31 || taper == 33) return .904973 * dbh;
  if (taper == 32 || taper == 34) {
    return .86951 * std::pow(dbh, 1.00983);
  }
  if (taper == 41 || taper == 42) return dbh - (.2113 + .0445 * dbh);
  if (taper == 48 || taper == 56) return dbh / 1.071;
  if (taper == 52 || taper == 54) return dbh / 1.053;
  if (taper == 51 || taper == 53) return .837291 * dbh;
  if (taper == 55) return dbh - (.1231 + .1306 * dbh);
  return std::numeric_limits<double>::quiet_NaN();
}

inline Result blm_short_total(const Equation& equation, double dbh,
                              double profile_ht) {
  const double dbh_ib = blm_inside_bark_dbh(equation, dbh);
  if (!std::isfinite(dbh_ib) || dbh_ib <= .0001) return {NAN, 314};
  const double cylinder = kSmalian * dbh_ib * dbh_ib * profile_ht;
  if (profile_ht <= 17.8) return {cylinder, 0};
  const double inside_17 = std::floor(std::sqrt(
      dbh_ib * dbh_ib - dbh_ib * dbh_ib * 17.3 / profile_ht) + .5);
  const double primary_top = std::floor(.184 * dbh + 2.24 + .5);
  if (inside_17 < primary_top) return {cylinder, 0};
  return {NAN, 53};
}

inline Result smalian_total(const Equation& equation, double dbh, double ht,
                            const Auxiliary& auxiliary) {
  const double profile_ht = equation.family == Family::blm ? ht + 1.5 : ht;
  if (equation.family == Family::blm) {
    const Result short_total = blm_short_total(equation, dbh, profile_ht);
    if (short_total.status != 53) return short_total;
  }
  Result start = diameter(equation, dbh, profile_ht, 1.0, auxiliary);
  if (start.status != 0) return start;
  double large = start.value;
  double volume = kPiNvel * std::pow(large / 2.0, 2.0) / 144.0;
  double height = 1.0;
  const int loops = static_cast<int>((profile_ht + .5 - 1.0) / 4.0);
  for (int index = 0; index < loops; ++index) {
    const double next = height + 4.0;
    Result small = diameter(equation, dbh, profile_ht, next, auxiliary);
    if (small.status != 0) return small;
    volume += kSmalian * (large * large + small.value * small.value) * 4.0;
    height = next;
    large = small.value;
  }
  if (profile_ht - height > 0.0) {
    volume += kSmalian * large * large * (profile_ht - height);
  }
  return {volume, 0};
}

inline double r6_behre_total(const Equation& equation, double dbh, double ht,
                             double form_class) {
  const double h17 = equation.id.compare(0, 3, "632") == 0 ? 33.6 : 17.3;
  if (ht <= h17) return kSmalian * dbh * dbh * ht;
  const double d17 = form_class / 100.0 * dbh;
  constexpr double top = 4.0;
  if (dbh < top) return kSmalian * dbh * dbh * ht;
  if (d17 < top) {
    return kSmalian * (dbh * dbh + d17 * d17) * h17 +
        kSmalian * d17 * d17 * (ht - h17);
  }
  double volume = kSmalian * (dbh * dbh + d17 * d17) * h17;
  const double upper_length = ht - h17;
  double previous = d17;
  int index = 2;
  for (; index <= 20; ++index) {
    const double height_ratio =
        (upper_length - (index - 1) * 16.3) / upper_length;
    if (height_ratio <= 0.0) break;
    const double current = d17 * height_ratio / (.62 * height_ratio + .38);
    if (current < top) break;
    volume += kSmalian * (previous * previous + current * current) * 16.3;
    previous = current;
    if (current == top) {
      const double tip_length = upper_length - (index - 1) * 16.3;
      volume += kSmalian * top * top * tip_length;
      return volume;
    }
  }
  const double diameter_ratio = top / d17;
  const double hx = diameter_ratio * .38 * upper_length /
      (1.0 - .62 * diameter_ratio);
  const double completed = (index - 2) * 16.3;
  const double section = upper_length - hx - completed;
  volume += kSmalian * (previous * previous + top * top) * section;
  const double tip_length = ht - (completed + h17) - section;
  volume += kSmalian * top * top * tip_length;
  return volume;
}

inline Result volume(const Equation& equation, double dbh, double ht,
                     double lower, double upper, const Auxiliary& auxiliary) {
  if (equation.family == Family::r2 && equation.three_point &&
      (!std::isfinite(auxiliary.upper_ht1) || !std::isfinite(auxiliary.upper_d1))) {
    return {NAN, 309};
  }
  if ((equation.family == Family::r12 || equation.family == Family::blm ||
       equation.family == Family::behre) && !std::isfinite(auxiliary.form_class)) {
    return {NAN, 302};
  }
  if (lower == 0.0 && upper == ht) {
    double value = NAN;
    if (equation.family == Family::r1) {
      value = r1_total(equation, dbh, ht);
    } else if (equation.family == Family::r12) {
      const int group = r12_species_group(equation);
      const double* c = coefficient::r12_total_cubic[group];
      const double fc = auxiliary.form_class / 100.0;
      value = c[0] + c[1] * dbh + c[3] * dbh * dbh * ht * fc +
          c[5] * fc + c[6] * ht;
      return {value, std::isfinite(value) ? 0 : 54};
    } else if (equation.family == Family::behre) {
      value = r6_behre_total(
          equation, dbh, ht, auxiliary.form_class);
      return {value, std::isfinite(value) ? 0 : 54};
    } else {
      Result total = smalian_total(equation, dbh, ht, auxiliary);
      if (total.status != 0) {
        if (equation.family == Family::r5 && total.status == 54) {
          return {0.0, 0};
        }
        return total;
      }
      value = total.value;
      if (equation.family == Family::blm) {
        return {value, std::isfinite(value) ? 0 : 54};
      }
    }
    value = std::max(0.0, std::floor(value * 10.0 + .5) / 10.0);
    return {value, std::isfinite(value) ? 0 : 54};
  }
  if (!(lower >= 0.0) || !(upper <= ht) || !(lower < upper)) return {NAN, 54};
  Result large = diameter(equation, dbh, ht, lower, auxiliary);
  if (large.status != 0) return large;
  double height = lower;
  double value = 0.0;
  while (height + 4.0 <= upper) {
    Result small = diameter(equation, dbh, ht, height + 4.0, auxiliary);
    if (small.status != 0) return small;
    value += kSmalian * (large.value * large.value + small.value * small.value) * 4.0;
    height += 4.0;
    large = small;
  }
  if (height < upper) {
    Result small = diameter(equation, dbh, ht, upper, auxiliary);
    if (small.status != 0) return small;
    value += kSmalian * (large.value * large.value + small.value * small.value) *
        (upper - height);
  }
  return {value, std::isfinite(value) ? 0 : 54};
}

}  // namespace smalltapers
}  // namespace treevolume

#endif
