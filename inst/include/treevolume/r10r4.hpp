#ifndef TREEVOLUME_R10R4_HPP
#define TREEVOLUME_R10R4_HPP

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

#include <treevolume/r10r4_coefficients.hpp>

namespace treevolume {
namespace r10r4 {

struct Result {
  double value;
  int status;
};

enum class Family { r10, r4_driver };

struct Equation {
  Family family;
  std::string id;
  int species;
  int r4_index;
  bool old_r10_driver;
};

inline Result failure(int status) {
  return {std::numeric_limits<double>::quiet_NaN(), status};
}

inline bool three_digits(const std::string& value) {
  return value.size() == 3 && value[0] >= '0' && value[0] <= '9' &&
      value[1] >= '0' && value[1] <= '9' && value[2] >= '0' &&
      value[2] <= '9';
}

inline int r4_coefficient_index(const std::string& id) {
  const std::string prefix = id.substr(0, 3);
  const std::string species = id.substr(7, 3);
  if (species == "746") return 0;
  if (species == "202" && prefix == "400") return 1;
  if (species == "202" && prefix == "405") return 2;
  if (species == "202" && prefix == "401") return 3;
  if (species == "019" && prefix == "400") return 4;
  if (species == "019" && prefix == "405") return 5;
  if (species == "015" && prefix == "400") return 6;
  if (species == "015" && prefix == "401") return 7;
  if (species == "081") return 8;
  if (species == "073") return 9;
  if (species == "122" && prefix == "403") return 10;
  if (species == "108" && prefix == "400") return 11;
  if (species == "108" && prefix == "401") return 12;
  if (species == "122" && prefix == "401") return 13;
  if (species == "122" && prefix == "402") return 14;
  if (species == "122" && prefix == "400") return 15;
  if (species == "093" && prefix == "400") return 16;
  if (species == "093" && prefix == "407") return 17;
  if (species == "020") return 18;
  if (species == "117") return 19;
  return -1;
}

inline bool parse_equation(const std::string& key, Equation* equation) {
  static const std::string r10_prefix = "r10:";
  static const std::string r4_prefix = "r4mat:";
  Family family;
  std::string id;
  if (key.compare(0, r10_prefix.size(), r10_prefix) == 0) {
    family = Family::r10;
    id = key.substr(r10_prefix.size());
  } else if (key.compare(0, r4_prefix.size(), r4_prefix) == 0) {
    family = Family::r4_driver;
    id = key.substr(r4_prefix.size());
  } else {
    return false;
  }
  if (id.size() != 10 || !three_digits(id.substr(7, 3))) {
    return false;
  }
  const std::string mnemonic = id.substr(3, 3);
  if (family == Family::r10 && mnemonic != "DEM" && mnemonic != "CUR" &&
      mnemonic != "BRU") {
    return false;
  }
  if (family == Family::r4_driver && mnemonic != "MAT") {
    return false;
  }
  equation->family = family;
  equation->id = id;
  equation->species = std::stoi(id.substr(7, 3));
  equation->r4_index = family == Family::r4_driver
      ? r4_coefficient_index(id) : -1;
  equation->old_r10_driver = id.substr(0, 3) == "A01" ||
      id.substr(0, 3) == "A02";
  return true;
}

inline double r10_diameter(const Equation& equation, double dbh, double ht,
                           double height, bool source_thresholds = false) {
  using namespace r10r4_coefficients;
  const double rh = (ht - height) / (ht - 4.5);
  if (rh <= 0.0) return 0.0;
  const double rh32 = std::max(rh, 0.078);
  const double rh40 = std::max(rh, 0.15);
  const double rh15 = std::pow(rh, spruce_shape);
  const double rh3 = std::pow(rh, 3.0);
  const double tail32 = rh15 - std::pow(rh32, 32.0);
  double squared_ratio;
  double bark = 1.0;
  if (equation.species == 42 &&
      (source_thresholds ? dbh <= 38.01 : dbh < 38.01)) {
    squared_ratio = rh15 + (rh15 - rh3) *
        (alaska_cedar_d * dbh + alaska_cedar_h2 * ht * ht +
         alaska_cedar_hd * ht / dbh) + tail32 *
        (alaska_cedar_d_tail * dbh + alaska_cedar_h2_tail * ht * ht +
         alaska_cedar_d2_tail / (dbh * dbh) +
         alaska_cedar_hd_tail * ht / dbh);
    squared_ratio = std::max(0.0, squared_ratio);
    bark = alaska_cedar_bark_0 + alaska_cedar_bark_d * dbh +
        alaska_cedar_bark_h_inv / ht;
  } else if (equation.species == 242 &&
             (source_thresholds ? dbh <= 56.01 : dbh < 56.01)) {
    squared_ratio = rh15 + (rh15 - rh3) *
        (redcedar_d2 / (dbh * dbh) + redcedar_d * dbh +
         redcedar_h * ht + redcedar_h2 * ht * ht +
         redcedar_d_sq * dbh * dbh) + tail32 *
        (redcedar_h2_tail * ht * ht + redcedar_d2_tail / (dbh * dbh));
    squared_ratio = std::max(0.0, squared_ratio);
    bark = redcedar_bark_0 + redcedar_bark_h * ht +
        redcedar_bark_d_inv / dbh;
  } else if (equation.species == 351) {
    squared_ratio = alder_0 * rh15 -
        alder_d * (rh15 - rh3) * dbh * 1e-2 +
        alder_h * (rh15 - rh3) * ht * 1e-3 -
        alder_hd * tail32 * ht * dbh * 1e-5 +
        alder_sqrt_h * tail32 * std::sqrt(ht) * 1e-3 -
        alder_h2 * (rh15 - std::pow(rh40, 40.0)) * ht * ht * 1e-6;
    squared_ratio = std::max(0.0, squared_ratio);
  } else {
    squared_ratio = rh15 +
        (spruce_h1 * ht + spruce_h2 * ht * ht + spruce_hd * ht / dbh) *
            (rh15 - rh3) +
        (spruce_d2 / (dbh * dbh) + spruce_h2_tail * ht * ht +
         spruce_hd_tail * ht / dbh) * tail32;
    squared_ratio = std::max(0.0, squared_ratio);
    bark = spruce_bark_0 + spruce_bark_d * dbh + spruce_bark_h * ht;
  }
  return dbh * std::sqrt(squared_ratio * bark);
}

struct R4Profile {
  double stump_diameter;
  double cf0;
  double exponent;
  int status;
};

inline R4Profile r4_profile(const Equation& equation, double dbh, double ht) {
  using r10r4_coefficients::r4_cfcoef;
  if (dbh < 1.0) return {0.0, 0.0, 0.0, 303};
  if (ht <= 5.0) return {0.0, 0.0, 0.0, 304};
  if (equation.r4_index < 0) return {0.0, 0.0, 0.0, 301};
  const int index = equation.r4_index;
  const double total = ht - 1.0;
  const double ht67 = r4_cfcoef[index] *
      std::pow(dbh, r4_cfcoef[index + 20]) *
      std::pow(total, r4_cfcoef[index + 40]);
  const double butt = r4_cfcoef[index + 80] * dbh +
      r4_cfcoef[index + 60];
  const double stump = std::sqrt(butt * butt * total / (total - 4.0));
  const double d67 = r4_cfcoef[index + 120] * dbh * (2.0 / 3.0) +
      r4_cfcoef[index + 100];
  const double cf0 = 0.002727 *
      (ht67 * stump * stump + d67 * d67 * total);
  const double form = cf0 / (0.005454 * stump * stump * total);
  const double exponent = (1.0 - form) / (2.0 * form);
  if (!std::isfinite(stump) || !std::isfinite(cf0) ||
      !std::isfinite(exponent)) {
    return {0.0, 0.0, 0.0, 54};
  }
  return {stump, cf0, exponent, 0};
}

inline Result diameter(const Equation& equation, double dbh, double ht,
                       double height) {
  if (equation.family == Family::r10) {
    if (!(dbh > 0.0) || !(ht > 4.5)) return failure(54);
    const double value = r10_diameter(equation, dbh, ht, height);
    return std::isfinite(value) ? Result{value, 0} : failure(54);
  }
  const R4Profile profile = r4_profile(equation, dbh, ht);
  if (profile.status != 0) return failure(profile.status);
  double value = 0.0;
  if (height > 0.0 && height < ht) {
    value = height <= 1.0 ? profile.stump_diameter :
        profile.stump_diameter * std::pow(
            (ht - height) / (ht - 1.0), profile.exponent);
  }
  return std::isfinite(value) ? Result{value, 0} : failure(54);
}

inline double r10_general_shape(const Equation& equation, double dbh,
                                double ht, double rh,
                                bool initial_alder_step = false) {
  using namespace r10r4_coefficients;
  if (rh <= 0.0) return 0.0;
  const double rh32 = std::max(rh, 0.078);
  double rh40 = std::max(rh, 0.15);
  if (initial_alder_step && rh >= 0.078 && rh < 0.15) rh40 = 0.13;
  const double rh15 = std::pow(rh, spruce_shape);
  const double rh3 = std::pow(rh, 3.0);
  const double tail32 = rh15 - std::pow(rh32, 32.0);
  double shape;
  if (equation.species == 351) {
    shape = alder_0 * rh15 - alder_d * (rh15 - rh3) * dbh * 1e-2 +
        alder_h * (rh15 - rh3) * ht * 1e-3 -
        alder_hd * tail32 * ht * dbh * 1e-5 +
        alder_sqrt_h * tail32 * std::sqrt(ht) * 1e-3 -
        alder_h2 * (rh15 - std::pow(rh40, 40.0)) * ht * ht * 1e-6;
  } else {
    shape = rh15 +
        (spruce_h1 * ht + spruce_h2 * ht * ht + spruce_hd * ht / dbh) *
            (rh15 - rh3) +
        (spruce_d2 / (dbh * dbh) + spruce_h2_tail * ht * ht +
         spruce_hd_tail * ht / dbh) * tail32;
  }
  return std::max(0.0, shape);
}

inline Result r10_driver_height(const Equation& equation, double dbh,
                                double ht, double target,
                                bool source_thresholds = false) {
  using namespace r10r4_coefficients;
  const bool small_alaska_cedar = equation.species == 42 &&
      (source_thresholds ? dbh <= 38.01 : dbh < 38.01);
  const bool small_redcedar = equation.species == 242 &&
      (source_thresholds ? dbh <= 56.01 : dbh < 56.01);
  if (small_alaska_cedar || small_redcedar) {
    double hitop = 1.0;
    double dsi = 0.0;
    do {
      hitop += 16.3;
      if (hitop > ht) break;
      dsi = r10_diameter(equation, dbh, ht, hitop, source_thresholds);
    } while (dsi >= target);
    hitop -= 16.3;
    double increment = 1.0;
    for (int iteration = 0; iteration < 100000; ++iteration) {
      hitop += increment;
      dsi = r10_diameter(equation, dbh, ht, hitop, source_thresholds);
      if (dsi > target - 0.001 && dsi < target + 0.001) {
        return {hitop, 0};
      }
      if (dsi <= target) {
        hitop -= increment;
        increment *= 0.1;
      }
      if (increment == 0.0 || !std::isfinite(hitop)) break;
    }
    return failure(103);
  }

  const double bark = equation.species == 351 ? 1.0 :
      spruce_bark_0 + spruce_bark_d * dbh + spruce_bark_h * ht;
  const double target_shape = target * target / (bark * dbh * dbh);
  const double xll = 1.0 - (2.0 / 3.0) * (target / dbh);
  const double initial_height = ht * xll;
  double rh = (ht - initial_height) / (ht - 4.5);
  double shape = r10_general_shape(equation, dbh, ht, rh, true);
  const double trial_rh = 0.9 * rh;
  const double trial_shape = r10_general_shape(
      equation, dbh, ht, trial_rh, false);
  const double taper = (shape - trial_shape) / (0.1 * rh);
  for (int iteration = 0; iteration < 10; ++iteration) {
    if (shape > target_shape - 0.0001 &&
        shape < target_shape + 0.0001) break;
    if (taper != 0.0) rh += (target_shape - shape) / taper;
    if (rh <= 0.0) rh = 0.0;
    shape = r10_general_shape(equation, dbh, ht, rh, false);
  }
  const double value = ht - rh * (ht - 4.5);
  return std::isfinite(value) ? Result{value, 0} : failure(54);
}

inline Result height_at_diameter(const Equation& equation, double dbh,
                                 double ht, double target,
                                 bool source_thresholds = false) {
  if (equation.family == Family::r10) {
    if (!(dbh > 0.0) || !(ht > 4.5) || !(target > 0.0)) return failure(54);
    return r10_driver_height(
        equation, dbh, ht, target, source_thresholds);
  }
  const R4Profile profile = r4_profile(equation, dbh, ht);
  if (profile.status != 0) return failure(profile.status);
  if (!(target > 0.0) || !(target < profile.stump_diameter)) {
    return {0.0, 0};
  }
  const double value = 1.0 + (ht - 1.0) - (ht - 1.0) *
      std::pow(target / profile.stump_diameter, 1.0 / profile.exponent);
  return std::isfinite(value) ? Result{value, 0} : failure(54);
}

inline double nvel_round_tenth(double value) {
  return std::floor(value * 10.0 + 0.5) / 10.0;
}

inline double r10_total(const Equation& equation, double dbh, double ht) {
  using namespace r10r4_coefficients;
  if (ht <= 40.0 || dbh < 9.0) {
    double result;
    if (dbh <= 3.5 || ht < 18.0) {
      if (ht <= 4.5) return 0.0;
      double form;
      if (ht <= 18.0) {
        const double term1 = (ht - 0.9) * (ht - 0.9) /
            ((ht - 4.5) * (ht - 4.5));
        const double term2 = term1 * (ht - 0.9) / (ht - 4.5);
        form = fstgro_short_0 * term1 + fstgro_short_d * dbh * term2 +
            fstgro_short_dh * dbh * ht * term2;
      } else {
        form = fstgro_tall_0 + fstgro_tall_h2 / (ht * ht) +
            fstgro_tall_dh2 * dbh / (ht * ht) +
            fstgro_tall_dh * dbh / ht + fstgro_tall_d * dbh;
      }
      result = std::max(0.0, fstgro_scale * form * dbh * dbh * ht);
    } else {
      result = std::exp(secgro_0 + secgro_d * std::log(dbh) +
                        secgro_h * std::log(ht));
    }
    return nvel_round_tenth(result);
  }
  const int sections = static_cast<int>((ht - 1.0) / 4.0);
  double section_height = 1.0;
  double diameter_now = r10_diameter(equation, dbh, ht, section_height);
  double radius = diameter_now / 2.0;
  double result = r10tc_pi * radius * radius / 144.0;
  for (int section = 0; section < sections; ++section) {
    const double diameter_old = diameter_now;
    section_height += 4.0;
    diameter_now = r10_diameter(equation, dbh, ht, section_height);
    result += r10tc_smalian *
        (diameter_old * diameter_old + diameter_now * diameter_now) * 4.0;
  }
  if (ht > section_height) {
    result += r10tc_smalian * diameter_now * diameter_now *
        (ht - section_height);
  }
  return equation.old_r10_driver ? result : nvel_round_tenth(result);
}

inline Result r10_segment_volume(const Equation& equation, double dbh,
                                 double ht, double lower, double upper) {
  using namespace r10r4_coefficients;
  double result = 0.0;
  double position = lower;
  if (position < 1.0) {
    const double end = std::min(upper, 1.0);
    const double d = r10_diameter(equation, dbh, ht, 1.0);
    result += r10tc_pi * d * d / 576.0 * (end - position);
    position = end;
  }
  while (position < upper) {
    const double next_grid = 1.0 + 4.0 *
        (std::floor((position - 1.0) / 4.0) + 1.0);
    const double end = std::min(upper, next_grid);
    const double d1 = r10_diameter(equation, dbh, ht, position);
    const double d2 = r10_diameter(equation, dbh, ht, end);
    result += r10tc_smalian * (d1 * d1 + d2 * d2) * (end - position);
    position = end;
  }
  return std::isfinite(result) ? Result{result, 0} : failure(54);
}

inline Result volume(const Equation& equation, double dbh, double ht,
                     double lower, double upper) {
  if (equation.family == Family::r10) {
    if (!(dbh > 0.0) || !(ht > 4.5)) return failure(54);
    if (lower == 0.0 && upper == ht) {
      const double value = r10_total(equation, dbh, ht);
      return std::isfinite(value) ? Result{value, 0} : failure(54);
    }
    return r10_segment_volume(equation, dbh, ht, lower, upper);
  }
  const R4Profile profile = r4_profile(equation, dbh, ht);
  if (profile.status != 0) return failure(profile.status);
  const double start = std::max(lower, 1.0);
  const double power = 2.0 * profile.exponent;
  const double total = ht - 1.0;
  double integral = 0.0;
  if (start < upper) {
    const double left = (ht - start) / total;
    const double right = (ht - upper) / total;
    if (std::abs(power + 1.0) < 1e-12) {
      integral = total * std::log(left / right);
    } else {
      integral = total * (std::pow(left, power + 1.0) -
                          std::pow(right, power + 1.0)) / (power + 1.0);
    }
  }
  if (lower < 1.0) integral += std::min(upper, 1.0) - lower;
  const double value = 3.14159265358979323846 / 576.0 *
      profile.stump_diameter * profile.stump_diameter * integral;
  return std::isfinite(value) ? Result{value, 0} : failure(54);
}

inline Result driver_total_volume(const Equation& equation, double dbh,
                                  double ht) {
  if (equation.family != Family::r4_driver) return failure(54);
  if (ht > 4.5 && ht <= 6.0) {
    return {dbh * dbh * ht * .00272708, 0};
  }
  const R4Profile profile = r4_profile(equation, dbh, ht);
  if (profile.status != 0) return failure(profile.status);
  return std::isfinite(profile.cf0) ? Result{profile.cf0, 0} : failure(54);
}

}  // namespace r10r4
}  // namespace treevolume

#endif
