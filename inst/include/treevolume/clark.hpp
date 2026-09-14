#ifndef TREEVOLUME_CLARK_HPP
#define TREEVOLUME_CLARK_HPP

#include <algorithm>
#include <cmath>
#include <limits>
#include <string>

#include <treevolume/clark_coefficients.hpp>

namespace treevolume {
namespace clark {

struct Result {
  double value = std::numeric_limits<double>::quiet_NaN();
  int status = 0;
};

struct Equation {
  std::string id;
  int region = 0;
  int geocode = 0;
  int top_code = 0;
  int species = 0;
  char variant = 'E';
  bool old_region8 = false;
  bool region8_on_region9 = false;
};

struct Auxiliary {
  double upper_ht1 = std::numeric_limits<double>::quiet_NaN();
  double site_index = std::numeric_limits<double>::quiet_NaN();
  double basal_area = std::numeric_limits<double>::quiet_NaN();
};

struct Profile {
  double dbh_ob = 0.0;
  double input_ht = 0.0;
  double total_ht = 0.0;
  double dbh_ib = 0.0;
  double dib17 = 0.0;
  double r = 0.0;
  double c = 0.0;
  double e = 0.0;
  double p = 0.0;
  double b = 0.0;
  double a = 0.0;
  double q = 0.0;
  double a4 = 0.0;
  double b4 = 0.0;
  double a17 = 0.0;
  double b17 = 0.0;
  double fixed_diameter = 0.0;
  double upper_ht = 0.0;
  double short_scale = 1.0;
  int species = 0;
  int species_group = 0;
  int top_code = 0;
  bool old_region8 = false;
  bool actual_region9 = false;
  bool outside_volume = false;
};

inline double region9_height(const Profile& profile, double diameter);

inline bool ascii_digit(char value) {
  return value >= '0' && value <= '9';
}

inline int three_digit_number(const std::string& value, std::size_t start) {
  return (value[start] - '0') * 100 + (value[start + 1] - '0') * 10 +
      value[start + 2] - '0';
}

inline bool parse_equation(const std::string& id, Equation* equation) {
  if (equation == nullptr || id.size() != 10 ||
      id.compare(3, 3, "CLK") != 0 ||
      !ascii_digit(id[0]) || !ascii_digit(id[1]) || !ascii_digit(id[2]) ||
      !ascii_digit(id[7]) || !ascii_digit(id[8]) || !ascii_digit(id[9])) {
    return false;
  }
  const int region = id[0] - '0';
  const int top_code = id[2] - '0';
  if (region != 8 && region != 9) {
    return false;
  }
  if (region == 8 && top_code != 0 && top_code != 1 && top_code != 4 && top_code != 7 &&
      top_code != 8 && top_code != 9) {
    return false;
  }
  Equation parsed;
  parsed.id = id;
  parsed.region = region;
  parsed.geocode = id[1] - '0';
  parsed.top_code = top_code;
  parsed.species = three_digit_number(id, 7);
  parsed.variant = id[6];
  parsed.old_region8 = region == 8 && top_code != 1;
  parsed.region8_on_region9 = region == 8 && top_code == 1;
  *equation = parsed;
  return true;
}

inline const double* find_row(const double* table, int rows, int columns,
                              int key, int key_column = 0) {
  for (int row = 0; row < rows; ++row) {
    const double* candidate = table + row * columns;
    if (static_cast<int>(candidate[key_column] + 0.5) == key) {
      return candidate;
    }
  }
  return nullptr;
}

inline int map_region8_species(int species) {
  if (species == 123 || species == 197) {
    return 100;
  }
  if (species == 268) {
    return 261;
  }
  if (species == 313 || species == 314 || species == 317 ||
      species == 650 || species == 651 || species == 691 ||
      species == 711 || species == 742 || species == 762 ||
      species == 920 || species == 930 || species == 545 ||
      species == 546) {
    return 300;
  }
  if (species == 521 || species == 550 || species == 580 ||
      species == 601 || species == 602 || species == 318) {
    return 500;
  }
  if (species == 804 || species == 817 || species == 820 ||
      species == 823 || species == 825 || species == 826 ||
      species == 830 || species == 834) {
    return 800;
  }
  return species;
}

inline const double* find_region8_geographic_row(int geocode, int species,
                                                  int* row_index = nullptr) {
  const double* table = clark_coefficients::r8_cf;
  for (int pass = 0; pass < 2; ++pass) {
    const int wanted_geocode = pass == 0 ? geocode : 9;
    if (pass == 1 && geocode == 9) {
      break;
    }
    for (int row = 0; row < 182; ++row) {
      const double* candidate = table + row * 18;
      if (static_cast<int>(candidate[0] + 0.5) == wanted_geocode &&
          static_cast<int>(candidate[1] + 0.5) == species) {
        if (row_index != nullptr) {
          *row_index = row;
        }
        return candidate;
      }
    }
  }
  return nullptr;
}

inline int region9_group(int species) {
  if (species < 300) {
    if (species >= 90 && species <= 99) {
      return 1090;
    }
    if (species >= 100 && species <= 199) {
      return 1100;
    }
    return 1000;
  }
  if (species >= 310 && species <= 329) {
    return 1310;
  }
  if (species >= 370 && species <= 379) {
    return 1370;
  }
  if (species >= 400 && species <= 410) {
    return 1400;
  }
  if (species >= 540 && species <= 549) {
    return 1540;
  }
  if (species == 740 || species == 742 || species == 744 ||
      species == 745 || species == 753) {
    return 1740;
  }
  if (species == 741 || species == 743 || species == 746 ||
      species == 752) {
    return 1750;
  }
  if (species >= 760 && species <= 769) {
    return 1760;
  }
  if (species >= 800 && species <= 899) {
    return 1800;
  }
  if (species >= 950 && species <= 954) {
    return 1950;
  }
  if (species >= 970 && species <= 979) {
    return 1970;
  }
  return 1300;
}

inline double region8_form_minimum(int group, double height) {
  if (group == 100) {
    if (height < 32.5) return 56.0;
    if (height < 37.5) return 64.0;
    if (height < 42.5) return 66.0;
    return 67.0;
  }
  if (group == 300) {
    if (height < 32.5) return 57.0;
    if (height < 37.5) return 60.0;
    if (height < 42.5) return 64.0;
    return 67.0;
  }
  if (height < 32.5) return 58.0;
  if (height < 37.5) return 65.0;
  if (height < 42.5) return 67.0;
  return 69.0;
}

inline bool form_minimum_exempt(int species) {
  return species == 221 || species == 222 || species == 544;
}

inline double derive_old_region8_upper(double dbh, double ht, int species,
                                        int group, const double* geographic,
                                        int geographic_index,
                                        double fixed_diameter) {
  const double* total = find_row(
      clark_coefficients::r8_total, 49, 7, species);
  const double* outside_total = find_row(
      clark_coefficients::r8_ototal, 49, 7, species);
  if (total == nullptr || outside_total == nullptr || ht < 20.0) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  double form = dbh *
      (geographic[13] + geographic[14] * std::pow(17.3 / ht, 2.0));
  form = std::max(form, 0.0);
  if (!form_minimum_exempt(species)) {
    const double minimum = dbh * region8_form_minimum(group, ht) * 0.01;
    if (ht < 47.5 && form < minimum) {
      form = minimum;
    }
  }
  double outside_form = (form - geographic[5]) / geographic[6];
  outside_form = std::max(outside_form, form);

  Profile outside;
  outside.total_ht = ht;
  outside.dbh_ib = dbh;
  outside.dib17 = outside_form;
  outside.r = outside_total[1];
  outside.c = outside_total[2];
  outside.e = outside_total[3];
  outside.p = outside_total[4];
  outside.b = outside_total[5];
  outside.a = outside_total[6];
  const double* outside_geographic =
      clark_coefficients::r8_cfo + geographic_index * 9;
  outside.a17 = outside_geographic[3];
  outside.b17 = outside_geographic[4];
  return std::max(region9_height(outside, fixed_diameter), 4.5);
}

inline double derive_old_region8_inverse_upper(const Equation& equation,
                                                double dbh, double ht) {
  const int species = map_region8_species(equation.species);
  const double* geographic = find_region8_geographic_row(
      equation.geocode, species);
  const double* total = find_row(
      clark_coefficients::r8_total, 49, 7, species);
  const double* limits = find_row(
      clark_coefficients::r8_dibmen, 49, 3, species);
  if (geographic == nullptr || total == nullptr || limits == nullptr) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  Profile inside;
  inside.total_ht = ht;
  inside.dbh_ib = geographic[3] + geographic[4] * dbh;
  inside.dib17 = dbh *
      (geographic[13] + geographic[14] * std::pow(17.3 / ht, 2.0));
  inside.r = total[1];
  inside.c = total[2];
  inside.e = total[3];
  inside.p = total[4];
  inside.b = total[5];
  inside.a = total[6];
  return region9_height(inside, limits[1]);
}

inline Result initialize_old_region8(const Equation& equation, double dbh,
                                     double ht, const Auxiliary& auxiliary,
                                     Profile* profile) {
  if (equation.geocode < 1 || equation.geocode > 9 || equation.geocode == 8) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 301};
  }
  const int species = map_region8_species(equation.species);
  int geographic_index = -1;
  const double* geographic = find_region8_geographic_row(
      equation.geocode, species, &geographic_index);
  if (geographic == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const int group = static_cast<int>(geographic[2] + 0.5);
  if (group != 100 && group != 300 && group != 500) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const double* diameter_limits = find_row(
      clark_coefficients::r8_dibmen, 49, 3, species);
  if (diameter_limits == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const double* coefficients = nullptr;
  int columns = 0;
  if (equation.top_code == 4) {
    coefficients = find_row(clark_coefficients::r8_four, 49, 6, species);
    columns = 6;
  } else if (equation.top_code == 7) {
    coefficients = find_row(clark_coefficients::r8_seven, 15, 6, species);
    columns = 6;
  } else if (equation.top_code == 9) {
    coefficients = find_row(clark_coefficients::r8_nine, 34, 6, species);
    columns = 6;
  } else {
    coefficients = find_row(clark_coefficients::r8_total, 49, 7, species);
    columns = 7;
  }
  if (coefficients == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 306};
  }

  Profile value;
  value.dbh_ob = dbh;
  value.input_ht = ht;
  value.total_ht = ht;
  value.species = species;
  value.species_group = group;
  value.top_code = equation.top_code;
  value.old_region8 = true;
  value.a4 = geographic[3];
  value.b4 = geographic[4];
  value.dbh_ib = value.a4 + value.b4 * dbh;
  value.r = coefficients[1];
  value.c = coefficients[2];
  value.e = coefficients[3];
  value.p = coefficients[4];
  if (columns == 7) {
    value.b = coefficients[5];
    value.a = coefficients[6];
    value.a17 = geographic[13];
    value.b17 = geographic[14];
  } else {
    value.q = coefficients[5];
    if (equation.top_code == 4) {
      value.a17 = geographic[7];
      value.b17 = geographic[8];
    } else if (equation.top_code == 7) {
      value.a17 = geographic[9];
      value.b17 = geographic[10];
    } else {
      value.a17 = geographic[11];
      value.b17 = geographic[12];
    }
  }
  value.fixed_diameter = diameter_limits[
      equation.top_code == 7 || equation.top_code == 9 ? 2 : 1];
  if (equation.top_code == 4 || equation.top_code == 7 ||
      equation.top_code == 9) {
    value.upper_ht = auxiliary.upper_ht1;
    if (!std::isfinite(value.upper_ht) || value.upper_ht < 0.1) {
      value.upper_ht = derive_old_region8_upper(
          dbh, ht, species, group, geographic, geographic_index,
          value.fixed_diameter);
    }
  } else {
    value.upper_ht = ht;
  }
  if (!std::isfinite(value.upper_ht) || value.upper_ht <= 4.5) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 54};
  }
  value.dib17 = dbh *
      (value.a17 + value.b17 * std::pow(17.3 / value.upper_ht, 2.0));
  *profile = value;
  return Result{0.0, 0};
}

inline Result initialize_actual_region9(const Equation& equation, double dbh,
                                        double ht, Profile* profile) {
  const double* coefficient0 = find_row(
      clark_coefficients::r9_0, 47, 9, equation.species);
  if (coefficient0 == nullptr) {
    coefficient0 = find_row(
        clark_coefficients::r9_0, 47, 9, region9_group(equation.species));
  }
  if (coefficient0 == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 301};
  }
  const int table_species = static_cast<int>(coefficient0[0] + 0.5);
  const double* coefficient_a = find_row(
      clark_coefficients::r9_a, 47, 4, table_species);
  if (coefficient_a == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 301};
  }

  Profile value;
  value.dbh_ob = dbh;
  value.input_ht = ht;
  value.total_ht = ht >= 17.4 ? ht : 17.4;
  value.species = equation.species;
  value.top_code = 0;
  value.actual_region9 = true;
  value.short_scale = ht < 17.4 ? ht / 17.3 : 1.0;
  value.a4 = coefficient_a[2];
  value.b4 = coefficient_a[3];
  value.dbh_ib = value.a4 + value.b4 * dbh;
  if (value.dbh_ib >= dbh || value.dbh_ib <= 0.0) {
    value.dbh_ib = std::max(dbh - 0.1, 0.1);
  }
  value.a17 = coefficient0[1];
  value.b17 = coefficient0[2];
  value.r = coefficient0[3];
  value.c = coefficient0[4];
  value.e = coefficient0[5];
  value.p = coefficient0[6];
  value.a = coefficient0[7];
  value.b = coefficient0[8];
  value.dib17 = value.dbh_ib *
      (value.a17 + value.b17 * std::pow(17.3 / value.total_ht, 2.0));
  value.dib17 = std::max(value.dib17, 0.1);
  *profile = value;
  return Result{0.0, 0};
}

inline Result initialize_region8_on_region9(const Equation& equation,
                                             double dbh, double ht,
                                             Profile* profile,
                                             Profile* outside = nullptr) {
  if (equation.geocode < 1 || equation.geocode > 9 || equation.geocode == 8) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 301};
  }
  const int species = map_region8_species(equation.species);
  int geographic_index = -1;
  const double* geographic = find_region8_geographic_row(
      equation.geocode, species, &geographic_index);
  if (geographic == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const int group = static_cast<int>(geographic[2] + 0.5);
  if (group != 100 && group != 300 && group != 500) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const double* limits = find_row(
      clark_coefficients::r8_dibmen, 49, 3, species);
  const double* total = find_row(
      clark_coefficients::r8_total, 49, 7, species);
  const double* outside_total = find_row(
      clark_coefficients::r8_ototal, 49, 7, species);
  if (limits == nullptr || total == nullptr || outside_total == nullptr) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 301};
  }

  Profile value;
  value.dbh_ob = dbh;
  value.input_ht = ht;
  value.total_ht = ht >= 17.4 ? ht : 17.4;
  value.short_scale = ht < 17.4 ? ht / 17.3 : 1.0;
  value.species = species;
  value.species_group = group;
  value.top_code = 1;
  value.r = total[1];
  value.c = total[2];
  value.e = total[3];
  value.p = total[4];
  value.b = total[5];
  value.a = total[6];
  value.a4 = geographic[3];
  value.b4 = geographic[4];
  value.a17 = geographic[13];
  value.b17 = geographic[14];
  value.fixed_diameter = limits[1];
  value.dbh_ib = std::max(value.a4 + value.b4 * dbh, value.fixed_diameter);
  value.dib17 = dbh *
      (value.a17 + value.b17 * std::pow(17.3 / value.total_ht, 2.0));
  if (value.dib17 < 0.0) {
    value.dib17 = 0.1;
  }
  if (!form_minimum_exempt(species)) {
    const double minimum = dbh * region8_form_minimum(group, ht) * 0.01;
    if (ht < 47.5 && value.dib17 < minimum) {
      value.dib17 = minimum;
    }
  }
  value.outside_volume = equation.variant == 'O' || equation.variant == '0';

  Profile outside_value = value;
  outside_value.r = outside_total[1];
  outside_value.c = outside_total[2];
  outside_value.e = outside_total[3];
  outside_value.p = outside_total[4];
  outside_value.b = outside_total[5];
  outside_value.a = outside_total[6];
  outside_value.dbh_ib = dbh;
  const double* outside_geographic =
      clark_coefficients::r8_cfo + geographic_index * 9;
  outside_value.a17 = outside_geographic[3];
  outside_value.b17 = outside_geographic[4];
  if (form_minimum_exempt(species)) {
    outside_value.dib17 = 0.0;
  } else {
    outside_value.dib17 = (value.dib17 - geographic[5]) / geographic[6];
  }
  if (outside_value.dib17 < value.dib17) {
    outside_value.dib17 = value.dib17;
  }
  if (outside_value.dib17 < 0.0 && value.total_ht > 17.2) {
    outside_value.dib17 =
        dbh * (value.total_ht - 17.3) / (value.total_ht - 4.5);
  }

  *profile = value;
  if (outside != nullptr) {
    *outside = outside_value;
  }
  return Result{0.0, 0};
}

inline Result initialize_profile(const Equation& equation, double dbh,
                                 double ht, const Auxiliary& auxiliary,
                                 Profile* profile,
                                 Profile* outside = nullptr) {
  (void) auxiliary.site_index;
  (void) auxiliary.basal_area;
  if (equation.old_region8) {
    return initialize_old_region8(equation, dbh, ht, auxiliary, profile);
  }
  if (equation.region8_on_region9) {
    return initialize_region8_on_region9(equation, dbh, ht, profile, outside);
  }
  return initialize_actual_region9(equation, dbh, ht, profile);
}

inline double region9_diameter(const Profile& profile, double height) {
  double stem_height = height;
  if (profile.r < 0.0 && std::fabs(stem_height - profile.total_ht) < 0.00001) {
    stem_height -= 0.1;
  }
  double stem_fraction = stem_height / profile.total_ht;
  if (std::log(1.0 - stem_fraction) < -20.0 / std::fabs(profile.r)) {
    stem_fraction = 1.0;
  }
  double squared = 0.0;
  if (stem_height < 4.5) {
    squared = profile.dbh_ib * profile.dbh_ib *
        (1.0 + (profile.c + profile.e / std::pow(profile.dbh_ib, 3.0)) *
         (std::pow(1.0 - stem_fraction, profile.r) -
          std::pow(1.0 - 4.5 / profile.total_ht, profile.r)) /
         (1.0 - std::pow(1.0 - 4.5 / profile.total_ht, profile.r)));
  } else if (stem_height <= 17.3) {
    squared = profile.dbh_ib * profile.dbh_ib -
        (profile.dbh_ib * profile.dbh_ib - profile.dib17 * profile.dib17) *
        (std::pow(1.0 - 4.5 / profile.total_ht, profile.p) -
         std::pow(1.0 - stem_height / profile.total_ht, profile.p)) /
        (std::pow(1.0 - 4.5 / profile.total_ht, profile.p) -
         std::pow(1.0 - 17.3 / profile.total_ht, profile.p));
  } else {
    const double position = (stem_height - 17.3) /
        (profile.total_ht - 17.3);
    const double indicator = stem_height <
        17.3 + profile.a * (profile.total_ht - 17.3) ? 1.0 : 0.0;
    squared = profile.dib17 * profile.dib17 *
        (profile.b * std::pow(position - 1.0, 2.0) +
         indicator * (1.0 - profile.b) / (profile.a * profile.a) *
         std::pow(profile.a - position, 2.0));
  }
  return squared > 0.0 ? std::sqrt(squared) : 0.0;
}

inline double old_region8_diameter(const Equation& equation,
                                   const Profile& profile, double height) {
  const double dbh2 = profile.dbh_ib * profile.dbh_ib;
  const double dib172 = profile.dib17 * profile.dib17;
  if (equation.top_code == 0 || equation.top_code == 8) {
    const bool stem = height < 4.5;
    const bool breast = height >= 4.5 && height <= 17.3;
    const bool top = height > 17.3;
    const bool inside_crown = height <
        17.3 + profile.a * (profile.input_ht - 17.3);
    double squared = 0.0;
    if (stem) {
      squared = dbh2 * (1.0 +
          (profile.c + profile.e / std::pow(profile.dbh_ib, 3.0)) *
          (std::pow(1.0 - height / profile.input_ht, profile.r) -
           std::pow(1.0 - 4.5 / profile.input_ht, profile.r)) /
          (1.0 - std::pow(1.0 - 4.5 / profile.input_ht, profile.r)));
    } else if (breast) {
      squared = dbh2 - (dbh2 - dib172) *
          (std::pow(1.0 - 4.5 / profile.input_ht, profile.p) -
           std::pow(1.0 - height / profile.input_ht, profile.p)) /
          (std::pow(1.0 - 4.5 / profile.input_ht, profile.p) -
           std::pow(1.0 - 17.3 / profile.input_ht, profile.p));
    } else if (top) {
      const double position = (height - 17.3) / (profile.input_ht - 17.3);
      squared = dib172 *
          (profile.b * std::pow(position - 1.0, 2.0) +
           (inside_crown ? 1.0 : 0.0) * (1.0 - profile.b) /
               (profile.a * profile.a) *
               std::pow(profile.a - position, 2.0));
    }
    return std::sqrt(squared);
  }

  if (height < 4.5) {
    return std::sqrt(dbh2 *
        (1.0 + (profile.c + profile.e / std::pow(profile.dbh_ib, 3.0)) *
         (std::pow(1.0 - height / profile.upper_ht, profile.r) -
          std::pow(1.0 - 4.5 / profile.upper_ht, profile.r)) /
         (1.0 - std::pow(1.0 - 4.5 / profile.upper_ht, profile.r))));
  }
  if (height <= 17.3) {
    if (profile.upper_ht > 17.3) {
      return std::sqrt(dbh2 - (dbh2 - dib172) *
          (std::pow(1.0 - 4.5 / profile.upper_ht, profile.p) -
           std::pow(1.0 - height / profile.upper_ht, profile.p)) /
          (std::pow(1.0 - 4.5 / profile.upper_ht, profile.p) -
           std::pow(1.0 - 17.3 / profile.upper_ht, profile.p)));
    }
    if (height <= profile.upper_ht) {
      return std::sqrt(dbh2 - (height - 4.5) /
          (profile.upper_ht - 4.5) *
          (dbh2 - profile.fixed_diameter * profile.fixed_diameter));
    }
    if (profile.input_ht > 0.0) {
      return std::sqrt(profile.fixed_diameter * profile.fixed_diameter -
          (height - profile.upper_ht) /
          (profile.input_ht - profile.upper_ht) *
          profile.fixed_diameter * profile.fixed_diameter);
    }
  }
  if (height > 17.3) {
    if (profile.upper_ht > 17.3) {
      if (height <= profile.upper_ht) {
        return std::sqrt(dib172 -
            (dib172 - profile.fixed_diameter * profile.fixed_diameter) *
            (1.0 - std::pow(
                (profile.upper_ht - height) / (profile.upper_ht - 17.3),
                profile.q)));
      }
      Equation total_equation = equation;
      total_equation.top_code = 8;
      Profile total_profile;
      Auxiliary no_auxiliary;
      const Result initialized = initialize_old_region8(
          total_equation, profile.dbh_ob, profile.input_ht, no_auxiliary,
          &total_profile);
      if (initialized.status != 0) {
        return std::numeric_limits<double>::quiet_NaN();
      }
      total_profile.dbh_ib = profile.dbh_ib;
      total_profile.dib17 = profile.dib17;
      const double at_upper = region9_diameter(total_profile, profile.upper_ht);
      const double factor = profile.fixed_diameter / at_upper;
      const double value = height < profile.input_ht
          ? region9_diameter(total_profile, height)
          : 0.0;
      return value * factor;
    }
    return std::sqrt(profile.fixed_diameter * profile.fixed_diameter -
        (height - profile.upper_ht) / (profile.input_ht - profile.upper_ht) *
        profile.fixed_diameter * profile.fixed_diameter);
  }
  return 0.0;
}

inline double inside_diameter(const Equation& equation, const Profile& profile,
                              double height) {
  if (profile.old_region8) {
    return old_region8_diameter(equation, profile, height);
  }
  return region9_diameter(profile, height);
}

inline double region9_height(const Profile& profile, double diameter) {
  const double dbh2 = profile.dbh_ib * profile.dbh_ib;
  const double dib172 = profile.dib17 * profile.dib17;
  const double diameter2 = diameter * diameter;
  const double g = std::pow(1.0 - 4.5 / profile.total_ht, profile.r);
  const double w = (profile.c + profile.e / std::pow(profile.dbh_ib, 3.0)) /
      (1.0 - g);
  const double x = std::pow(1.0 - 4.5 / profile.total_ht, profile.p);
  const double y = std::pow(1.0 - 17.3 / profile.total_ht, profile.p);
  const double z = (dbh2 - dib172) / (x - y);
  double stem_height = 0.0;
  if (diameter >= profile.dbh_ib) {
    const double value = (diameter2 / dbh2 - 1.0) / w + g;
    if (value > 0.0) {
      stem_height = profile.total_ht *
          (1.0 - std::pow(value, 1.0 / profile.r));
    }
  } else if (diameter >= profile.dib17) {
    const double value = x - (dbh2 - diameter2) / z;
    if (value > 0.0) {
      stem_height = profile.total_ht *
          (1.0 - std::pow(value, 1.0 / profile.p));
    }
  } else {
    const double indicator = diameter2 >
        profile.b * std::pow(profile.a - 1.0, 2.0) * dib172 ? 1.0 : 0.0;
    const double qa = profile.b + indicator *
        (1.0 - profile.b) / (profile.a * profile.a);
    const double qb = -2.0 * profile.b - indicator *
        2.0 * (1.0 - profile.b) / profile.a;
    const double qc = profile.b + (1.0 - profile.b) * indicator -
        diameter2 / dib172;
    const double discriminant = qb * qb - 4.0 * qa * qc;
    if (discriminant > 0.0) {
      stem_height = 17.3 + (profile.total_ht - 17.3) *
          (-qb - std::sqrt(discriminant)) / (2.0 * qa);
    }
  }
  return stem_height;
}

inline double old_region8_upper_height(const Profile& profile,
                                       double diameter) {
  const double diameter2 = diameter * diameter;
  const double d2 = profile.dbh_ib * profile.dbh_ib;
  const double d3 = d2 * profile.dbh_ib;
  const double f2 = profile.dib17 * profile.dib17;
  const double fixed2 = profile.fixed_diameter * profile.fixed_diameter;
  const double g = std::pow(1.0 - 4.5 / profile.upper_ht, profile.r);
  const double w = (profile.c + profile.e / d3) / (1.0 + g);
  const double x = std::pow(1.0 - 4.5 / profile.upper_ht, profile.p);
  const double y = std::pow(1.0 - 17.3 / profile.upper_ht, profile.p);
  const double z = (d2 - f2) / (x - y);
  const double j = std::pow(1.0 - 17.3 / profile.upper_ht, profile.q);
  const double rr = (f2 - fixed2) / j;
  if (diameter2 <= f2) {
    return profile.upper_ht *
        (1.0 - std::pow(j - (f2 - diameter2) / rr, 1.0 / profile.q));
  }
  if (diameter2 <= d2) {
    return profile.upper_ht *
        (1.0 - std::pow(x - (d2 - diameter2) / z, 1.0 / profile.p));
  }
  return profile.upper_ht *
      (1.0 - std::pow((diameter2 / d2 - 1.0) / w + g,
                      1.0 / profile.r));
}

inline double source_old_region8_upper_height(const Profile& profile,
                                              double diameter) {
  const double diameter2 = diameter * diameter;
  const double d2 = profile.dbh_ib * profile.dbh_ib;
  const double d3 = d2 * profile.dbh_ib;
  const double f2 = profile.dib17 * profile.dib17;
  const double fixed2 = profile.fixed_diameter * profile.fixed_diameter;
  const double g = std::pow(1.0 - 4.5 / profile.upper_ht, profile.r);
  const double w = (profile.c + profile.e / d3) / (1.0 + g);
  const double x = std::pow(1.0 - 4.5 / profile.upper_ht, profile.p);
  const double y = std::pow(1.0 - 17.3 / profile.upper_ht, profile.p);
  const double z = (d2 - f2) / (x - y);
  const double j = std::pow(1.0 - 17.3 / profile.upper_ht, profile.q);
  const double rr = (f2 - fixed2) / j;
  double value = 0.0;
  // R8CLKHT uses independent IF blocks. When f2 is greater than d2,
  // more than one root is accumulated.
  if (diameter2 >= d2) {
    value += profile.upper_ht *
        (1.0 - std::pow((diameter2 / d2 - 1.0) / w + g,
                        1.0 / profile.r));
  }
  if (diameter2 < d2 && diameter2 >= f2) {
    value += profile.upper_ht *
        (1.0 - std::pow(x - (d2 - diameter2) / z, 1.0 / profile.p));
  }
  if (diameter2 < f2) {
    value += profile.upper_ht *
        (1.0 - std::pow(j - (f2 - diameter2) / rr, 1.0 / profile.q));
  }
  return value;
}

inline bool source_old_region8_adds_roots(const Equation& equation,
                                          double dbh, double ht,
                                          double diameter,
                                          const Auxiliary& auxiliary) {
  if (!equation.old_region8 || equation.top_code == 0 ||
      equation.top_code == 8) {
    return false;
  }
  Auxiliary resolved_auxiliary = auxiliary;
  if (!std::isfinite(resolved_auxiliary.upper_ht1) ||
      resolved_auxiliary.upper_ht1 < 0.1) {
    resolved_auxiliary.upper_ht1 = derive_old_region8_inverse_upper(
        equation, dbh, ht);
  }
  Profile profile;
  const Result initialized = initialize_profile(
      equation, dbh, ht, resolved_auxiliary, &profile);
  if (initialized.status != 0 || diameter <= profile.fixed_diameter) {
    return false;
  }
  const double diameter2 = diameter * diameter;
  const double d2 = profile.dbh_ib * profile.dbh_ib;
  const double f2 = profile.dib17 * profile.dib17;
  return diameter2 >= d2 && diameter2 < f2;
}

inline Result height_at_diameter(const Equation& equation, double dbh,
                                 double ht, double diameter,
                                 const Auxiliary& auxiliary) {
  Auxiliary resolved_auxiliary = auxiliary;
  if (equation.old_region8 && equation.top_code != 0 &&
      equation.top_code != 8 &&
      (!std::isfinite(resolved_auxiliary.upper_ht1) ||
       resolved_auxiliary.upper_ht1 < 0.1)) {
    resolved_auxiliary.upper_ht1 = derive_old_region8_inverse_upper(
        equation, dbh, ht);
  }
  Profile profile;
  const Result initialized = initialize_profile(
      equation, dbh, ht, resolved_auxiliary, &profile);
  if (initialized.status != 0) {
    return initialized;
  }
  double value = 0.0;
  if (!profile.old_region8 || equation.top_code == 0 || equation.top_code == 8) {
    value = region9_height(profile, diameter);
  } else if (diameter <= profile.fixed_diameter) {
    Equation total_equation = equation;
    total_equation.top_code = 8;
    Profile total_profile;
    Auxiliary no_auxiliary;
    const Result total_initialized = initialize_old_region8(
        total_equation, dbh, ht, no_auxiliary, &total_profile);
    if (total_initialized.status != 0) {
      return total_initialized;
    }
    value = region9_height(total_profile, diameter);
  } else {
    value = old_region8_upper_height(profile, diameter);
  }
  if (!std::isfinite(value)) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 54};
  }
  if (value < 1.0) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 100};
  }
  return Result{value, 0};
}

inline Result source_height_at_diameter(const Equation& equation, double dbh,
                                        double ht, double diameter,
                                        const Auxiliary& auxiliary) {
  Auxiliary resolved_auxiliary = auxiliary;
  if (equation.old_region8 && equation.top_code != 0 &&
      equation.top_code != 8 &&
      (!std::isfinite(resolved_auxiliary.upper_ht1) ||
       resolved_auxiliary.upper_ht1 < 0.1)) {
    resolved_auxiliary.upper_ht1 = derive_old_region8_inverse_upper(
        equation, dbh, ht);
  }
  Profile profile;
  const Result initialized = initialize_profile(
      equation, dbh, ht, resolved_auxiliary, &profile);
  if (initialized.status != 0) return initialized;
  double value = 0.0;
  if (diameter <= profile.fixed_diameter) {
    Equation total_equation = equation;
    total_equation.top_code = 8;
    Profile total_profile;
    Auxiliary no_auxiliary;
    const Result total_initialized = initialize_old_region8(
        total_equation, dbh, ht, no_auxiliary, &total_profile);
    if (total_initialized.status != 0) return total_initialized;
    value = region9_height(total_profile, diameter);
  } else {
    value = source_old_region8_upper_height(profile, diameter);
  }
  return std::isfinite(value)
      ? Result{value, 0}
      : Result{std::numeric_limits<double>::quiet_NaN(), 54};
}

inline double cubic_volume(const Profile& profile, double lower,
                           double upper) {
  if (upper <= 0.0) {
    return 0.0;
  }
  const double g = std::pow(1.0 - 4.5 / profile.total_ht, profile.r);
  const double w = (profile.c + profile.e / std::pow(profile.dbh_ib, 3.0)) /
      (1.0 - g);
  const double x = std::pow(1.0 - 4.5 / profile.total_ht, profile.p);
  double y;
  if ((1.0 - 17.3 / profile.total_ht) < 0.005748 && profile.p > 14.0) {
    y = 0.0;
  } else {
    y = std::pow(1.0 - 17.3 / profile.total_ht, profile.p);
  }
  const double z = (profile.dbh_ib * profile.dbh_ib -
                    profile.dib17 * profile.dib17) / (x - y);
  const double t = profile.dbh_ib * profile.dbh_ib - z * x;
  const double l1 = std::max(lower, 0.0);
  const double u1 = std::min(upper, 4.5);
  const double l2 = std::max(lower, 4.5);
  const double u2 = std::min(upper, 17.3);
  const double l3 = std::max(lower, 17.3);
  const double u3 = std::min(profile.total_ht, upper);
  double v1 = 0.0;
  double v2 = 0.0;
  double v3 = 0.0;
  if (lower < 4.5) {
    v1 = profile.dbh_ib * profile.dbh_ib *
        ((1.0 - g * w) * (u1 - l1) +
         w * (std::pow(1.0 - l1 / profile.total_ht, profile.r) *
                  (profile.total_ht - l1) -
              std::pow(1.0 - u1 / profile.total_ht, profile.r) *
                  (profile.total_ht - u1)) /
             (profile.r + 1.0));
  }
  if (lower < 17.3 && upper > 4.5) {
    const double lower_term =
        std::pow(1.0 - l2 / profile.total_ht, profile.p) *
        (profile.total_ht - l2);
    double upper_term = 0.0;
    if (!((1.0 - u2 / profile.total_ht) < 0.005748 && profile.p > 14.0)) {
      upper_term = std::pow(1.0 - u2 / profile.total_ht, profile.p) *
          (profile.total_ht - u2);
    }
    v2 = t * (u2 - l2) + z * (lower_term - upper_term) /
        (profile.p + 1.0);
  }
  if (upper > 17.3) {
    const double lower_inside =
        (l3 - 17.3) < profile.a * (profile.total_ht - 17.3) ? 1.0 : 0.0;
    const double upper_inside =
        (u3 - 17.3) < profile.a * (profile.total_ht - 17.3) ? 1.0 : 0.0;
    const double span = profile.total_ht - 17.3;
    v3 = profile.dib17 * profile.dib17 *
        (profile.b * (u3 - l3) -
         profile.b * (std::pow(u3 - 17.3, 2.0) -
                      std::pow(l3 - 17.3, 2.0)) / span +
         (profile.b / 3.0) * (std::pow(u3 - 17.3, 3.0) -
                              std::pow(l3 - 17.3, 3.0)) /
             (span * span) +
         lower_inside / 3.0 * (1.0 - profile.b) /
             (profile.a * profile.a) *
             std::pow(profile.a * span - (l3 - 17.3), 3.0) /
             (span * span) -
         upper_inside / 3.0 * (1.0 - profile.b) /
             (profile.a * profile.a) *
             std::pow(profile.a * span - (u3 - 17.3), 3.0) /
             (span * span));
  }
  const double volume = 0.005454154 * (v1 + v2 + v3);
  return volume < 0.0 ? 0.0 : volume;
}

inline double old_region8_cubic_volume(const Equation& equation,
                                       const Profile& profile, double lower,
                                       double upper) {
  if (equation.top_code == 0 || equation.top_code == 8) {
    return cubic_volume(profile, lower, upper);
  }
  if (upper <= 0.0) return 0.0;
  const double top_ht = profile.upper_ht;
  const double g = std::pow(1.0 - 4.5 / top_ht, profile.r);
  const double w = (profile.c + profile.e / std::pow(profile.dbh_ib, 3.0)) /
      (1.0 - g);
  const double x = std::pow(1.0 - 4.5 / top_ht, profile.p);
  const double y = std::pow(1.0 - 17.3 / top_ht, profile.p);
  const double z = (profile.dbh_ib * profile.dbh_ib -
                    profile.dib17 * profile.dib17) / (x - y);
  const double t = profile.dbh_ib * profile.dbh_ib - z * x;
  const double j = std::pow(1.0 - 17.3 / top_ht, profile.q);
  const double rr = (profile.dib17 * profile.dib17 -
                     profile.fixed_diameter * profile.fixed_diameter) / j;
  const double n = profile.dib17 * profile.dib17 - rr * j;
  const double l1 = std::max(lower, 0.0);
  const double u1 = std::min(upper, 4.5);
  const double l2 = std::max(lower, 4.5);
  const double u2 = std::min(upper, 17.3);
  const double l3 = std::max(lower, 17.3);
  const double u3 = std::min(top_ht, upper);
  double v1 = 0.0;
  double v2 = 0.0;
  double v3 = 0.0;
  if (lower < 4.5) {
    v1 = profile.dbh_ib * profile.dbh_ib *
        ((1.0 - g * w) * (u1 - l1) +
         w * (std::pow(1.0 - l1 / top_ht, profile.r) * (top_ht - l1) -
              std::pow(1.0 - u1 / top_ht, profile.r) * (top_ht - u1)) /
             (profile.r + 1.0));
  }
  if (lower < 17.3 && upper > 4.5) {
    v2 = t * (u2 - l2) +
        z * (std::pow(1.0 - l2 / top_ht, profile.p) * (top_ht - l2) -
             std::pow(1.0 - u2 / top_ht, profile.p) * (top_ht - u2)) /
            (profile.p + 1.0);
  }
  if (upper > 17.3) {
    v3 = n * (u3 - l3) +
        rr * (std::pow(1.0 - l3 / top_ht, profile.q) * (top_ht - l3) -
              std::pow(1.0 - u3 / top_ht, profile.q) * (top_ht - u3)) /
            (profile.q + 1.0);
  }
  const double value = .005454154 * (v1 + v2 + v3);
  return value < 0.0 ? 0.0 : value;
}

inline double region9_correction(int species) {
  if (species < 300) {
    return 1.04;
  }
  if ((species >= 741 && species <= 746) || species == 621) {
    return 1.0;
  }
  return 1.1;
}

template <std::size_t Size>
inline bool raile_contains(int species, const int (&values)[Size]) {
  for (std::size_t index = 0; index < Size; ++index) {
    if (values[index] == species) return true;
  }
  return false;
}

inline int raile_reference_species(int species) {
  static constexpr int conifers[] = {
      10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 40, 41, 42, 43,
      50, 51, 52, 53, 54, 55, 56, 57, 64, 67, 68, 70, 71, 72, 73, 81,
      200, 201, 202, 211, 212, 220, 221, 222, 223, 230, 231, 232, 240,
      241, 242, 250, 251, 252, 260, 261, 262, 263, 264};
  static constexpr int soft_hardwoods[] = {
      100, 101, 102, 103, 104, 105, 107, 108, 109, 110, 111, 112, 113,
      114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
      127, 128, 129, 130, 131, 132, 135, 136};
  static constexpr int eastern_softwoods[] = {
      90, 91, 92, 93, 94, 95, 96, 97, 98};
  static constexpr int woodland_hardwoods[] = {
      350, 351, 352, 353, 355, 740, 741, 742, 743, 744, 745, 746, 747,
      748, 749, 752, 753, 920, 921, 922, 923, 924, 925, 926, 927, 928,
      929};
  static constexpr int northern_hardwoods[] = {
      310, 311, 312, 313, 315, 316, 317, 319, 320, 370, 371, 372, 373,
      374, 375, 377, 378, 379};
  static constexpr int hardwoods[] = {
      314, 318, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410,
      411, 412, 413, 531, 800, 801, 802, 804, 805, 806, 807, 808, 809,
      811, 812, 813, 815, 816, 817, 818, 819, 820, 821, 822, 823, 824,
      825, 826, 827, 828, 830, 831, 832, 833, 834, 835, 836, 837, 838,
      839, 840, 841, 842, 844, 845, 851};
  static constexpr int other_hardwoods[] = {
      58, 59, 60, 61, 62, 63, 65, 66, 69, 106, 133, 134, 138, 140, 141,
      143, 300, 303, 304, 321, 322, 363, 475, 523, 755, 756, 757, 758,
      803, 810, 814, 829, 843, 846, 847, 867, 902, 990};
  if (raile_contains(species, conifers)) return 261;
  if (raile_contains(species, soft_hardwoods)) return 125;
  if (raile_contains(species, eastern_softwoods)) return 90;
  if (raile_contains(species, woodland_hardwoods)) return 746;
  if (raile_contains(species, northern_hardwoods)) return 317;
  if (raile_contains(species, hardwoods) ||
      raile_contains(species, other_hardwoods)) return 833;
  return 544;
}

inline double raile_stump_volume(int species, double dbh, double stump) {
  static constexpr int species_values[] = {
      129, 125, 105, 90, 94, 95, 12, 261, 241, 802, 833, 531,
      371, 318, 317, 544, 543, 375, 743, 746, 950, 740, 970};
  static constexpr double dib_a[] = {
      .91385, .90698, .90973, .94804, .95487, .94122, .93793, .91400,
      .94698, .91130, .92267, .96731, .94423, .93818, .94181, .91979,
      .93502, .93763, .91625, .91882, .92442, .92736, .93257};
  static constexpr double dib_b[] = {
      .11182, .08469, .07926, .13722, .15664, .11781, .14553, .11975,
      .18702, .14907, .12506, .14082, .14335, .11424, .10740, .12152,
      .17071, .10640, .06478, .08593, .14240, .17626, .15803};
  int mapped = species;
  std::size_t coefficient = 23;
  for (std::size_t index = 0; index < 23; ++index) {
    if (species_values[index] == mapped) {
      coefficient = index;
      break;
    }
  }
  if (coefficient == 23) {
    mapped = raile_reference_species(species);
    for (std::size_t index = 0; index < 23; ++index) {
      if (species_values[index] == mapped) {
        coefficient = index;
        break;
      }
    }
  }
  if (coefficient == 23) return 0.0;
  if (stump < .01) stump = 1.0;
  const double a = dib_a[coefficient];
  const double b = dib_b[coefficient];
  const auto primitive = [a, b](double height) {
    return (a - b) * (a - b) * height +
        11.0 * b * (a - b) * std::log(height + 1.0) -
        30.25 * b * b / (height + 1.0);
  };
  return .0054541539 * dbh * dbh * (primitive(stump) - primitive(0.0));
}

inline Result volume(const Equation& equation, double dbh, double ht,
                     double lower, double upper, const Auxiliary& auxiliary) {
  Profile profile;
  const Result initialized = initialize_profile(
      equation, dbh, ht, auxiliary, &profile);
  if (initialized.status != 0) {
    return initialized;
  }
  const double value = profile.old_region8
      ? old_region8_cubic_volume(equation, profile, lower, upper)
      : cubic_volume(profile, lower, upper);
  if (!std::isfinite(value)) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 54};
  }
  return Result{value, 0};
}

inline Result driver_total_volume(const Equation& equation, double dbh,
                                  double ht, double lower, double upper,
                                  const Auxiliary& auxiliary) {
  (void) upper;
  Profile profile;
  Profile outside;
  const Result initialized = initialize_profile(
      equation, dbh, ht, auxiliary, &profile, &outside);
  if (initialized.status != 0) return initialized;
  if (profile.old_region8) return Result{0.0, 0};
  const Profile& volume_profile = profile.outside_volume ? outside : profile;
  double value = cubic_volume(
      volume_profile, lower, volume_profile.total_ht) *
      profile.short_scale;
  if (equation.region == 9) value *= region9_correction(equation.species);
  if (value > 0.0) value = std::round(value * 10.0) / 10.0;
  if (!std::isfinite(value)) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 54};
  }
  return Result{value, 0};
}

inline Result driver_stump_volume(const Equation& equation, double dbh,
                                  double ht, double stump,
                                  const Auxiliary& auxiliary) {
  Profile profile;
  const Result initialized = initialize_profile(
      equation, dbh, ht, auxiliary, &profile);
  if (initialized.status != 0) return initialized;
  double value = cubic_volume(profile, 0.0, stump);
  if (!(value > 0.0) && stump > 0.0) {
    const double stump_dib = inside_diameter(equation, profile, stump);
    if (stump_dib > 0.0) value = .005454154 * stump_dib * stump_dib * stump;
  }
  if (!(value > 0.0) && stump > 0.0) {
    value = raile_stump_volume(equation.species, dbh, stump);
  }
  if (!std::isfinite(value)) {
    return Result{std::numeric_limits<double>::quiet_NaN(), 54};
  }
  return Result{std::max(0.0, value), 0};
}

}  // namespace clark
}  // namespace treevolume

#endif
