#ifndef TREEVOLUME_FLEWELLING_HPP
#define TREEVOLUME_FLEWELLING_HPP

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <limits>
#include <string>

#include <treevolume/flewelling_coefficients.hpp>

namespace treevolume {
namespace flewelling {

namespace coefficient = flewelling_coefficients;

constexpr double kBreastHeight = 4.5;
constexpr double kPiNvel = 3.1416;
constexpr double kSmalian = 0.00272708;
// f_west.f line 263 uses F(48), but SHP_W4 has no DATA initializer for it.
// The Fortran standard leaves that element undefined. The pinned gfortran
// build supplies zero from static storage, so the port uses zero explicitly.
constexpr double kWest4UndefinedF48 = 0.0;

struct Equation {
  int jsp = 0;
  int species = 0;
  char geocode = '\0';
  std::array<char, 3> geosub{{'0', '0', '\0'}};
  bool three_point = false;
  bool condition_upper = false;
  bool library_rejected = false;
};

struct Auxiliary {
  double bark_ratio = 0.0;
  double upper_ht1 = std::numeric_limits<double>::quiet_NaN();
  double upper_d1 = std::numeric_limits<double>::quiet_NaN();
  double upper_ht2 = std::numeric_limits<double>::quiet_NaN();
  double upper_d2 = std::numeric_limits<double>::quiet_NaN();
  int upper_bark = 0;
};

struct Result {
  double value = std::numeric_limits<double>::quiet_NaN();
  int status = 0;
};

struct Profile {
  Equation equation;
  double dbh = 0.0;
  double ht = 0.0;
  double dbt_bh = 0.0;
  double scale = 1.0;
  std::array<double, 6> form{{}};
  std::array<double, 4> heights{{}};
  std::array<double, 12> taper{{}};
  int extra_count = 0;
  std::array<double, 2> extra_height{{}};
  std::array<double, 2> extra_z{{}};
  std::array<double, 2> inverse_z{{}};
  std::array<double, 3> modifier{{}};
};

inline double logistic(double value) {
  const double exponential = std::exp(value);
  return exponential / (1.0 + exponential);
}

inline double nvel_real(double value) {
  // The double oracle build promotes NVEL's default REAL declarations.
  // Keeping this boundary explicit documents that the kernel uses doubles.
  return value;
}

inline double clamp(double value, double lower, double upper) {
  return std::max(lower, std::min(value, upper));
}

inline bool geosub_is(const Equation& equation, const char* value) {
  return equation.geosub[0] == value[0] && equation.geosub[1] == value[1];
}

inline int parse_three_digits(const std::string& value, std::size_t start) {
  if (value.size() < start + 3) {
    return 0;
  }
  int result = 0;
  for (std::size_t index = start; index < start + 3; ++index) {
    if (value[index] < '0' || value[index] > '9') {
      return 0;
    }
    result = result * 10 + value[index] - '0';
  }
  return result;
}

inline bool parse_equation(const std::string& id, Equation* equation) {
  if (id.size() != 10 || equation == nullptr || id[3] != 'F') {
    return false;
  }
  Equation parsed;
  parsed.species = parse_three_digits(id, 7);
  parsed.geocode = id[0];
  parsed.geosub = {{id[1], id[2], '\0'}};
  if (geosub_is(parsed, "OO") || geosub_is(parsed, "oo")) {
    parsed.geosub = {{'0', '0', '\0'}};
  }
  const std::string form = id.substr(3, 3);
  parsed.three_point = form == "FW3" || form == "F32" || form == "F33";
  parsed.condition_upper = form == "FW3" || form == "F33";
  if (form != "FW2" && !parsed.three_point) {
    return false;
  }

  const char region = id[0];
  const int species = parsed.species;
  if (region == 'I' || region == 'i' || region == '1') {
    if (species == 202 || species == 205 || species == 204) parsed.jsp = 11;
    else if (species == 73 || species == 70) parsed.jsp = 12;
    else if (species == 17) parsed.jsp = 13;
    else if (species == 122) parsed.jsp = 14;
    else if (species == 108) parsed.jsp = 15;
    else if (species == 242 || species == 240) parsed.jsp = 16;
    else if (species == 260 || species == 263 || species == 264) parsed.jsp = 17;
    else if (species == 119) parsed.jsp = 18;
    else if (species == 93 || species == 90) parsed.jsp = 19;
    else if (species == 19) parsed.jsp = 20;
    else if (species == 12) parsed.jsp = 21;
  } else if (region == 'F' || region == 'f') {
    if (species == 202 || species == 205 || species == 204) parsed.jsp = 3;
    else if (species == 263) parsed.jsp = 4;
    else if (species == 242) parsed.jsp = 5;
  } else if (region == '2') {
    if (species == 122) parsed.jsp = geosub_is(parsed, "03") ? 22 : 23;
    else if (species == 108) parsed.jsp = 25;
    else if (species == 202) parsed.jsp = 26;
    else if (species == 15) parsed.jsp = 27;
    else if (species == 746) parsed.jsp = 28;
  } else if (region == '4') {
    if (geosub_is(parsed, "07") && species == 93) parsed.jsp = 24;
    else if (geosub_is(parsed, "07") && species == 122) parsed.jsp = 23;
  } else if (region == '3') {
    if (geosub_is(parsed, "00")) {
      if (species == 122) parsed.jsp = 29;
      else if (species == 202) parsed.jsp = 26;
    } else if (geosub_is(parsed, "01")) {
      if (species == 122) parsed.jsp = 29;
      else if (species == 108) parsed.jsp = 25;
      else if (species == 202) parsed.jsp = 26;
      else if (species == 15) parsed.jsp = 27;
    }
  } else if (region == 'A' || region == 'a') {
    if (species == 42) parsed.jsp = 31;
    else if (species == 242) parsed.jsp = 32;
    else if (species == 98) parsed.jsp = geosub_is(parsed, "02") ? 35 : 33;
    else if (species == 263 || species == 260 || species == 264) {
      parsed.jsp = geosub_is(parsed, "02") ? 36 : 34;
    }
  }
  if (parsed.jsp == 0) {
    parsed.library_rejected = true;
  }
  *equation = parsed;
  return true;
}

template <std::size_t Size>
inline void copy_values(std::array<double, 50>* destination, int first,
                        const double (&source)[Size]) {
  for (std::size_t index = 0; index < Size; ++index) {
    (*destination)[static_cast<std::size_t>(first) + index] = source[index];
  }
}

inline void finish_standard_shape(const std::array<double, 10>& u,
                                  Profile* profile, bool direct_crown,
                                  bool cap_lower_inflection = true) {
  profile->form[0] = nvel_real(logistic(u[1]));
  profile->form[1] = nvel_real(logistic(u[2]));
  profile->form[2] = nvel_real(logistic(u[3]));
  profile->form[3] = nvel_real(logistic(u[4]));
  profile->form[4] = nvel_real(u[5] <= 7.0 ? 0.5 + 0.5 * logistic(u[5]) : 1.0);
  profile->form[5] = nvel_real(u[6]);
  profile->heights[0] = nvel_real(cap_lower_inflection
      ? std::min(0.5, logistic(u[7])) : logistic(u[7]));
  profile->heights[3] = nvel_real(u[9]);
  profile->heights[1] = nvel_real(profile->heights[0] + profile->heights[3]);
  profile->heights[2] = nvel_real(direct_crown
      ? u[8]
      : profile->heights[1] + (1.0 - profile->heights[1]) * logistic(u[8]));
  if (direct_crown && profile->heights[2] < profile->heights[1] + 0.01) {
    profile->heights[2] = nvel_real(std::min(profile->heights[1] + 0.01,
                                   (profile->heights[1] + 1.0) / 2.0));
  }
}

inline void shape_from_matrix(const double* values, double diameter, double ht,
                              bool lodgepole, Profile* profile) {
  std::array<double, 50> f{{}};
  for (int index = 10; index <= 47; ++index) {
    f[static_cast<std::size_t>(index)] = values[index - 10];
  }
  const double log_ht = std::log(ht);
  const double median = f[10] * std::pow(ht - kBreastHeight,
                                         f[11] + f[12] * ht);
  const double dform = diameter / median - 1.0;
  std::array<double, 10> u{{}};
  u[7] = f[13] + f[14] * log_ht + f[15] * dform;
  double u9t = clamp(f[18] + f[19] * log_ht + f[20] * dform, -7.0, 7.0);
  u[9] = f[16] * logistic(u9t);
  u[8] = std::min(0.99, f[21] + f[22] * ht + f[23] * log_ht + f[24] * dform);
  u[1] = f[25] + f[26] * log_ht + f[27] * dform +
      f[28] * dform * log_ht;
  u[2] = f[29] + f[30] * dform + f[31] * log_ht +
      f[32] * dform * log_ht + f[33] * diameter;
  u[3] = lodgepole
      ? f[34] + f[35] * dform + f[36] * (1.0 - std::exp(f[37] * ht))
      : f[34] + f[35] * dform + f[36] * log_ht +
            f[37] * log_ht * dform;
  u[4] = f[38] + f[39] * dform + f[40] * log_ht + f[41] * diameter;
  u[5] = f[42] + f[43] * log_ht;
  u[6] = clamp(f[45] + f[46] * dform + f[47] * log_ht, 1.005, 10.0);
  for (int index = 1; index <= 5; ++index) {
    u[static_cast<std::size_t>(index)] = clamp(
        u[static_cast<std::size_t>(index)], -7.0, index == 5 ? 7.1 : 7.0);
  }
  u[7] = clamp(u[7], -7.0, 7.0);
  u[9] = clamp(u[9], 0.0, 0.3);
  finish_standard_shape(u, profile, true);
}

inline const double* ingy_shape_values(int jsp) {
  switch (jsp) {
    case 11: return coefficient::f_ingy_37;
    case 12: return coefficient::f_ingy_58;
    case 13: return coefficient::f_ingy_85;
    case 14: return coefficient::f_ingy_108;
    case 15: return coefficient::f_ingy_133;
    case 16: return coefficient::f_ingy_148;
    case 17: return coefficient::f_ingy_163;
    case 18: return coefficient::f_ingy_178;
    case 19: return coefficient::f_ingy_193;
    case 20: return coefficient::f_ingy_215;
    case 21: return coefficient::f_ingy_230;
    default: return nullptr;
  }
}

inline void apply_ingy_subregion(const Equation& equation,
                                 std::array<double, 38>* values) {
  const char* code = equation.geosub.data();
  auto set_value = [values](int index, double value) {
    (*values)[static_cast<std::size_t>(index - 10)] = value;
  };
  if (equation.jsp == 11 && (geosub_is(equation, "15") ||
                             geosub_is(equation, "03"))) {
    set_value(25, coefficient::f_ingy_53[0]);
    set_value(34, coefficient::f_ingy_53[1]);
    set_value(38, coefficient::f_ingy_53[2]);
  } else if (equation.jsp == 19 && geosub_is(equation, "15")) {
    set_value(25, coefficient::f_ingy_210[0]);
    set_value(34, coefficient::f_ingy_210[1]);
    set_value(38, coefficient::f_ingy_210[2]);
  } else if (equation.jsp == 13 && (geosub_is(equation, "15") ||
                                    geosub_is(equation, "03"))) {
    std::copy(coefficient::f_ingy_260, coefficient::f_ingy_260 + 38,
              values->begin());
  } else if (equation.jsp == 13 && !geosub_is(equation, "00")) {
    static constexpr const char* codes[] = {
        "11", "11", "12", "12", "13", "13", "14", "14",
        "00", "00", "00", "00", "00", "00"};
    for (int index = 0; index < 14; ++index) {
      if (code[0] == codes[index][0] && code[1] == codes[index][1]) {
        set_value(static_cast<int>(coefficient::f_ingy_99[index]),
                  coefficient::f_ingy_101[index]);
      }
    }
  } else if (equation.jsp == 14 && !geosub_is(equation, "00")) {
    static constexpr const char* codes[] = {
        "11", "11", "12", "12", "13", "13", "14", "14",
        "15", "15", "21", "21", "22", "22"};
    // SHP_C2 declares 16 entries but its DO 60 loop intentionally visits 14.
    for (int index = 0; index < 14; ++index) {
      if (code[0] == codes[index][0] && code[1] == codes[index][1]) {
        set_value(static_cast<int>(coefficient::f_ingy_122[index]),
                  coefficient::f_ingy_125[index]);
      }
    }
  } else if (equation.jsp == 12 && !geosub_is(equation, "00")) {
    static constexpr const char* codes[] = {
        "11", "11", "11", "12", "12", "12", "13", "13", "13",
        "14", "14", "14", "21", "21", "21", "22", "22", "22",
        "23", "23", "23"};
    for (int index = 0; index < 21; ++index) {
      if (code[0] == codes[index][0] && code[1] == codes[index][1]) {
        set_value(static_cast<int>(coefficient::f_ingy_72[index]),
                  coefficient::f_ingy_75[index]);
      }
    }
  }
}

inline void shape_ingy(const Equation& equation, double dbh, double ht,
                       Profile* profile) {
  std::array<double, 38> values{{}};
  const double* source = ingy_shape_values(equation.jsp);
  std::copy(source, source + 38, values.begin());
  apply_ingy_subregion(equation, &values);
  shape_from_matrix(values.data(), dbh, ht, equation.jsp == 15, profile);
}

inline const double* other_shape_values(int jsp) {
  switch (jsp) {
    case 23: return coefficient::f_other_192;
    case 24: return coefficient::f_other_207;
    case 25: return coefficient::f_other_223;
    case 26: return coefficient::f_other_239;
    case 27: return coefficient::f_other_254;
    case 28: return coefficient::f_other_269;
    case 29: return coefficient::f_other_283;
    default: return nullptr;
  }
}

inline void shape_other(const Equation& equation, double dbh, double ht,
                        Profile* profile) {
  if (equation.jsp == 22) {
    const double dform = dbh /
        (1.6802 * std::pow(ht - kBreastHeight, 0.4085 + 0.00169 * ht)) - 1.0;
    std::array<double, 10> u{{}};
    u[7] = clamp(-1.2726446 - 0.0048259438 * ht, -7.0, 7.0);
    u[9] = clamp(0.1821947, 0.0, 0.3);
    u[8] = std::min(0.99, 0.99);
    u[1] = clamp(-1.5505171 - 0.017174522 * ht, -7.0, 7.0);
    u[2] = clamp(0.27722769 - 0.21540189 * dbh, -7.0, 7.0);
    u[3] = clamp(2.0426515 - 0.83434213 * std::log(ht), -7.0, 7.0);
    u[4] = -7.0;
    u[5] = 7.1;
    u[6] = clamp(1.3766370 - 0.47598661 * dform, 1.005, 100.0);
    finish_standard_shape(u, profile, true);
    return;
  }
  shape_from_matrix(other_shape_values(equation.jsp), dbh, ht,
                    equation.jsp == 25, profile);
}

inline const double* alaska_shape_values(int jsp) {
  if (jsp == 31) return coefficient::f_alaska_38;
  if (jsp == 32) return coefficient::f_alaska_53;
  if (jsp == 33 || jsp == 34) return coefficient::f_alaska_68;
  return coefficient::f_alaska_83;
}

inline void shape_alaska(const Equation& equation, double dbh, double ht,
                         Profile* profile) {
  std::array<double, 38> values{{}};
  const double* source = alaska_shape_values(equation.jsp);
  std::copy(source, source + 38, values.begin());
  if ((equation.jsp == 33 || equation.jsp == 34) && geosub_is(equation, "01")) {
    values[25 - 10] = coefficient::f_alaska_98[0];
    values[34 - 10] = coefficient::f_alaska_98[1];
    values[42 - 10] = coefficient::f_alaska_98[2];
  }
  shape_from_matrix(values.data(), dbh, ht, false, profile);
}

inline void set_f(std::array<double, 50>* values, int first,
                  const double* source, int count) {
  for (int index = 0; index < count; ++index) {
    (*values)[static_cast<std::size_t>(first + index)] = source[index];
  }
}

inline void shape_west3(const Equation& equation, double dbh, double ht,
                        Profile* profile) {
  std::array<double, 50> f{{}};
  set_f(&f, 13, coefficient::f_west_38, 3);
  set_f(&f, 17, coefficient::f_west_39, 3);
  set_f(&f, 21, coefficient::f_west_40, 4);
  set_f(&f, 25, coefficient::f_west_42, 2);
  set_f(&f, 29, coefficient::f_west_43, 3);
  set_f(&f, 33, coefficient::f_west_44, 5);
  set_f(&f, 38, coefficient::f_west_46, 3);
  set_f(&f, 42, coefficient::f_west_47, 3);
  set_f(&f, 45, coefficient::f_west_48, 3);
  double f25 = f[25];
  double f34 = f[34];
  if (!geosub_is(equation, "00") && equation.geosub[0] == '0' &&
      equation.geosub[1] >= '1' && equation.geosub[1] <= '8') {
    const int region = equation.geosub[1] - '1';
    f25 = coefficient::f_west_57[region];
    f34 = coefficient::f_west_59[region];
  }
  const double median = coefficient::f_west_37[0] *
      std::pow(ht - kBreastHeight,
               coefficient::f_west_37[1] + coefficient::f_west_37[2] * ht);
  const double dform = dbh / median - 1.0;
  std::array<double, 10> u{{}};
  u[7] = f[13] + f[14] * std::log(dbh + 1.0) + f[15] * std::log(ht);
  const double u9a = clamp(f[18] + f[19] * ht, -7.0, 7.0);
  u[9] = f[17] * logistic(u9a);
  u[8] = clamp(f[21] + f[22] * std::log(ht) + f[23] * dform +
                   f[24] * std::pow(dbh / 10.0, 1.5), -7.0, 7.0);
  u[1] = f25 + f[26] * std::log(ht);
  u[2] = f[29] + f[30] * std::log(dbh) + f[31] * dbh;
  u[3] = f34 + f[35] * std::log(dbh + 1.0) + f[36] * std::log(ht) +
      f[37] * dform * ht + f[33] * dform;
  u[4] = f[38] + f[39] * dbh + f[40] * ht * dbh;
  u[5] = f[42] + f[43] * ht + f[44] * dform;
  for (int index = 1; index <= 5; ++index) {
    u[static_cast<std::size_t>(index)] = clamp(u[static_cast<std::size_t>(index)],
                                               -7.0, 7.0);
  }
  u[6] = 1.0 + std::exp(clamp(f[45] + f[46] * std::log(dbh + 1.0) +
                                  f[47] * std::log(ht), -6.0, 6.0));
  u[6] = clamp(u[6], 1.005, 100.0);
  u[7] = clamp(u[7], -7.0, 7.0);
  finish_standard_shape(u, profile, false);
}

inline void shape_west4(const Equation& equation, double dbh, double ht,
                        Profile* profile) {
  std::array<double, 50> f{{}};
  set_f(&f, 13, coefficient::f_west_180, 3);
  set_f(&f, 17, coefficient::f_west_181, 3);
  set_f(&f, 21, coefficient::f_west_182, 4);
  set_f(&f, 25, coefficient::f_west_184, 4);
  f[29] = coefficient::f_west_186[0];
  set_f(&f, 34, coefficient::f_west_187, 4);
  set_f(&f, 38, coefficient::f_west_189, 3);
  set_f(&f, 42, coefficient::f_west_190, 3);
  set_f(&f, 45, coefficient::f_west_191, 3);
  double f25 = f[25];
  double f34 = f[34];
  if (!geosub_is(equation, "00") && equation.geosub[0] == '0' &&
      equation.geosub[1] >= '1' && equation.geosub[1] <= '8') {
    const int region = equation.geosub[1] - '1';
    f25 = coefficient::f_west_201[region];
    f34 = coefficient::f_west_203[region];
  }
  const double log_ht = std::log(ht);
  const double median = 0.2855 * std::pow(
      ht - kBreastHeight,
      0.307 - 0.00505 * ht + 0.00001745 * ht * ht + 0.19 * log_ht);
  const double dform = dbh / median - 1.0;
  std::array<double, 10> u{{}};
  u[7] = clamp(f[13] + f[14] * (1.0 - std::exp(f[15] * ht)), -7.0, 7.0);
  double u9a = f[17] + f[18] * log_ht + f[19] * dform;
  if (logistic(u[7]) + u9a > 0.75) u9a = 0.75 - logistic(u[7]);
  u[9] = std::max(0.0, u9a);
  u[8] = clamp(f[21] + f[22] * log_ht + f[23] * dform +
                   f[24] * log_ht * dform, -7.0, 7.0);
  const double h100 = ht / 100.0;
  u[1] = f25 + f[26] / h100 + f[27] / (h100 * h100) +
      f[28] / (h100 * h100 * h100);
  u[2] = f[29];
  u[3] = f34 + f[35] / ht + f[36] * ht * dbh / 1000.0 + f[37] * dform;
  u[4] = f[38] + f[39] * log_ht + f[40] * dform;
  u[5] = f[42] + f[43] * log_ht + f[44] * dbh;
  for (int index = 1; index <= 5; ++index) {
    u[static_cast<std::size_t>(index)] = clamp(u[static_cast<std::size_t>(index)],
                                               -7.0, 7.0);
  }
  f[48] = kWest4UndefinedF48;
  u[6] = 1.0 + std::exp(clamp(f[45] + f[46] * std::log(dbh) + f[47] * ht +
                                  f[48] * log_ht, -6.0, 6.0));
  u[6] = clamp(u[6], 1.005, 100.0);
  finish_standard_shape(u, profile, false);
}

inline void shape_west5(double dbh, double ht, Profile* profile) {
  std::array<double, 50> f{{}};
  f[13] = coefficient::f_west_326[0];
  f[15] = coefficient::f_west_326[1];
  set_f(&f, 17, coefficient::f_west_327, 2);
  set_f(&f, 21, coefficient::f_west_328, 2);
  set_f(&f, 25, coefficient::f_west_329, 3);
  set_f(&f, 29, coefficient::f_west_331, 3);
  set_f(&f, 34, coefficient::f_west_332, 3);
  set_f(&f, 38, coefficient::f_west_333, 3);
  set_f(&f, 42, coefficient::f_west_334, 2);
  set_f(&f, 45, coefficient::f_west_335, 3);
  const double median = 0.11 * std::pow(ht - kBreastHeight, 1.08 + 0.0006 * ht);
  const double dform = dbh / median - 1.0;
  std::array<double, 10> u{{}};
  u[7] = clamp(f[13] + f[15] * dform, -7.0, 1.0);
  double u9a = f[17] + f[18] * std::log(ht);
  if (logistic(u[7]) + u9a > 0.75) u9a = 0.75 - logistic(u[7]);
  u[9] = std::max(0.0, u9a);
  u[8] = clamp(f[21] + f[22] / dbh, -7.0, 7.0);
  u[1] = f[25] + f[26] * std::log(dbh) + f[27] / dbh;
  u[2] = f[29] + f[30] / dbh + f[31] / (dbh * dbh);
  u[3] = f[34] + f[35] * dbh + f[36] * ht;
  u[4] = f[38] + f[39] * dbh + f[40] * dform;
  u[5] = f[42] + f[43] / dbh;
  for (int index = 1; index <= 5; ++index) {
    u[static_cast<std::size_t>(index)] = clamp(u[static_cast<std::size_t>(index)],
                                               -7.0, 7.0);
  }
  u[6] = 1.0 + std::exp(clamp(f[45] + f[46] * dbh + f[47] / dbh,
                                  -6.0, 6.0));
  u[6] = clamp(u[6], 1.005, 100.0);
  finish_standard_shape(u, profile, false, false);
}

inline void shape_profile(const Equation& equation, double dbh, double ht,
                          Profile* profile) {
  if (equation.jsp == 3) shape_west3(equation, dbh, ht, profile);
  else if (equation.jsp == 4) shape_west4(equation, dbh, ht, profile);
  else if (equation.jsp == 5) shape_west5(dbh, ht, profile);
  else if (equation.jsp >= 11 && equation.jsp <= 21) {
    shape_ingy(equation, dbh, ht, profile);
  } else if (equation.jsp >= 22 && equation.jsp <= 29) {
    shape_other(equation, dbh, ht, profile);
  } else {
    shape_alaska(equation, dbh, ht, profile);
  }
}

inline void calculate_taper(Profile* profile) {
  const double r1 = profile->form[0];
  const double r2 = profile->form[1];
  const double r3 = profile->form[2];
  const double r4 = profile->form[3];
  const double r5 = profile->form[4];
  const double a3 = profile->form[5];
  const double rhi1 = profile->heights[0];
  const double rhi2 = profile->heights[1];
  const double rhc = profile->heights[2];
  const double rhlongi = profile->heights[3];
  const double yc = 1.0 - rhc;
  const double c2 = r5 * yc;
  const double c1 = 3.0 * (yc - c2);
  const double slope = -(3.0 - r5) / 2.0;
  const double s1 = slope * (rhc - rhi2);
  const double yi_min = yc - s1 * (1.0 + 2.0 * r3) / 3.0;
  const double yi_max = yc - s1 * (5.0 + 4.0 * r3) / 9.0;
  const double yi2 = yi_min + r4 * (yi_max - yi_min);
  const double s0 = r3 * s1;
  const double b1 = (6.0 * yc - 6.0 * yi2 - 2.0 * s0 - 4.0 * s1) /
      (-3.0 * yc + 3.0 * yi2 + 2.0 * s0 + s1);
  const double b2 = s1 * (1.0 - r3) / (0.5 - 1.0 / (b1 + 1.0));
  const double b4 = s0;
  const double b0 = yi2;
  const double slope_rhi = r3 * s1 / (rhc - rhi2);
  const double yi1 = yi2 - slope_rhi * rhlongi;
  const double e2 = rhlongi > 0.0 ? (yi2 - yi1) / rhlongi : 0.0;
  const double e1 = rhlongi > 0.0 ? yi1 - e2 * rhi1 : yi2;
  const double s3 = -slope_rhi * rhi1;
  const double k2 = s3 / r1;
  const double f_a3 = 1.0 / (6.0 * a3 * a3) + std::log(1.0 - 1.0 / a3) +
      1.0 / (3.0 * (a3 - 1.0)) + 2.0 / (3.0 * a3);
  const double g_a3 = (1.0 / (a3 - 1.0) - 1.0 / a3 - 1.0 / (a3 * a3) -
      1.0 / std::pow(a3 - 1.0, 3.0)) / f_a3;
  const double yb_min = yi1 + (2.0 * s3 + k2) / 3.0 + (s3 - k2) * f_a3 /
      (1.0 / (a3 - 1.0) - 1.0 / a3 - 1.0 / (a3 * a3) -
       1.0 / (a3 * a3 * a3));
  const double yb_max = yi1 + (2.0 * s3 + k2) / 3.0 + (s3 - k2) / g_a3;
  const double yb = yb_min + r2 * (yb_max - yb_min);
  const double a0 = yi1;
  const double a2 = (yb - yi1 - (2.0 * s3 + k2) / 3.0) / f_a3;
  const double a1 = (k2 - s3 + a2 * (1.0 / (a3 - 1.0) - 1.0 / a3 -
                                      1.0 / (a3 * a3))) / 3.0;
  const double a4 = s3;
  profile->taper = {{nvel_real(a0), nvel_real(a1), nvel_real(a2),
      nvel_real(a4), nvel_real(b0), nvel_real(b1), nvel_real(b2),
      nvel_real(b4), nvel_real(c1), nvel_real(c2), nvel_real(e1),
      nvel_real(e2)}};
}

inline double base_diameter(const Profile& profile, double height) {
  if (height > profile.ht) return 0.0;
  if (height < 0.0) return profile.scale;
  const double rh = nvel_real(height / profile.ht);
  const double rhi1 = profile.heights[0];
  const double rhi2 = profile.heights[1];
  const double rhc = profile.heights[2];
  const double a3 = profile.form[5];
  double value = 0.0;
  if (rh >= rhc) {
    const double x = (1.0 - rh) / (1.0 - rhc);
    if (profile.equation.jsp == 22) {
      value = profile.taper[9] * x +
          (profile.taper[8] / 2.0) * x * x -
          (profile.taper[8] / 6.0) * x * x * x;
    } else {
      value = x * (profile.taper[9] + x *
          (profile.taper[8] / 2.0 - (profile.taper[8] / 6.0) * x));
    }
  } else if (rh >= rhi2) {
    const double x = (rh - rhi2) / (rhc - rhi2);
    if (x > 0.0) {
      if (profile.equation.jsp == 22) {
        const double powered = profile.taper[5] * std::log10(x) <= -20.0
            ? 0.0 : std::pow(x, profile.taper[5] + 2.0);
        value = profile.taper[4] + profile.taper[7] * x -
            profile.taper[6] /
                ((profile.taper[5] + 1.0) * (profile.taper[5] + 2.0)) *
                powered +
            (profile.taper[6] / 6.0) * x * x * x;
      } else {
        const double powered = profile.taper[5] * std::log10(x) <= -20.0
            ? 0.0 : std::pow(x, profile.taper[5]);
        value = profile.taper[4] + x * (profile.taper[7] + x *
            (-profile.taper[6] /
                 ((profile.taper[5] + 1.0) * (profile.taper[5] + 2.0)) *
                 powered +
             (profile.taper[6] / 6.0) * x));
      }
    } else {
      value = profile.taper[4];
    }
  } else if (profile.heights[3] > 0.0 && rh > rhi1) {
    value = profile.taper[10] + profile.taper[11] * rh;
  } else {
    const double x = (rhi1 - rh) / rhi1;
    if (profile.equation.jsp == 22) {
      value = profile.taper[0] +
          (profile.taper[3] + profile.taper[2] / a3) * x +
          profile.taper[2] / (2.0 * a3 * a3) * x * x +
          profile.taper[1] * x * x * x +
          profile.taper[2] * std::log(1.0 - x / a3);
    } else {
      value = profile.taper[0] + x *
          (profile.taper[3] + profile.taper[2] / a3 +
           x * (profile.taper[2] / (2.0 * a3 * a3) + profile.taper[1] * x)) +
          profile.taper[2] * std::log(1.0 - x / a3);
    }
  }
  return nvel_real(profile.scale * value);
}

inline double base_diameter_slope(const Profile& profile, double height) {
  if (height > profile.ht || height < 0.0) return 0.0;
  const double rh = nvel_real(height / profile.ht);
  const double rhi1 = profile.heights[0];
  const double rhi2 = profile.heights[1];
  const double rhc = profile.heights[2];
  const double a3 = profile.form[5];
  double derivative = 0.0;
  double relative_length = 1.0;
  double sign = 1.0;
  if (rh >= rhc) {
    const double x = (1.0 - rh) / (1.0 - rhc);
    derivative = profile.taper[9] + x *
        (profile.taper[8] - (profile.taper[8] / 2.0) * x);
    relative_length = 1.0 - rhc;
    sign = -1.0;
  } else if (rh >= rhi2) {
    const double x = (rh - rhi2) / (rhc - rhi2);
    double powered = 0.0;
    if (x > 0.0 && profile.taper[5] * std::log10(x) > -20.0) {
      powered = std::pow(x, profile.taper[5] + 1.0);
    }
    derivative = x > 0.0
        ? profile.taper[7] - profile.taper[6] /
              (profile.taper[5] + 1.0) * powered +
              (profile.taper[6] / 2.0) * x * x
        : profile.taper[7];
    relative_length = rhc - rhi2;
  } else if (profile.heights[3] > 0.0 && rh > rhi1) {
    derivative = profile.taper[11];
  } else {
    const double x = (rhi1 - rh) / rhi1;
    derivative = profile.taper[3] + profile.taper[2] / a3 +
        profile.taper[2] * x / (a3 * a3) +
        3.0 * profile.taper[1] * x * x -
        profile.taper[2] / (a3 - x);
    relative_length = rhi1;
    sign = -1.0;
  }
  return nvel_real(sign * derivative * profile.scale /
                   (relative_length * profile.ht));
}

inline double region_three_bark_ratio(const Equation& equation) {
  if (equation.geocode != '3') return 0.0;
  if (equation.geosub[0] != '0') return 0.0;
  if (equation.geosub[1] == '0' && equation.species == 202) return 0.8885;
  if (equation.geosub[1] != '1') return 0.0;
  if (equation.species == 122) return 0.8912;
  if (equation.species == 108) return 0.9326;
  if (equation.species == 202) return 0.8972;
  if (equation.species == 15) return 0.9116;
  return 0.0;
}

inline double bark_thickness_west(const Equation& equation, double dbh,
                                  double ht) {
  if (equation.jsp == 3) {
    const double median = 0.566 *
        std::pow(ht - kBreastHeight, 0.634 + 0.00074 * ht);
    const double dform = dbh / median - 1.0;
    double ratio = std::exp(-2.4641 + 0.04393 * std::log(dbh) -
                            0.2922 * dform + 0.05964 * dform * std::log(dbh));
    if (!geosub_is(equation, "00") && equation.geosub[0] == '0' &&
        equation.geosub[1] >= '1' && equation.geosub[1] <= '8') {
      const int index = equation.geosub[1] - '1';
      ratio = std::exp(-2.5087 + 0.036 * std::log(dbh) - 0.4086 * dform +
                       0.1012 * dform * std::log(dbh) +
                       coefficient::f_west_917[index]);
    }
    return ratio * dbh;
  }
  if (equation.jsp == 4) {
    double ratio = 0.04504 * (1.0 + 0.8307 * std::exp(-0.2048 * dbh));
    if (!geosub_is(equation, "00") && equation.geosub[0] == '0' &&
        equation.geosub[1] >= '1' && equation.geosub[1] <= '8') {
      const int index = equation.geosub[1] - '1';
      ratio = 0.04221 * (1.0 + 0.8836 * std::exp(-0.2145 * dbh)) *
          (1.0 + coefficient::f_west_919[index]);
    }
    return ratio * dbh;
  }
  const double diameter = std::max(dbh, 3.8);
  return 0.01949 * (1.0 + 15.599 / diameter -
                    29.212 / (diameter * diameter)) * dbh;
}

inline const double* ingy_bh_values(int jsp) {
  switch (jsp) {
    case 11: return coefficient::f_ingy_874;
    case 12: return coefficient::f_ingy_881;
    case 13: return coefficient::f_ingy_887;
    case 14: return coefficient::f_ingy_893;
    case 15: return coefficient::f_ingy_899;
    case 16: return coefficient::f_ingy_906;
    case 17: return coefficient::f_ingy_913;
    case 18: return coefficient::f_ingy_920;
    case 19: return coefficient::f_ingy_927;
    case 20: return coefficient::f_ingy_934;
    default: return coefficient::f_ingy_941;
  }
}

inline const double* ingy_region_bh_values(int jsp) {
  switch (jsp) {
    case 11: return coefficient::f_ingy_876;
    case 12: return coefficient::f_ingy_883;
    case 13: return coefficient::f_ingy_889;
    case 14: return coefficient::f_ingy_895;
    case 15: return coefficient::f_ingy_901;
    case 16: return coefficient::f_ingy_908;
    case 17: return coefficient::f_ingy_915;
    case 18: return coefficient::f_ingy_922;
    case 19: return coefficient::f_ingy_929;
    case 20: return coefficient::f_ingy_936;
    default: return coefficient::f_ingy_943;
  }
}

inline double bark_thickness_ingy(const Equation& equation, double dbh,
                                  double ht) {
  const int species_index = equation.jsp - 10;
  if (species_index == 1 && geosub_is(equation, "15")) {
    return dbh - (-0.06933188 + 0.8981755 * dbh);
  }
  if (species_index == 3 && geosub_is(equation, "15")) {
    return dbh - (0.35929539 + 0.9213101 * dbh - 0.2543261 * std::log(dbh));
  }
  if (species_index == 9 && geosub_is(equation, "15")) {
    return dbh - (-0.2110537 + 0.9682267 * dbh - 0.0053090 * std::log(dbh));
  }
  const double* a = ingy_bh_values(equation.jsp);
  double a0 = a[0];
  if (!geosub_is(equation, "00")) {
    const int region = (equation.geosub[0] - '0') * 10 + equation.geosub[1] - '0';
    if (region >= 11 && region <= 23) {
      const double regional = ingy_region_bh_values(equation.jsp)[region - 11];
      if (regional != 0.0) a0 = regional;
    }
  }
  double diameter = dbh;
  if (a[2] < 0.0 && a[5] == 0.0) {
    const double maximum = -(a[1] + a[4] * ht) / (2.0 * a[2]);
    diameter = std::min(dbh, maximum);
  } else if (a[2] < 0.0 && a[5] == -1.0) {
    const double minimum = -(a[1] + a[4] * ht) / (2.0 * a[2]);
    diameter = std::max(dbh, minimum);
  }
  const double linear = clamp(a0 + a[1] * diameter + a[2] * diameter * diameter +
                                  a[3] * ht + a[4] * ht * diameter,
                              -8.0, 8.0);
  return logistic(linear) * dbh;
}

inline double bark_thickness_alaska(int jsp, double dbh, double ht) {
  double inside = 0.0;
  if (jsp == 31) inside = -0.307528 + 0.982927 * dbh;
  else if (jsp == 32) inside = -0.451778 + 0.975611 * dbh;
  else if (jsp == 33) inside = -0.0171289 + 0.9511897 * dbh + 0.000507 * dbh * dbh;
  else if (jsp == 34) inside = -0.054306 + 0.916145 * dbh + 0.004821 * ht;
  else if (jsp == 35) inside = -0.212445 + 0.983230 * dbh;
  else inside = -0.088908 + 0.963193 * dbh;
  return dbh - inside;
}

inline const double* other_bark_values(int jsp) {
  switch (jsp) {
    case 22: return coefficient::f_other_29;
    case 23: return coefficient::f_other_33;
    case 24: return coefficient::f_other_37;
    case 25: return coefficient::f_other_41;
    case 26: return coefficient::f_other_45;
    case 27: return coefficient::f_other_49;
    case 28: return coefficient::f_other_54;
    default: return coefficient::f_other_58;
  }
}

inline double other_inside_at_bh(const Equation& equation, double dbh) {
  const double* bark = other_bark_values(equation.jsp);
  const int index = equation.jsp - 21;
  if (index <= 1) return dbh * (bark[6] + bark[7] * std::log(dbh));
  if (index == 2) {
    if (geosub_is(equation, "07")) {
      return 0.4065547 + 0.7794452 * dbh + 0.0035815 * dbh * dbh;
    }
    if (geosub_is(equation, "13")) return dbh * (bark[6] + bark[7] * std::log(dbh));
    if (geosub_is(equation, "01")) return -0.649171195 + 0.925582344 * dbh;
    return -1.024742 + 0.933772 * dbh;
  }
  if (index == 3) return dbh * (bark[6] + bark[7] * std::log(dbh));
  if (index == 4) {
    if (geosub_is(equation, "02")) {
      return dbh * (0.925915 + 0.0153361 * std::log(dbh));
    }
    return dbh * (bark[6] + bark[7] * std::log(dbh));
  }
  if (index >= 5 && index <= 7) return bark[6] + bark[7] * dbh;
  return bark[6] + bark[7] * dbh + bark[8] * dbh * dbh;
}

inline double bark_ratio_west(const Profile& profile, double height) {
  const double dbh = profile.dbh;
  const double ht = profile.ht;
  if (profile.equation.jsp == 3) {
    const double exponent = 0.51705 + 0.483 * std::exp(-0.251 * dbh);
    const double t1 = 0.6406 + 0.0512 * (1.0 - std::exp(-0.0201 * dbh));
    const double t_bh = std::pow(kBreastHeight / ht, exponent);
    const double r1 = std::min(0.99, 0.6294 + 0.7901 * std::exp(-0.3 * dbh));
    const double e1 = (r1 - 1.0) /
        (-2.0 * t1 * (t1 - t_bh) + t1 * t1 - t_bh * t_bh);
    const double e0 = -2.0 * t1 * e1;
    const double t = height <= 0.0 ? 0.0 : std::pow(height / ht, exponent);
    const double h75 = std::max(0.75 * ht, kBreastHeight +
                                             0.5 * (ht - kBreastHeight));
    double relative = 1.0 + e0 * (t - t_bh) + e1 * (t * t - t_bh * t_bh);
    if (height > h75) {
      relative += 0.5983 * std::pow((height - h75) / (ht - h75), 2.0);
    }
    return profile.dbt_bh / dbh * relative;
  }
  const double x_bh = kBreastHeight / ht;
  if (profile.equation.jsp == 4) {
    const double x = height / ht;
    const double factor = 1.0 + (coefficient::f_west_990[0] +
        coefficient::f_west_990[1] *
            std::exp(coefficient::f_west_990[2] * dbh)) * (x - x_bh) +
        coefficient::f_west_990[3] * (x * x - x_bh * x_bh);
    return profile.dbt_bh / dbh * factor;
  }
  const double exponent = coefficient::f_west_987[4];
  const double x = height > 0.0 ? std::pow(height / ht, exponent) : 0.0;
  const double transformed_bh = std::pow(x_bh, exponent);
  const double factor = 1.0 + (coefficient::f_west_987[0] +
      coefficient::f_west_987[1] *
          std::exp(coefficient::f_west_987[2] * dbh)) *
          (x - transformed_bh) + coefficient::f_west_987[3] *
          (x * x - transformed_bh * transformed_bh);
  return profile.dbt_bh / dbh * factor;
}

inline const double* ingy_diameter_bark_values(int jsp) {
  switch (jsp) {
    case 11: return coefficient::f_ingy_1218;
    case 12: return coefficient::f_ingy_1221;
    case 13: return coefficient::f_ingy_1224;
    case 14: return coefficient::f_ingy_1227;
    case 15: return coefficient::f_ingy_1230;
    case 16: return coefficient::f_ingy_1233;
    case 17: return coefficient::f_ingy_1236;
    case 18: return coefficient::f_ingy_1239;
    case 19: return coefficient::f_ingy_1242;
    case 20: return coefficient::f_ingy_1245;
    default: return coefficient::f_ingy_1248;
  }
}

inline const double* ingy_height_bark_values(int jsp) {
  switch (jsp) {
    case 11: return coefficient::f_ingy_1340;
    case 12: return coefficient::f_ingy_1343;
    case 13: return coefficient::f_ingy_1346;
    case 14: return coefficient::f_ingy_1349;
    case 15: return coefficient::f_ingy_1352;
    case 16: return coefficient::f_ingy_1355;
    case 17: return coefficient::f_ingy_1358;
    case 18: return coefficient::f_ingy_1361;
    case 19: return coefficient::f_ingy_1364;
    case 20: return coefficient::f_ingy_1367;
    default: return coefficient::f_ingy_1370;
  }
}

inline double bark_ratio_ingy_diameter(const Profile& profile, double height,
                                       double outside) {
  const double* bark = ingy_diameter_bark_values(profile.equation.jsp);
  const double inside_bh = profile.dbh - profile.dbt_bh;
  if (height < kBreastHeight) {
    double exponent = bark[0] + bark[1] * profile.dbh + bark[2] * profile.dbt_bh;
    exponent = std::max(-0.9, exponent);
    double inside = outside * (inside_bh / profile.dbh) *
        std::pow(outside / profile.dbh, exponent);
    inside = std::max(0.5 * outside, inside);
    inside = std::min(std::max(0.99, inside_bh / profile.dbh) * outside, inside);
    return (outside - inside) / outside;
  }
  if (bark[3] == 0.0) {
    const double dr = std::max(0.07, std::min(outside / profile.dbh, 1.0));
    double thickness = profile.dbt_bh *
        (dr * (bark[4] - 1.0) / (bark[4] - std::pow(dr, bark[5])) -
         (std::pow(dr, bark[6]) - 1.0) / profile.dbt_bh);
    thickness = std::min(thickness, 0.5 * outside);
    thickness = std::max(thickness,
                         std::min(0.01, profile.dbt_bh / profile.dbh) * outside);
    return thickness / outside;
  }
  const double ratio_bh = profile.dbt_bh / profile.dbh;
  double exponent = bark[4] + bark[5] * profile.dbh + bark[6] * ratio_bh +
      bark[7] * ratio_bh * ratio_bh;
  exponent = std::max(-0.9, exponent);
  double inside = outside * (inside_bh / profile.dbh) *
      std::pow(outside / profile.dbh, exponent);
  inside = std::max(0.5 * outside, inside);
  inside = std::min(std::max(0.99, inside_bh / profile.dbh) * outside, inside);
  return (outside - inside) / outside;
}

inline double bark_ratio_ingy_height(const Profile& profile, double height) {
  const double* bark = ingy_height_bark_values(profile.equation.jsp);
  const double x = height > 0.0
      ? std::pow(height / profile.ht, bark[4]) : 0.0;
  const double x_bh = std::pow(kBreastHeight / profile.ht, bark[4]);
  double ratio = profile.dbt_bh / profile.dbh *
      (1.0 + (bark[0] + bark[1] * std::exp(bark[2] * profile.dbh)) *
           (x - x_bh) + bark[3] * (x * x - x_bh * x_bh));
  ratio = std::min(0.5, ratio);
  return std::max(std::min(0.01, profile.dbt_bh / profile.dbh), ratio);
}

inline double combine_ingy_bark(const Profile& profile, double height,
                                double diameter_ratio, double height_ratio) {
  if (height <= kBreastHeight) return diameter_ratio;
  const double weight = coefficient::f_ingy_1169[profile.equation.jsp - 11];
  const double area_diameter = 1.0 - std::pow(1.0 - diameter_ratio, 2.0);
  const double area_height = 1.0 - std::pow(1.0 - height_ratio, 2.0);
  const double area = weight * area_diameter + (1.0 - weight) * area_height;
  return 1.0 - std::sqrt(1.0 - area);
}

inline double bark_ratio_ingy_inside(const Profile& profile, double height,
                                     double inside) {
  const double initial = bark_ratio_ingy_height(profile, height);
  auto error = [&profile, height, inside](double ratio) {
    const double outside = inside / (1.0 - ratio);
    const double estimated = bark_ratio_ingy_diameter(profile, height, outside);
    return (1.0 - estimated) * outside - inside;
  };
  double lower = initial;
  double upper = initial;
  double lower_error = error(lower);
  double upper_error = lower_error;
  const double tolerance = 1e-12 * std::max(1.0, inside);
  if (std::abs(lower_error) <= tolerance) {
    return combine_ingy_bark(profile, height, initial, initial);
  }
  if (lower_error > 0.0) {
    do {
      upper = lower;
      upper_error = lower_error;
      lower *= 0.7;
      lower_error = error(lower);
    } while (lower_error > 0.0 && lower > 1e-12);
  } else {
    int passes = 0;
    do {
      lower = upper;
      lower_error = upper_error;
      upper += 0.05;
      upper_error = error(upper);
      ++passes;
    } while (upper_error < 0.0 && !(passes > 4 && upper > 0.5));
    if (upper_error < 0.0) return initial;
  }
  double answer = initial;
  for (int iteration = 0; iteration < 200; ++iteration) {
    const double midpoint = (lower + upper) / 2.0;
    const double midpoint_error = error(midpoint);
    answer = midpoint;
    if (std::abs(midpoint_error) <= tolerance) break;
    if (lower_error * midpoint_error <= 0.0) {
      upper = midpoint;
      upper_error = midpoint_error;
    } else {
      lower = midpoint;
      lower_error = midpoint_error;
    }
  }
  (void) upper_error;
  return combine_ingy_bark(profile, height, answer, initial);
}

inline double bark_other_inside(const Profile& profile, double height,
                                double outside) {
  const double* bark = other_bark_values(profile.equation.jsp);
  const double dr = outside > 0.0 ? outside / profile.dbh : 0.0;
  double relative = 0.0;
  if (height > kBreastHeight) {
    if (dr > 0.01) {
      relative = dr * (bark[0] - 1.0) /
          (bark[0] - std::pow(dr, bark[1])) -
          (std::pow(dr, bark[2]) - 1.0) / profile.dbt_bh;
    }
  } else if (height == kBreastHeight) {
    relative = 1.0;
  } else {
    const double value = bark[3] * (dr - 1.0);
    relative = value >= 0.0
        ? 1.0 + std::pow(value, bark[4] + bark[5] * profile.dbt_bh) : 1.0;
  }
  return std::max(0.0, outside - relative * profile.dbt_bh);
}

inline double bark_other_outside(const Profile& profile, double height,
                                 double inside) {
  double lower = inside;
  double upper = std::max(profile.dbh, inside * 2.0);
  for (int expansion = 0;
       expansion < 100 && bark_other_inside(profile, height, upper) < inside;
       ++expansion) {
    upper *= 2.0;
  }
  if (bark_other_inside(profile, height, upper) < inside) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  for (int iteration = 0; iteration < 100; ++iteration) {
    const double midpoint = (lower + upper) / 2.0;
    if (bark_other_inside(profile, height, midpoint) < inside) {
      lower = midpoint;
    } else {
      upper = midpoint;
    }
  }
  return (lower + upper) / 2.0;
}

inline const double* ingy_correlation_values(int jsp) {
  switch (jsp) {
    case 11: return coefficient::f_ingy_467;
    case 12: return coefficient::f_ingy_471;
    case 13: return coefficient::f_ingy_475;
    case 14: return coefficient::f_ingy_479;
    case 15: return coefficient::f_ingy_483;
    case 16: return coefficient::f_ingy_487;
    case 17: return coefficient::f_ingy_491;
    case 18: return coefficient::f_ingy_495;
    case 19: return coefficient::f_ingy_499;
    case 20: return coefficient::f_ingy_503;
    default: return coefficient::f_ingy_507;
  }
}

inline const double* other_correlation_values(int jsp) {
  switch (jsp) {
    case 23: return coefficient::f_other_412;
    case 24: return coefficient::f_other_417;
    case 25: return coefficient::f_other_422;
    case 26: return coefficient::f_other_427;
    case 27: return coefficient::f_other_432;
    case 28: return coefficient::f_other_437;
    default: return coefficient::f_other_442;
  }
}

inline const double* west_correlation_values(int jsp) {
  if (jsp == 3) return coefficient::f_west_436;
  if (jsp == 4) return coefficient::f_west_438;
  return coefficient::f_west_440;
}

inline const double* alaska_correlation_values(int jsp) {
  int group = jsp - 30;
  if (group == 4) group = 3;
  if (group == 5 || group == 6) group = 4;
  if (group == 1) return coefficient::f_alaska_216;
  if (group == 2) return coefficient::f_alaska_220;
  if (group == 3) return coefficient::f_alaska_224;
  return coefficient::f_alaska_228;
}

inline double correlation_from_values(const Profile& profile, double first,
                                      double second, const double* values,
                                      bool third_is_q3,
                                      bool black_hills_limits = false) {
  if (first == second) return 1.0;
  if (first == kBreastHeight || second == kBreastHeight) return 0.5;
  const double q1 = values[0];
  const double q2 = values[1];
  const double q3 = third_is_q3 ? values[2] : values[2] - q1 - q2;
  const double q4 = values[3];
  const double q5 = values[4];
  const double h1 = std::min(first, second);
  const double h2 = std::max(first, second);
  double correlation = 0.0;
  if (h1 > kBreastHeight) {
    const double t3 = (h1 - kBreastHeight) /
        (profile.ht - kBreastHeight);
    const double t4 = (h2 - kBreastHeight) /
        (profile.ht - kBreastHeight);
    correlation = std::exp(q1 * (t4 - t3) +
        q2 * (t4 * t4 - t3 * t3) / 2.0 +
        q3 * (t4 * t4 * t4 - t3 * t3 * t3) / 3.0);
  } else if (h2 > kBreastHeight) {
    const double t3 = (h2 - kBreastHeight) /
        (profile.ht - kBreastHeight);
    const double t2 = (kBreastHeight - h1) / kBreastHeight;
    correlation = q5 * std::exp(q4 * t2 + q1 * t3 +
        q2 * t3 * t3 / 2.0 + q3 * t3 * t3 * t3 / 3.0);
  } else {
    const double t2 = (kBreastHeight - h2) / kBreastHeight;
    const double t1 = (kBreastHeight - h1) / kBreastHeight;
    correlation = std::exp(q4 * (t1 - t2));
  }
  return black_hills_limits && h2 > kBreastHeight
      ? clamp(correlation, -0.999, 0.999) : correlation;
}

inline double conditioning_correlation(const Profile& profile, double first,
                                       double second) {
  const int jsp = profile.equation.jsp;
  if (jsp <= 5) {
    return correlation_from_values(
        profile, first, second, west_correlation_values(jsp), true);
  }
  if (jsp <= 21) {
    return correlation_from_values(
        profile, first, second, ingy_correlation_values(jsp), false);
  }
  if (jsp == 22) {
    static constexpr double values[] = {
        -4.2141136, 3.6157646, 0.0, -1.5164459, 0.28261064};
    return correlation_from_values(
        profile, first, second, values, false, true);
  }
  if (jsp <= 29) {
    return correlation_from_values(
        profile, first, second, other_correlation_values(jsp), false);
  }
  return correlation_from_values(
      profile, first, second, alaska_correlation_values(jsp), false);
}

inline void ingy_variance_values(int jsp, const double** median,
                                 const double** variance) {
  switch (jsp) {
    case 11: *median = coefficient::f_ingy_614;
      *variance = coefficient::f_ingy_617; break;
    case 12: *median = coefficient::f_ingy_625;
      *variance = coefficient::f_ingy_628; break;
    case 13: *median = coefficient::f_ingy_636;
      *variance = coefficient::f_ingy_639; break;
    case 14: *median = coefficient::f_ingy_647;
      *variance = coefficient::f_ingy_650; break;
    case 15: *median = coefficient::f_ingy_658;
      *variance = coefficient::f_ingy_661; break;
    case 16: *median = coefficient::f_ingy_669;
      *variance = coefficient::f_ingy_672; break;
    case 17: *median = coefficient::f_ingy_680;
      *variance = coefficient::f_ingy_683; break;
    case 18: *median = coefficient::f_ingy_691;
      *variance = coefficient::f_ingy_694; break;
    case 19: *median = coefficient::f_ingy_702;
      *variance = coefficient::f_ingy_705; break;
    case 20: *median = coefficient::f_ingy_713;
      *variance = coefficient::f_ingy_716; break;
    default: *median = coefficient::f_ingy_724;
      *variance = coefficient::f_ingy_727; break;
  }
}

inline void other_variance_values(int jsp, const double** median,
                                  const double** variance) {
  switch (jsp) {
    case 23: *median = coefficient::f_other_535;
      *variance = coefficient::f_other_538; break;
    case 24: *median = coefficient::f_other_547;
      *variance = coefficient::f_other_550; break;
    case 25: *median = coefficient::f_other_557;
      *variance = coefficient::f_other_560; break;
    case 26: *median = coefficient::f_other_568;
      *variance = coefficient::f_other_571; break;
    case 27: *median = coefficient::f_other_579;
      *variance = coefficient::f_other_582; break;
    case 28: *median = coefficient::f_other_590;
      *variance = coefficient::f_other_593; break;
    default: *median = coefficient::f_other_601;
      *variance = coefficient::f_other_604; break;
  }
}

inline void alaska_variance_values(int jsp, const double** median,
                                   const double** variance) {
  int group = jsp - 30;
  if (group == 4) group = 3;
  if (group == 5 || group == 6) group = 4;
  if (group == 1) {
    *median = coefficient::f_alaska_323;
    *variance = coefficient::f_alaska_326;
  } else if (group == 2) {
    *median = coefficient::f_alaska_334;
    *variance = coefficient::f_alaska_337;
  } else if (group == 3) {
    *median = coefficient::f_alaska_345;
    *variance = coefficient::f_alaska_348;
  } else {
    *median = coefficient::f_alaska_356;
    *variance = coefficient::f_alaska_359;
  }
}

inline double lognormal_standard_error(const Profile& profile, double height) {
  const double* median = nullptr;
  const double* variance = nullptr;
  const int jsp = profile.equation.jsp;
  const bool adjust_boundaries = (jsp >= 11 && jsp <= 21) || jsp >= 31;
  if (jsp >= 11 && jsp <= 21) {
    ingy_variance_values(jsp, &median, &variance);
  } else if (jsp >= 23 && jsp <= 29) {
    other_variance_values(jsp, &median, &variance);
  } else if (jsp >= 31 && jsp <= 36) {
    alaska_variance_values(jsp, &median, &variance);
  } else {
    return std::numeric_limits<double>::quiet_NaN();
  }
  const double dmedian = median[0] * std::pow(
      profile.ht - kBreastHeight, median[1] + median[2] * profile.ht);
  const double diameter_ratio = profile.dbh / dmedian;
  const double log_height = std::log(profile.ht);
  const double va0 = variance[0] + variance[1] * log_height +
      variance[2] * diameter_ratio + variance[9] * log_height * diameter_ratio;
  const double vb0 = variance[3] + variance[4] * log_height;
  const double ve0 = variance[10] + variance[11] * log_height +
      variance[12] * diameter_ratio;
  const double vf0 = variance[13] + variance[14] * log_height;
  const double vc = variance[5] + variance[6] * log_height;
  if (height < kBreastHeight) {
    double xu = (kBreastHeight - height) / kBreastHeight;
    const double original = xu;
    const bool modified = adjust_boundaries && xu < 0.111;
    if (modified) xu = 0.111;
    const double log_variance = clamp(ve0 + vf0 * std::pow(xu, variance[15]),
                                      -15.0, 15.0);
    const double standard_error = std::sqrt(std::exp(log_variance));
    return modified ? standard_error * original / xu : standard_error;
  }
  if (height == kBreastHeight) return 0.0;
  if (height >= profile.ht) return 1.0;
  double xu = (height - kBreastHeight) / (profile.ht - kBreastHeight);
  double fraction = 1.0;
  bool modified = false;
  if (adjust_boundaries && xu < 0.02) {
    fraction = xu / 0.02;
    xu = 0.02;
    modified = true;
  } else if (adjust_boundaries && xu > 0.96) {
    xu = 0.96;
    modified = true;
  }
  const double log_variance = clamp(
      va0 + vb0 * std::pow(xu, vc) +
          variance[7] * std::pow(profile.ht / 50.0, variance[8]) / (1.0 - xu),
      -15.0, 15.0);
  const double standard_error = std::sqrt(std::exp(log_variance));
  return modified ? fraction * standard_error : standard_error;
}

inline double black_hills_standard_error(const Profile& profile,
                                         double height) {
  if (height == kBreastHeight) return 0.0;
  double log_variance = 0.0;
  if (height < kBreastHeight) {
    log_variance = -1.5512542 - 0.82366251 * height +
        0.10190777 * profile.dbh;
  } else {
    const double relative_height = (height - kBreastHeight) /
        (profile.ht - kBreastHeight);
    const double log_dbh = std::log(profile.dbh);
    log_variance = -7.23357 + 2.2333875 * log_dbh +
        (2.5870429 - 0.043950365 * log_dbh) *
        std::pow(relative_height, -0.57411597 + 0.67914946 * log_dbh);
  }
  return std::sqrt(std::exp(log_variance));
}

struct WestDistribution {
  double mu = 0.0;
  double lambda = 0.0;
  double gamma = 0.0;
  double delta = 0.0;
};

inline WestDistribution west_distribution(const Profile& profile,
                                           double height) {
  const int jsp = profile.equation.jsp;
  const double t = height > kBreastHeight
      ? (height - kBreastHeight) / (profile.ht - kBreastHeight)
      : (kBreastHeight - height) / kBreastHeight;
  double mu = 0.0;
  double lambda = 0.0;
  double tr2 = 0.0;
  if (jsp == 3) {
    mu = coefficient::f_west_558[0];
    if (height > kBreastHeight) {
      const double log_lambda = clamp(coefficient::f_west_564[0] +
          coefficient::f_west_564[1] * t * t, -12.0, 12.0);
      lambda = 1.0 - mu + std::exp(log_lambda);
      tr2 = coefficient::f_west_559[0] +
          coefficient::f_west_559[1] * profile.dbh +
          coefficient::f_west_559[2] * std::log(profile.ht) +
          (coefficient::f_west_560[0] + coefficient::f_west_560[1] *
           std::log(profile.ht)) * t + coefficient::f_west_561[0] *
          std::pow(t, 4.0) + coefficient::f_west_561[1] * std::log(t);
    } else {
      const double log_lambda = clamp(coefficient::f_west_557[0] +
          coefficient::f_west_557[1] * t * t, 0.0, 12.0);
      lambda = std::exp(log_lambda);
      tr2 = coefficient::f_west_562[0] + coefficient::f_west_562[1] *
          profile.ht + (coefficient::f_west_563[0] +
          coefficient::f_west_563[2] * std::pow(profile.ht, 0.3)) *
          std::pow(t, coefficient::f_west_563[1]);
      tr2 = clamp(tr2, -25.0, 12.0);
    }
  } else {
    const double* a1_values = jsp == 4 ? coefficient::f_west_683
                                       : coefficient::f_west_802;
    const double* a2_values = jsp == 4 ? coefficient::f_west_684
                                       : coefficient::f_west_803;
    const double* a3_values = jsp == 4 ? coefficient::f_west_685
                                       : coefficient::f_west_804;
    const double* a4_values = jsp == 4 ? coefficient::f_west_686
                                       : coefficient::f_west_805;
    const double* s13_values = jsp == 4 ? coefficient::f_west_687
                                        : coefficient::f_west_806;
    const double* s56_values = jsp == 4 ? coefficient::f_west_688
                                        : coefficient::f_west_807;
    const double s7 = jsp == 4 ? coefficient::f_west_689[0]
                               : coefficient::f_west_808[0];
    const double* u_values = jsp == 4 ? coefficient::f_west_690
                                      : coefficient::f_west_809;
    if (height > kBreastHeight) {
      mu = s13_values[0];
      lambda = 1.0 - mu + std::exp(s56_values[0] + s56_values[1] * t * t);
      const double log_diameter = std::log(profile.dbh + 0.5);
      const double a1 = a1_values[0] + a1_values[1] *
          std::log(profile.dbh + 1.0) + a1_values[2] / profile.dbh;
      const double a2 = std::min(a2_values[0] + a2_values[1] * profile.dbh, 0.0);
      const double a3 = a3_values[0] + a3_values[1] * log_diameter +
          a3_values[2] * log_diameter * log_diameter;
      const double a4 = a4_values[0] + a4_values[1] *
          std::exp(a4_values[2] * profile.dbh);
      tr2 = a1 + a2 * std::pow(t, a3) + a4 / t;
    } else {
      mu = s13_values[1];
      lambda = s7;
      tr2 = u_values[0] * (1.0 - std::exp(u_values[1] * profile.dbh)) +
          u_values[2] * t;
    }
    tr2 = clamp(tr2, -12.0, 12.0);
  }
  const double derivative = std::exp(tr2) * lambda;
  const double median_y = (1.0 - mu) / lambda;
  const double delta = derivative * lambda / 4.0 *
      (1.0 - std::pow(2.0 * median_y - 1.0, 2.0));
  const double gamma = -delta * std::log(median_y / (1.0 - median_y));
  return {mu, lambda, gamma, delta};
}

inline double west_standardized_error(const Profile& profile, double height,
                                      double predicted, double measured) {
  const WestDistribution distribution = west_distribution(profile, height);
  const double x = std::max(measured / predicted, distribution.mu + 0.0005);
  const double y = std::min((x - distribution.mu) / distribution.lambda,
                            0.999999);
  return distribution.gamma + distribution.delta * std::log(y / (1.0 - y));
}

inline double west_diameter_from_error(const Profile& profile, double height,
                                       double predicted, double error) {
  if (std::abs(height - kBreastHeight) < 1e-6) return predicted;
  const WestDistribution distribution = west_distribution(profile, height);
  const double y = logistic((error - distribution.gamma) / distribution.delta);
  return predicted * (distribution.mu + distribution.lambda * y);
}

inline double conditioning_standardized_error(const Profile& profile,
                                              double height,
                                              double predicted,
                                              double measured) {
  const int jsp = profile.equation.jsp;
  if (jsp <= 5) {
    return west_standardized_error(profile, height, predicted, measured);
  }
  if (jsp == 22) {
    return (measured - predicted) /
        black_hills_standard_error(profile, height);
  }
  return std::log(measured / predicted) /
      lognormal_standard_error(profile, height);
}

inline double diameter_from_standardized_error(const Profile& profile,
                                               double height,
                                               double predicted,
                                               double error) {
  if (profile.equation.jsp <= 5) {
    return west_diameter_from_error(profile, height, predicted, error);
  }
  const double standard_error = profile.equation.jsp == 22
      ? black_hills_standard_error(profile, height)
      : lognormal_standard_error(profile, height);
  return predicted * std::exp(clamp(error * standard_error, -5.0, 5.0));
}

inline double condition_profile(const Profile& profile, double height,
                                double diameter) {
  if (diameter <= 0.0 || height >= profile.ht) return 0.0;
  double expected_z = 0.0;
  if (profile.extra_count == 1) {
    expected_z = conditioning_correlation(
        profile, profile.extra_height[0], height) *
        profile.extra_z[0];
  } else {
    expected_z = conditioning_correlation(
        profile, profile.extra_height[0], height) *
        profile.inverse_z[0] +
        conditioning_correlation(
            profile, profile.extra_height[1], height) *
        profile.inverse_z[1];
  }
  const double actual = diameter_from_standardized_error(
      profile, height, diameter, expected_z);
  double change = actual - diameter;
  const double maximum = profile.modifier[0];
  if (std::abs(change) / diameter > maximum) {
    change = std::copysign(maximum * diameter, change);
  }
  if (height > kBreastHeight && height < profile.modifier[2]) {
    const double limit = (height - kBreastHeight) /
        (profile.modifier[2] - kBreastHeight) * profile.modifier[1];
    if (std::abs(change) / diameter > limit) {
      change = std::copysign(limit * diameter, change);
    }
  }
  return diameter + change;
}

inline double modeled_diameter(const Profile& profile, double height) {
  const double base = base_diameter(profile, height);
  if (profile.extra_count == 0) return base;
  return condition_profile(profile, height, base);
}

struct SourceDiameterSlope {
  double diameter;
  double slope;
};

inline SourceDiameterSlope source_diameter_slope(const Profile& profile,
                                                  double height,
                                                  bool need_slope) {
  if (height > profile.ht) return {0.0, need_slope ? -1.0 : 0.0};
  const double base = base_diameter(profile, height);
  const double base_slope = need_slope
      ? base_diameter_slope(profile, height) : 0.0;
  if (profile.extra_count == 0) return {base, base_slope};
  const double conditioned = nvel_real(
      condition_profile(profile, height, base));
  if (!need_slope) return {conditioned, 0.0};

  const double nearby_height = nvel_real(height < 0.99 * profile.ht
      ? height + profile.ht / 800.0 : height - profile.ht / 800.0);
  // SF_DS intentionally reuses the first relative height for D2.
  const double nearby_conditioned = nvel_real(condition_profile(
      profile, nearby_height, base));
  const double first_offset = nvel_real(conditioned - base);
  const double second_offset = nvel_real(nearby_conditioned - base);
  const double offset_slope = nvel_real(
      (second_offset - first_offset) / (nearby_height - height));
  return {conditioned, nvel_real(base_slope + offset_slope)};
}

inline double source_inside_diameter(const Profile& profile, double height,
                                     double bark_height) {
  const double value = source_diameter_slope(profile, height, false).diameter;
  if (profile.equation.jsp >= 22 && profile.equation.jsp <= 29) {
    return nvel_real(bark_other_inside(profile, bark_height, value));
  }
  return value;
}

inline Result source_height_at_diameter(const Profile& profile,
                                        double target) {
  const double rhi1 = profile.heights[0];
  const double rhi2 = profile.heights[1];
  const double rhlongi = profile.heights[3];
  const double hi2 = nvel_real(rhi2 * profile.ht);
  double too_high = profile.ht;
  double too_low = 0.0;
  double di2 = source_inside_diameter(profile, hi2, hi2);
  double rh = 0.0;

  if (target > di2) {
    too_high = hi2;
    double di1 = di2;
    if (rhlongi > 0.0) {
      const double hi1 = nvel_real(rhi1 * profile.ht);
      di1 = source_inside_diameter(profile, hi1, hi1);
      if (target < di1) {
        too_low = di1;
        rh = rhi2 - (rhi2 - rhi1) * (target - di2) / (di1 - di2);
      } else {
        too_high = di1;
      }
    }
    if (rh == 0.0) {
      const double base = source_inside_diameter(profile, 0.0, 0.0);
      if (base <= target) return {0.0, 0};
      const double rz = std::pow((target - di1) / (base - di1), 0.25);
      rh = (1.0 - rz) * rhi1;
    }
  } else {
    too_low = di2;
    const double rz = 1.0 - std::pow(target / di2, 2.0);
    rh = rhi2 + (1.0 - rhi2) * rz;
  }

  double height = nvel_real(rh * profile.ht);
  int restart = 0;
  for (;;) {
    bool converged = false;
    bool requested_restart = false;
    for (int iteration = 0; iteration < 30; ++iteration) {
      SourceDiameterSlope evaluated = source_diameter_slope(
          profile, height, true);
      if (profile.equation.jsp >= 22 && profile.equation.jsp <= 29) {
        evaluated.diameter = nvel_real(bark_other_inside(
            profile, hi2, evaluated.diameter));
      }
      const double error = nvel_real(evaluated.diameter - target);
      if (error < 0.0) too_high = height;
      const double adjustment = nvel_real(-error / evaluated.slope);
      height = nvel_real(height + adjustment);
      if (height > profile.ht) {
        height = nvel_real((height - adjustment + profile.ht) / 2.0);
      }
      if (height < 0.0) {
        height = nvel_real((height - adjustment) / 2.0);
      }
      if (std::abs(adjustment) <= 0.0005 * profile.ht &&
          std::abs(error) <= 0.001) {
        converged = true;
        if (evaluated.slope > 0.0 && restart < 2) {
          ++restart;
          height = restart == 1
              ? nvel_real(0.8 * height)
              : nvel_real(height + (profile.ht - height) * 0.25);
          converged = false;
          requested_restart = true;
        }
        break;
      }
    }
    if (converged) return {height, 0};
    if (requested_restart) continue;
    break;
  }

  double high = too_high;
  double low = too_low;
  const double high_error = source_inside_diameter(profile, high, high) - target;
  const double low_error = source_inside_diameter(profile, low, low) - target;
  if (low_error * high_error > 0.0) return {0.0, 0};
  double trial = 0.0;
  for (int iteration = 0; iteration <= 41; ++iteration) {
    trial = nvel_real((high + low) / 2.0);
    const double error = nvel_real(
        source_inside_diameter(profile, trial, trial) - target);
    if (std::abs(error) < 0.002) return {trial, 0};
    if (error > 0.0) {
      low = trial;
    } else {
      high = trial;
    }
  }
  return {trial, 0};
}

inline double inside_diameter(const Profile& profile, double height) {
  const double modeled = modeled_diameter(profile, height);
  if (profile.equation.jsp >= 22 && profile.equation.jsp <= 29) {
    return bark_other_inside(profile, height, modeled);
  }
  return modeled;
}

inline double outside_diameter(const Profile& profile, double height) {
  const double inside = modeled_diameter(profile, height);
  if (profile.equation.jsp >= 31) return 0.0;
  if (profile.equation.jsp >= 22) return inside;
  if (inside <= 0.0) return 0.0;
  const double ratio = profile.equation.jsp >= 11
      ? bark_ratio_ingy_inside(profile, height, inside)
      : bark_ratio_west(profile, height);
  return ratio > 0.0 && ratio < 1.0 ? inside / (1.0 - ratio) : inside;
}

inline Result initialize_profile(const Equation& equation, double dbh, double ht,
                                 const Auxiliary& auxiliary, Profile* profile) {
  if (equation.library_rejected) return {std::numeric_limits<double>::quiet_NaN(), 301};
  if (!(dbh > 0.0) || !(ht > kBreastHeight) || profile == nullptr) {
    return {std::numeric_limits<double>::quiet_NaN(), 54};
  }
  Profile initialized;
  initialized.equation = equation;
  initialized.dbh = nvel_real(dbh);
  initialized.ht = nvel_real(ht);
  dbh = initialized.dbh;
  ht = initialized.ht;
  const double regional_ratio = region_three_bark_ratio(equation);
  if (auxiliary.bark_ratio > 0.0 && auxiliary.bark_ratio <= 1.0) {
    initialized.dbt_bh = nvel_real(dbh * (1.0 - auxiliary.bark_ratio));
  } else if (regional_ratio > 0.0) {
    initialized.dbt_bh = nvel_real(dbh * (1.0 - regional_ratio));
  } else if (equation.jsp <= 5) {
    initialized.dbt_bh = nvel_real(bark_thickness_west(equation, dbh, ht));
  } else if (equation.jsp <= 21) {
    initialized.dbt_bh = nvel_real(bark_thickness_ingy(equation, dbh, ht));
  } else if (equation.jsp >= 31) {
    initialized.dbt_bh = nvel_real(bark_thickness_alaska(equation.jsp, dbh, ht));
  } else {
    initialized.dbt_bh = nvel_real(dbh - other_inside_at_bh(equation, dbh));
  }
  shape_profile(equation, dbh, ht, &initialized);
  calculate_taper(&initialized);
  const double breast_height_diameter = equation.jsp >= 22 && equation.jsp <= 29
      ? dbh : dbh - initialized.dbt_bh;
  const double unscaled_bh = base_diameter(initialized, kBreastHeight);
  initialized.scale = nvel_real(breast_height_diameter / unscaled_bh);

  if (equation.three_point && equation.condition_upper) {
    initialized.extra_count = std::isfinite(auxiliary.upper_ht2) &&
            std::isfinite(auxiliary.upper_d2)
        ? 2 : 1;
    initialized.extra_height[0] = auxiliary.upper_ht1;
    initialized.extra_height[1] = auxiliary.upper_ht2;
    initialized.modifier = {{0.15, 0.0, 0.0}};
    for (int index = 0; index < initialized.extra_count; ++index) {
      const double height = initialized.extra_height[static_cast<std::size_t>(index)];
      double measured = index == 0 ? auxiliary.upper_d1 : auxiliary.upper_d2;
      if (auxiliary.upper_bark == 1) {
        if (equation.jsp >= 31) {
          return {std::numeric_limits<double>::quiet_NaN(), 53};
        }
        if (equation.jsp <= 5) {
          measured *= 1.0 - bark_ratio_west(initialized, height);
        } else if (equation.jsp <= 21) {
          const double ratio = combine_ingy_bark(
              initialized, height,
              bark_ratio_ingy_diameter(initialized, height, measured),
              bark_ratio_ingy_height(initialized, height));
          measured *= 1.0 - ratio;
        }
      } else if (equation.jsp >= 22 && equation.jsp <= 29) {
        measured = bark_other_outside(initialized, height, measured);
      }
      const double predicted = base_diameter(initialized, height);
      if (!(measured > 0.0) || !(predicted > 0.0)) {
        return {std::numeric_limits<double>::quiet_NaN(), 54};
      }
      const double standardized_error = conditioning_standardized_error(
          initialized, height, predicted, measured);
      if (!std::isfinite(standardized_error)) {
        return {std::numeric_limits<double>::quiet_NaN(), 54};
      }
      initialized.extra_z[static_cast<std::size_t>(index)] = standardized_error;
      const double absolute_change = std::abs(measured - predicted);
      initialized.modifier[0] = std::max(initialized.modifier[0],
                                         absolute_change / predicted);
      if (height > kBreastHeight &&
          (initialized.modifier[2] == 0.0 || height < initialized.modifier[2])) {
        initialized.modifier[1] = std::min(predicted, 2.0 * absolute_change) / predicted;
        initialized.modifier[2] = height;
      }
    }
    if (initialized.extra_count == 2) {
      const double correlation = conditioning_correlation(
          initialized, initialized.extra_height[0], initialized.extra_height[1]);
      const double divisor = 1.0 - correlation * correlation;
      if (!std::isfinite(divisor) || divisor == 0.0) {
        return {std::numeric_limits<double>::quiet_NaN(), 54};
      }
      initialized.inverse_z[0] = (initialized.extra_z[0] -
          correlation * initialized.extra_z[1]) / divisor;
      initialized.inverse_z[1] = (initialized.extra_z[1] -
          correlation * initialized.extra_z[0]) / divisor;
    }
  }
  *profile = initialized;
  return {0.0, 0};
}

inline Result evaluate_diameter(const Equation& equation, bool outside,
                                double dbh, double ht, double height,
                                const Auxiliary& auxiliary) {
  Profile profile;
  const Result initialized = initialize_profile(equation, dbh, ht, auxiliary, &profile);
  if (initialized.status != 0) return initialized;
  const double value = outside ? outside_diameter(profile, height)
                               : inside_diameter(profile, height);
  return std::isfinite(value) ? Result{value, 0}
                              : Result{std::numeric_limits<double>::quiet_NaN(), 54};
}

inline double small_tree_diameter(const Profile& profile) {
  double diameter = inside_diameter(profile, 1.0);
  double minimum = 0.18 + (profile.ht - 5.0) / 10.0 * (0.11 - 0.18);
  double maximum = 0.18 + (profile.ht - 5.0) / 10.0 * (0.25 - 0.18);
  if (profile.equation.jsp == 14) {
    minimum = 0.25 + (profile.ht - 5.0) / 10.0 * (0.21 - 0.25);
    maximum = 0.25 + (profile.ht - 5.0) / 10.0 * (0.27 - 0.25);
  }
  double ratio = clamp(diameter / (profile.ht - 1.0), minimum, maximum);
  diameter = ratio * (profile.ht - 1.0);
  double diameter_ratio = 1.0;
  if (profile.ht < 15.0) diameter_ratio += 0.3 * (15.0 - profile.ht) / 10.0;
  const double breast_inside = profile.dbh - profile.dbt_bh;
  return std::max(diameter, diameter_ratio * breast_inside);
}

inline Result smalian_volume(const Equation& equation, double dbh, double ht,
                             double lower, double upper,
                             const Auxiliary& auxiliary) {
  Profile profile;
  const Result initialized = initialize_profile(equation, dbh, ht, auxiliary, &profile);
  if (initialized.status != 0) return initialized;
  if (!(lower >= 0.0) || !(upper <= ht) || !(lower < upper)) {
    return {std::numeric_limits<double>::quiet_NaN(), 54};
  }
  double volume = 0.0;
  double height = lower;
  double large = inside_diameter(profile, height);
  if (lower == 0.0) {
    height = std::min(1.0, upper);
    large = ht <= 15.0 ? small_tree_diameter(profile)
                       : inside_diameter(profile, height);
    volume = kPiNvel * std::pow(large / 2.0, 2.0) * height / 144.0;
  }
  while (height + 4.0 <= upper) {
    const double next_height = height + 4.0;
    const double small = inside_diameter(profile, next_height);
    volume += kSmalian * (large * large + small * small) * 4.0;
    height = next_height;
    large = small;
  }
  if (height < upper) {
    const double small = inside_diameter(profile, upper);
    volume += kSmalian * (large * large + small * small) * (upper - height);
  }
  if (lower == 0.0 && upper == profile.ht) {
    volume = std::floor(volume * 10.0 + 0.5) / 10.0;
  }
  return std::isfinite(volume) ? Result{volume, 0}
                               : Result{std::numeric_limits<double>::quiet_NaN(), 54};
}

}  // namespace flewelling
}  // namespace treevolume

#endif
