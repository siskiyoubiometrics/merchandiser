#ifndef TREEVOLUME_PUBLISHED_TAPERS_HPP
#define TREEVOLUME_PUBLISHED_TAPERS_HPP

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <string>

namespace treevolume {
namespace published_tapers {

enum class Form {
  kozak_1988,
  kozak_2002,
  max_burkhart,
  unknown
};

inline Form parse_form(const std::string& value) {
  if (value == "kozak_1988") {
    return Form::kozak_1988;
  }
  if (value == "kozak_2002") {
    return Form::kozak_2002;
  }
  if (value == "max_burkhart") {
    return Form::max_burkhart;
  }
  return Form::unknown;
}

inline std::size_t coefficient_count(Form form) {
  if (form == Form::max_burkhart) {
    return 6;
  }
  if (form == Form::kozak_1988 || form == Form::kozak_2002) {
    return 9;
  }
  return 0;
}

inline double missing_value() {
  return std::numeric_limits<double>::quiet_NaN();
}

inline bool finite_coefficients(Form form, const double* coefficients,
                                std::size_t size) {
  if (coefficients == nullptr || size != coefficient_count(form)) {
    return false;
  }
  for (std::size_t index = 0; index < size; ++index) {
    if (!std::isfinite(coefficients[index])) {
      return false;
    }
  }
  if (form == Form::kozak_1988) {
    return coefficients[0] > 0.0 && coefficients[2] > 0.0 &&
        coefficients[8] > 0.0 && coefficients[8] < 1.0;
  }
  if (form == Form::kozak_2002) {
    return coefficients[0] > 0.0;
  }
  if (form == Form::max_burkhart) {
    return coefficients[5] > 0.0 && coefficients[5] < coefficients[4] &&
        coefficients[4] < 1.0;
  }
  return false;
}

inline double kozak_1988_dib(const double* coefficients, double dbh,
                             double ht, double h) {
  if (!(dbh > 0.0) || !(ht > 0.0) || h < 0.0 || h > ht) {
    return missing_value();
  }
  if (h == ht) {
    return 0.0;
  }
  const double z = h / ht;
  const double denominator = 1.0 - std::sqrt(coefficients[8]);
  const double x = (1.0 - std::sqrt(z)) / denominator;
  if (!(x > 0.0)) {
    return missing_value();
  }
  const double exponent = coefficients[3] * z * z +
      coefficients[4] * std::log(z + 0.001) +
      coefficients[5] * std::sqrt(z) + coefficients[6] * std::exp(z) +
      coefficients[7] * dbh / ht;
  const double value = coefficients[0] * std::pow(dbh, coefficients[1]) *
      std::pow(coefficients[2], dbh) * std::pow(x, exponent);
  return std::isfinite(value) && value >= 0.0 ? value : missing_value();
}

inline double kozak_2002_dib(const double* coefficients, double dbh,
                             double ht, double h) {
  if (!(dbh > 0.0) || !(ht > 1.3) || h < 0.0 || h > ht) {
    return missing_value();
  }
  if (h == ht) {
    return 0.0;
  }
  const double z = h / ht;
  const double q = 1.0 - std::cbrt(z);
  const double denominator = 1.0 - std::cbrt(1.3 / ht);
  const double x = q / denominator;
  if (!(x > 0.0)) {
    return missing_value();
  }
  const double exponent = coefficients[3] * std::pow(z, 4.0) +
      coefficients[4] * std::exp(-dbh / ht) +
      coefficients[5] * std::pow(x, 0.1) + coefficients[6] / dbh +
      coefficients[7] * std::pow(ht, q) + coefficients[8] * x;
  const double value = coefficients[0] * std::pow(dbh, coefficients[1]) *
      std::pow(ht, coefficients[2]) * std::pow(x, exponent);
  return std::isfinite(value) && value >= 0.0 ? value : missing_value();
}

inline double max_burkhart_squared_ratio(const double* coefficients,
                                         double relative_height) {
  const double b1 = coefficients[0];
  const double b2 = coefficients[1];
  const double b3 = coefficients[2];
  const double b4 = coefficients[3];
  const double a1 = coefficients[4];
  const double a2 = coefficients[5];
  double value = b1 * (relative_height - 1.0) +
      b2 * (relative_height * relative_height - 1.0);
  if (relative_height <= a1) {
    value += b3 * std::pow(a1 - relative_height, 2.0);
  }
  if (relative_height <= a2) {
    value += b4 * std::pow(a2 - relative_height, 2.0);
  }
  return value;
}

inline double max_burkhart_dib(const double* coefficients, double dbh,
                               double ht, double h) {
  if (!(dbh > 0.0) || !(ht > 0.0) || h < 0.0 || h > ht) {
    return missing_value();
  }
  if (h == ht) {
    return 0.0;
  }
  double squared_ratio = max_burkhart_squared_ratio(coefficients, h / ht);
  if (squared_ratio < 0.0 && squared_ratio > -1e-8) {
    squared_ratio = 0.0;
  }
  if (!(squared_ratio >= 0.0)) {
    return missing_value();
  }
  return dbh * std::sqrt(squared_ratio);
}

inline double inside_diameter(Form form, const double* coefficients,
                              std::size_t size, double dbh, double ht,
                              double h) {
  if (!finite_coefficients(form, coefficients, size)) {
    return missing_value();
  }
  if (form == Form::kozak_1988) {
    return kozak_1988_dib(coefficients, dbh, ht, h);
  }
  if (form == Form::kozak_2002) {
    return kozak_2002_dib(coefficients, dbh, ht, h);
  }
  if (form == Form::max_burkhart) {
    return max_burkhart_dib(coefficients, dbh, ht, h);
  }
  return missing_value();
}

inline void add_quadratic_root(double quadratic, double linear,
                               double constant, double lower, double upper,
                               double* highest) {
  constexpr double tolerance = 1e-10;
  auto consider = [lower, upper, highest](double root) {
    if (std::isfinite(root) && root >= lower - 1e-10 &&
        root <= upper + 1e-10) {
      const double bounded = std::max(lower, std::min(upper, root));
      *highest = std::isfinite(*highest) ? std::max(*highest, bounded) : bounded;
    }
  };
  if (std::abs(quadratic) <= tolerance) {
    if (std::abs(linear) > tolerance) {
      consider(-constant / linear);
    }
    return;
  }
  double discriminant = linear * linear - 4.0 * quadratic * constant;
  if (discriminant < 0.0 && discriminant > -tolerance) {
    discriminant = 0.0;
  }
  if (discriminant < 0.0) {
    return;
  }
  const double root = std::sqrt(discriminant);
  consider((-linear - root) / (2.0 * quadratic));
  consider((-linear + root) / (2.0 * quadratic));
}

inline double max_burkhart_height(const double* coefficients, double dbh,
                                  double ht, double diameter,
                                  double stump) {
  if (!(dbh > 0.0) || !(ht > 0.0) || !(diameter >= 0.0) ||
      stump < 0.0 || stump > ht) {
    return missing_value();
  }
  const double target = std::pow(diameter / dbh, 2.0);
  const double a1 = coefficients[4];
  const double a2 = coefficients[5];
  const double lower_bound = stump / ht;
  const double breaks[] = {lower_bound, a2, a1, 1.0};
  double highest = missing_value();
  for (int segment = 0; segment < 3; ++segment) {
    const double lower = std::max(lower_bound, breaks[segment]);
    const double upper = std::max(lower_bound, breaks[segment + 1]);
    if (lower > upper || lower > 1.0 || upper < lower_bound) {
      continue;
    }
    const double midpoint = (lower + upper) / 2.0;
    double quadratic = coefficients[1];
    double linear = coefficients[0];
    double constant = -coefficients[0] - coefficients[1] - target;
    if (midpoint <= a1) {
      quadratic += coefficients[2];
      linear -= 2.0 * coefficients[2] * a1;
      constant += coefficients[2] * a1 * a1;
    }
    if (midpoint <= a2) {
      quadratic += coefficients[3];
      linear -= 2.0 * coefficients[3] * a2;
      constant += coefficients[3] * a2 * a2;
    }
    add_quadratic_root(quadratic, linear, constant, lower,
                       std::min(upper, 1.0), &highest);
  }
  return std::isfinite(highest) ? highest * ht : missing_value();
}

inline double max_burkhart_primitive_base(const double* coefficients,
                                          double x) {
  return coefficients[0] * (x * x / 2.0 - x) +
      coefficients[1] * (x * x * x / 3.0 - x);
}

inline double max_burkhart_knot_integral(double coefficient, double knot,
                                         double lower, double upper) {
  if (lower >= knot) {
    return 0.0;
  }
  const double right = std::min(upper, knot);
  return coefficient * (std::pow(knot - lower, 3.0) -
      std::pow(knot - right, 3.0)) / 3.0;
}

inline double max_burkhart_volume(const double* coefficients, double dbh,
                                  double ht, double lower, double upper) {
  if (!(dbh > 0.0) || !(ht > 0.0) || lower < 0.0 || upper > ht ||
      !(lower < upper)) {
    return missing_value();
  }
  const double x_lower = lower / ht;
  const double x_upper = upper / ht;
  double integral = max_burkhart_primitive_base(coefficients, x_upper) -
      max_burkhart_primitive_base(coefficients, x_lower);
  integral += max_burkhart_knot_integral(
      coefficients[2], coefficients[4], x_lower, x_upper);
  integral += max_burkhart_knot_integral(
      coefficients[3], coefficients[5], x_lower, x_upper);
  const double value = 3.14159265358979323846 * dbh * dbh * ht * integral /
      40000.0;
  return std::isfinite(value) && value >= 0.0 ? value : missing_value();
}

}  // namespace published_tapers
}  // namespace treevolume

#endif
