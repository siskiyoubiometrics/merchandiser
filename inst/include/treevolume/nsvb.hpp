#ifndef TREEVOLUME_NSVB_HPP
#define TREEVOLUME_NSVB_HPP

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <string>

#include <treevolume/nsvb_coefficients.hpp>
#include <treevolume/nsvb_scribner.hpp>
#include <treevolume/nsvb_species.hpp>

namespace treevolume {
namespace nsvb {

using nsvb_data::EquationCoefficient;
using nsvb_data::GroupCoefficient;

struct Equation {
  std::string id;
  int species = 0;
  int division = 0;
  int exact_division = 0;
  int stdorg = 0;
};

struct Result {
  double value = std::numeric_limits<double>::quiet_NaN();
  int status = 0;
};

struct Table {
  const EquationCoefficient* species;
  std::size_t species_count;
  const GroupCoefficient* group;
  std::size_t group_count;
};

struct BiomassOptions {
  int region = 0;
  int forest = 0;
  int decay_class = 0;
  double cull = 0.0;
  double primary_top = 6.0;
  double secondary_top = 4.0;
  double stump = 1.0;
  double max_log_length = std::numeric_limits<double>::quiet_NaN();
  double min_log_length = std::numeric_limits<double>::quiet_NaN();
  double minimum_top_length = std::numeric_limits<double>::quiet_NaN();
  double merchantable_length = std::numeric_limits<double>::quiet_NaN();
  double trim = std::numeric_limits<double>::quiet_NaN();
  int even_or_odd = 0;
  int option = 0;
  int corrected_scribner = -1;
  int ctype = 'C';
};

struct BiomassResult {
  double value[30];
  double volume[15];
  int n_logs_primary = 0;
  int n_logs_secondary = 0;
  int status = 0;

  BiomassResult() {
    std::fill(value, value + 30, std::numeric_limits<double>::quiet_NaN());
    std::fill(volume, volume + 15,
              std::numeric_limits<double>::quiet_NaN());
  }
};

struct MerchRules {
  int evod = 2;
  int option = 22;
  double max_length = 16.0;
  double min_length = 2.0;
  double minimum_merch_length = 8.0;
  double minimum_top_length = 2.0;
  double trim = 0.5;
  bool corrected_scribner = true;
};

struct Segments {
  double length[20];
  int count = 0;

  Segments() {
    std::fill(length, length + 20, 0.0);
  }

  double occupied_height(double trim) const {
    double result = 0.0;
    for (int index = 0; index < count; ++index) {
      result += length[index] + trim;
    }
    return result;
  }
};

inline bool digit(char value) {
  return value >= '0' && value <= '9';
}

inline int digits(const std::string& value, std::size_t begin,
                  std::size_t count) {
  int result = 0;
  for (std::size_t index = begin; index < begin + count; ++index) {
    if (index >= value.size() || !digit(value[index])) {
      return -1;
    }
    result = result * 10 + value[index] - '0';
  }
  return result;
}

inline bool parse_equation(const std::string& id, Equation* equation) {
  if (equation == nullptr || (id.size() != 10 && id.size() != 11) ||
      id.compare(0, 3, "NVB") != 0 || (id[3] != '0' && id[3] != 'M')) {
    return false;
  }
  const int division_digits = digits(id, 4, 3);
  const int species = digits(id, 7, 3);
  if (division_digits < 0 || species < 0 ||
      (id.size() == 11 && id[10] != 'P')) {
    return false;
  }
  equation->id = id;
  equation->species = species;
  equation->exact_division = division_digits + (id[3] == 'M' ? 1000 : 0);
  equation->division = (division_digits / 10) * 10 +
      (id[3] == 'M' ? 1000 : 0);
  equation->stdorg = id.size() == 11 ? 1 : 0;
  return true;
}

inline const nsvb_data::Species* find_species(int spcd) {
  std::size_t low = 0;
  std::size_t high = nsvb_data::countof(nsvb_data::species);
  while (low < high) {
    const std::size_t middle = low + (high - low) / 2;
    const int candidate = static_cast<int>(nsvb_data::species[middle].spcd);
    if (candidate < spcd) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  if (low < nsvb_data::countof(nsvb_data::species) &&
      static_cast<int>(nsvb_data::species[low].spcd) == spcd) {
    return &nsvb_data::species[low];
  }
  return nullptr;
}

inline const nsvb_data::Species* default_species() {
  return find_species(999);
}

inline Table coefficient_table(int number) {
  switch (number) {
    case 1:
      return {nsvb_data::table1, nsvb_data::countof(nsvb_data::table1),
              nsvb_data::table1_group,
              nsvb_data::countof(nsvb_data::table1_group)};
    case 2:
      return {nsvb_data::table2, nsvb_data::countof(nsvb_data::table2),
              nsvb_data::table2_group,
              nsvb_data::countof(nsvb_data::table2_group)};
    case 3:
      return {nsvb_data::table3, nsvb_data::countof(nsvb_data::table3),
              nsvb_data::table3_group,
              nsvb_data::countof(nsvb_data::table3_group)};
    case 4:
      return {nsvb_data::table4, nsvb_data::countof(nsvb_data::table4),
              nsvb_data::table4_group,
              nsvb_data::countof(nsvb_data::table4_group)};
    case 5:
      return {nsvb_data::table5, nsvb_data::countof(nsvb_data::table5),
              nsvb_data::table5_group,
              nsvb_data::countof(nsvb_data::table5_group)};
    case 6:
      return {nsvb_data::table6, nsvb_data::countof(nsvb_data::table6),
              nsvb_data::table6_group,
              nsvb_data::countof(nsvb_data::table6_group)};
    case 7:
      return {nsvb_data::table7, nsvb_data::countof(nsvb_data::table7),
              nsvb_data::table7_group,
              nsvb_data::countof(nsvb_data::table7_group)};
    case 8:
      return {nsvb_data::table8, nsvb_data::countof(nsvb_data::table8),
              nsvb_data::table8_group,
              nsvb_data::countof(nsvb_data::table8_group)};
    default:
      return {nsvb_data::table9, nsvb_data::countof(nsvb_data::table9),
              nsvb_data::table9_group,
              nsvb_data::countof(nsvb_data::table9_group)};
  }
}

inline const EquationCoefficient* species_coefficient(
    const Table& table, const Equation& equation) {
  for (std::size_t index = 0; index < table.species_count; ++index) {
    const EquationCoefficient& coefficient = table.species[index];
    if (static_cast<int>(coefficient.spcd) != equation.species) {
      continue;
    }
    if (static_cast<int>(coefficient.stdorg) == equation.stdorg &&
        (static_cast<int>(coefficient.division) == equation.division ||
         static_cast<int>(coefficient.division) == 0)) {
      return &coefficient;
    }
  }
  return nullptr;
}

inline const GroupCoefficient* group_coefficient(const Table& table,
                                                  int group) {
  for (std::size_t index = 0; index < table.group_count; ++index) {
    if (static_cast<int>(table.group[index].group) == group) {
      return &table.group[index];
    }
  }
  return nullptr;
}

inline double evaluate_equation(const EquationCoefficient& coefficient,
                                int spcd, double diameter, double height,
                                double wood_density) {
  const int form = static_cast<int>(coefficient.equation);
  if (form == 1) {
    return coefficient.a * std::pow(diameter, coefficient.b) *
        std::pow(height, coefficient.c);
  }
  if (form == 2) {
    const double knot = spcd < 300 ? 9.0 : 11.0;
    if (diameter < knot) {
      return coefficient.a0 * std::pow(diameter, coefficient.b0) *
          std::pow(height, coefficient.c);
    }
    return coefficient.a0 * std::pow(knot, coefficient.b0 - coefficient.b1) *
        std::pow(diameter, coefficient.b1) * std::pow(height, coefficient.c);
  }
  if (form == 3) {
    const double exponent = coefficient.a1 *
        std::pow(1.0 - std::exp(-coefficient.b1 * diameter), coefficient.c1);
    return coefficient.a * std::pow(diameter, exponent) *
        std::pow(height, coefficient.c);
  }
  if (form == 4) {
    return coefficient.a * std::pow(diameter, coefficient.b) *
        std::pow(height, coefficient.c) *
        std::exp(-(coefficient.b2 * diameter));
  }
  if (form == 5) {
    return coefficient.a * std::pow(diameter, coefficient.b) *
        std::pow(height, coefficient.c) * wood_density / 62.4;
  }
  return 0.0;
}

inline double evaluate_group(const GroupCoefficient& coefficient,
                             double diameter, double height,
                             double wood_density) {
  if (static_cast<int>(coefficient.equation) == 5) {
    return coefficient.a * std::pow(diameter, coefficient.b) *
        std::pow(height, coefficient.c) * wood_density / 62.4;
  }
  return coefficient.a * std::pow(diameter, coefficient.b) *
      std::pow(height, coefficient.c);
}

inline Result volume_weight(const Equation& equation, int table_number,
                            double diameter, double height,
                            const nsvb_data::Species& reference) {
  if (diameter < 1.0) {
    return {std::numeric_limits<double>::quiet_NaN(), 303};
  }
  if (height < 5.0) {
    return {std::numeric_limits<double>::quiet_NaN(), 304};
  }
  const Table table = coefficient_table(table_number);
  const EquationCoefficient* coefficient = species_coefficient(table, equation);
  const nsvb_data::Species* default_reference = default_species();
  const double wood_density =
      reference.wood_dry == 0.0 && default_reference != nullptr
      ? default_reference->wood_dry : reference.wood_dry;
  double value = 0.0;
  if (coefficient != nullptr) {
    value = evaluate_equation(*coefficient, equation.species, diameter, height,
                              wood_density);
  } else {
    const GroupCoefficient* fallback = group_coefficient(
        table, static_cast<int>(reference.group));
    if (fallback != nullptr) {
      value = evaluate_group(*fallback, diameter, height, wood_density);
    } else {
      value = evaluate_equation(table.species[table.species_count - 1],
                                equation.species, diameter, height,
                                wood_density);
    }
  }
  if (!std::isfinite(value)) {
    return {std::numeric_limits<double>::quiet_NaN(), 54};
  }
  return {value, 0};
}

inline bool ratio_coefficients(const Equation& equation, int table_number,
                               const nsvb_data::Species& reference,
                               double* a, double* b) {
  const Table table = coefficient_table(table_number);
  const EquationCoefficient* coefficient = species_coefficient(table, equation);
  if (coefficient != nullptr) {
    *a = coefficient->a;
    *b = coefficient->b;
    return true;
  }
  const GroupCoefficient* fallback = group_coefficient(
      table, static_cast<int>(reference.group));
  if (fallback != nullptr) {
    *a = fallback->a;
    *b = fallback->b;
    return true;
  }
  *a = table.species[table.species_count - 1].a;
  *b = table.species[table.species_count - 1].b;
  return true;
}

inline double ratio(double height, double point, double a, double b) {
  if (!(point > 0.0) || point > height) {
    return 0.0;
  }
  return std::pow(1.0 - std::pow(1.0 - point / height, a), b);
}

inline double diameter_at_height(double total_volume, double a, double b,
                                 double total_height, double height) {
  if (height >= total_height) {
    return 0.0;
  }
  const double x = height / total_height;
  const double derivative = a * b * std::pow(1.0 - x, a - 1.0) *
      std::pow(1.0 - std::pow(1.0 - x, a), b - 1.0);
  return std::sqrt(total_volume / 0.005454154 / total_height * derivative);
}

inline double height_at_diameter(double total_volume, double a, double b,
                                 double total_height, double top_diameter) {
  double low = 0.0;
  double high = total_height;
  double difference = 1.0;
  double middle = 0.0;
  int loop_count = 0;
  while (std::fabs(difference) > 0.001) {
    middle = (low + high) / 2.0;
    if (middle < 0.5) {
      middle = 0.0;
      break;
    }
    difference = top_diameter - diameter_at_height(
        total_volume, a, b, total_height, middle);
    if (std::fabs(difference) < 0.001) {
      break;
    }
    if (difference < 0.0) {
      low = middle;
    } else {
      high = middle;
    }
    ++loop_count;
    if (loop_count > 1000) {
      break;
    }
  }
  return middle;
}

inline Result profile_value(const Equation& equation, double dbh, double ht,
                            double height, bool outside) {
  const nsvb_data::Species* reference = find_species(equation.species);
  if (reference == nullptr) {
    return {std::numeric_limits<double>::quiet_NaN(), 306};
  }
  if (dbh < 1.0) {
    return {std::numeric_limits<double>::quiet_NaN(), 303};
  }
  if (ht < 5.0) {
    return {std::numeric_limits<double>::quiet_NaN(), 304};
  }
  if (height < 0.0 || height > ht) {
    return {std::numeric_limits<double>::quiet_NaN(), 4};
  }
  const Result outside_volume = volume_weight(equation, 3, dbh, ht, *reference);
  const Result inside_volume = volume_weight(equation, 1, dbh, ht, *reference);
  const Result bark_volume = volume_weight(equation, 2, dbh, ht, *reference);
  if (outside_volume.status != 0 || inside_volume.status != 0 ||
      bark_volume.status != 0) {
    return {std::numeric_limits<double>::quiet_NaN(),
            std::max(outside_volume.status,
                     std::max(inside_volume.status, bark_volume.status))};
  }
  double a = 0.0;
  double b = 0.0;
  ratio_coefficients(equation, 4, *reference, &a, &b);
  double value = diameter_at_height(outside_volume.value, a, b, ht, height);
  if (!outside) {
    value *= std::sqrt(inside_volume.value /
                       (inside_volume.value + bark_volume.value));
  }
  return std::isfinite(value) ? Result{value, 0} :
      Result{std::numeric_limits<double>::quiet_NaN(), 54};
}

inline Result inverse_value(const Equation& equation, double dbh, double ht,
                            double target) {
  const nsvb_data::Species* reference = find_species(equation.species);
  if (reference == nullptr) {
    return {std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const Result outside_volume = volume_weight(equation, 3, dbh, ht, *reference);
  const Result inside_volume = volume_weight(equation, 1, dbh, ht, *reference);
  const Result bark_volume = volume_weight(equation, 2, dbh, ht, *reference);
  if (outside_volume.status != 0 || inside_volume.status != 0 ||
      bark_volume.status != 0) {
    return {std::numeric_limits<double>::quiet_NaN(),
            std::max(outside_volume.status,
                     std::max(inside_volume.status, bark_volume.status))};
  }
  const double inside_outside = std::sqrt(
      inside_volume.value / (inside_volume.value + bark_volume.value));
  const double outside_target = target / inside_outside;
  double a = 0.0;
  double b = 0.0;
  ratio_coefficients(equation, 4, *reference, &a, &b);
  double value = height_at_diameter(
      outside_volume.value, a, b, ht, outside_target);
  if (value < 5.0 && outside_target < dbh) {
    value = 5.0;
  }
  return std::isfinite(value) ? Result{value, 0} :
      Result{std::numeric_limits<double>::quiet_NaN(), 54};
}

inline Result volume_value(const Equation& equation, double dbh, double ht,
                           double lower, double upper) {
  const nsvb_data::Species* reference = find_species(equation.species);
  if (reference == nullptr) {
    return {std::numeric_limits<double>::quiet_NaN(), 306};
  }
  const Result inside_volume = volume_weight(equation, 1, dbh, ht, *reference);
  if (inside_volume.status != 0) {
    return inside_volume;
  }
  double a = 0.0;
  double b = 0.0;
  ratio_coefficients(equation, 5, *reference, &a, &b);
  const double value = inside_volume.value *
      (ratio(ht, upper, a, b) - ratio(ht, lower, a, b));
  return std::isfinite(value) ? Result{value, 0} :
      Result{std::numeric_limits<double>::quiet_NaN(), 54};
}

inline MerchRules merch_rules(int region, const BiomassOptions& options) {
  MerchRules rules;
  if (region == 1) {
    rules.minimum_top_length = 16.0;
  } else if (region == 6 || region == 11) {
    rules.option = 23;
    rules.corrected_scribner = false;
  } else if (region == 7) {
    rules.option = 23;
    rules.corrected_scribner = false;
  } else if (region == 8) {
    rules.max_length = 8.0;
  } else if (region == 9) {
    rules.max_length = 8.0;
    rules.minimum_top_length = 4.0;
    rules.trim = 0.3;
  } else if (region == 10) {
    rules.min_length = 8.0;
    rules.minimum_top_length = 8.0;
    rules.option = 23;
  }
  if (std::isfinite(options.max_log_length) && options.max_log_length > 0.1) {
    rules.max_length = options.max_log_length;
  }
  if (std::isfinite(options.min_log_length) && options.min_log_length > 0.1) {
    rules.min_length = options.min_log_length;
  }
  if (std::isfinite(options.minimum_top_length) &&
      options.minimum_top_length > 0.1) {
    rules.minimum_top_length = options.minimum_top_length;
  }
  if (std::isfinite(options.merchantable_length) &&
      options.merchantable_length > 0.1) {
    rules.minimum_merch_length = options.merchantable_length;
  }
  if (std::isfinite(options.trim) && options.trim > 0.1) {
    rules.trim = options.trim;
  }
  if (options.even_or_odd > 0) rules.evod = options.even_or_odd;
  if (options.option > 0) rules.option = options.option;
  if (options.corrected_scribner >= 0) {
    rules.corrected_scribner = options.corrected_scribner != 0;
  }
  return rules;
}

inline int number_logs(const MerchRules& rules, double merchantable_length,
                       double minimum_length) {
  int segments = static_cast<int>(merchantable_length /
                                  (rules.max_length + rules.trim));
  const double leftover = merchantable_length -
      (rules.max_length + rules.trim) * static_cast<double>(segments);
  if (segments > 0 || leftover >= minimum_length) {
    if (rules.option < 20 && leftover >= rules.trim + 0.5) {
      ++segments;
    } else if ((rules.option == 21 || rules.option == 22) &&
               ((rules.evod == 1 && leftover >= rules.trim + 0.5) ||
                (rules.evod == 2 && leftover >= rules.trim + 1.0))) {
      ++segments;
    } else if (rules.option == 23 &&
               leftover >= rules.trim + minimum_length) {
      ++segments;
    } else if (rules.option == 24 &&
               leftover >= (rules.max_length + rules.trim) / 4.0) {
      ++segments;
    }
  } else {
    segments = 0;
  }
  return std::min(segments, 20);
}

inline Segments segment_logs(const MerchRules& rules,
                             double merchantable_length,
                             double number_log_minimum,
                             double segment_minimum) {
  Segments output;
  int segments = number_logs(
      rules, merchantable_length, number_log_minimum);
  if (segments == 0) {
    return output;
  }
  double available = merchantable_length - segments * rules.trim;
  if (rules.evod == 1) {
    available = std::floor(available + 0.5);
  } else {
    available = std::floor((available + 1.0) / 2.0) * 2.0;
  }
  available = std::min(available, segments * rules.max_length);
  if (segments == 1) {
    if (rules.option == 24) {
      if (available < rules.max_length * 0.25) {
        output.length[0] = 0.0;
      } else if (available <= rules.max_length * 0.75) {
        output.length[0] = rules.max_length / 2.0;
      } else {
        output.length[0] = rules.max_length;
      }
    } else if (available >= segment_minimum) {
      output.length[0] = std::min(available, rules.max_length);
    }
    output.count = output.length[0] > 0.0 ? 1 : 0;
    return output;
  }

  if (rules.option < 20) {
    const int average = static_cast<int>(available / segments);
    double leftover = available - average * segments;
    for (int index = 0; index < segments; ++index) {
      output.length[index] = average;
    }
    if (average > static_cast<int>(average / 2.0) * 2.0) {
      for (int index = 0; index < segments; ++index) {
        if (segments - 2 * (index + 1) + 1 >= 1) {
          output.length[index] += 1.0;
          output.length[segments - index - 1] -= 1.0;
        }
      }
    }
    if (leftover > 0.0 &&
        segments > static_cast<int>(segments / 2.0) * 2.0) {
      for (int index = 0; index < segments && leftover > 0.0; ++index) {
        if (output.length[index] >
            static_cast<int>(output.length[index] / 2.0) * 2.0) {
          output.length[index] += 1.0;
          leftover -= 1.0;
        }
      }
    }
    int iteration = 0;
    while (leftover > 0.0 && iteration <= 500) {
      for (int index = 0; index < segments && leftover > 0.0; ++index) {
        if (output.length[index] >= rules.max_length) {
          continue;
        }
        const double addition = leftover >= 2.0 ? 2.0 : 1.0;
        if (output.length[index] == output.length[segments - 1]) {
          output.length[index] += addition;
          leftover -= addition;
        } else if (index + 1 < segments &&
                   output.length[index] > output.length[index + 1]) {
          output.length[index + 1] += addition;
          leftover -= addition;
        }
      }
      ++iteration;
    }
  } else {
    const double leftover = available -
        static_cast<int>(rules.max_length) * (segments - 1);
    for (int index = 0; index < segments; ++index) {
      output.length[index] = rules.max_length;
    }
    if (rules.option == 21) {
      if (leftover >= rules.max_length / 2.0) {
        output.length[segments - 1] = leftover;
      } else {
        output.length[segments - 1] =
            static_cast<int>((rules.max_length + leftover) / 2.0);
        output.length[segments - 2] = rules.max_length + leftover -
            output.length[segments - 1];
        if (output.length[segments - 1] == output.length[segments - 2] &&
            output.length[segments - 1] >
            static_cast<int>(output.length[segments - 1] / 2.0) * 2.0) {
          output.length[segments - 1] -= 1.0;
          output.length[segments - 2] += 1.0;
        }
      }
    } else if (rules.option == 22) {
      output.length[segments - 1] =
          static_cast<int>((rules.max_length + leftover) / 2.0);
      output.length[segments - 2] = rules.max_length + leftover -
          output.length[segments - 1];
      if (output.length[segments - 1] < segment_minimum) {
        output.length[segments - 1] = 0.0;
        output.length[segments - 2] = rules.max_length;
        --segments;
      } else if (output.length[segments - 1] ==
                     output.length[segments - 2] &&
                 output.length[segments - 1] >
                     static_cast<int>(output.length[segments - 1] / 2.0) *
                         2.0) {
        output.length[segments - 1] -= 1.0;
        output.length[segments - 2] += 1.0;
      }
    } else if (rules.option == 23) {
      if (leftover >= segment_minimum) {
        output.length[segments - 1] = leftover;
      } else {
        output.length[segments - 1] = 0.0;
        --segments;
      }
    } else if (rules.option == 24) {
      if (leftover < rules.max_length * 0.25) {
        output.length[segments - 1] = 0.0;
        --segments;
      } else if (leftover <= rules.max_length * 0.75) {
        output.length[segments - 1] =
            static_cast<int>(rules.max_length * 0.5 + 0.5);
      } else {
        output.length[segments - 1] = rules.max_length;
      }
    }
  }
  output.count = segments;
  return output;
}

inline double segmented_height(const MerchRules& rules,
                               double merchantable_length,
                               double number_log_minimum,
                               double segment_minimum) {
  int segments = number_logs(
      rules, merchantable_length, number_log_minimum);
  if (segments == 0) {
    return 0.0;
  }
  double available = merchantable_length - segments * rules.trim;
  if (rules.evod == 1) {
    available = std::floor(available + 0.5);
  } else {
    available = std::floor((available + 1.0) / 2.0) * 2.0;
  }
  available = std::min(available, segments * rules.max_length);
  if (segments == 1) {
    if (rules.option == 24) {
      if (available < rules.max_length * 0.25) {
        available = 0.0;
      } else if (available <= rules.max_length * 0.75) {
        available = rules.max_length / 2.0;
      } else {
        available = rules.max_length;
      }
    } else if (available < segment_minimum) {
      available = 0.0;
    } else {
      available = std::min(available, rules.max_length);
    }
  } else if (rules.option >= 20) {
    double leftover = available - static_cast<int>(rules.max_length) *
        static_cast<double>(segments - 1);
    if (rules.option == 23 && leftover < segment_minimum) {
      available -= leftover;
      --segments;
    } else if (rules.option == 24 && leftover < rules.max_length * 0.25) {
      available -= leftover;
      --segments;
    } else if (rules.option == 24 && leftover <= rules.max_length * 0.75) {
      available += rules.max_length * 0.5 - leftover;
    }
    if (rules.option == 22) {
      const double top = std::floor((rules.max_length + leftover) / 2.0);
      if (top < segment_minimum) {
        available -= leftover;
        --segments;
      }
    }
  }
  return available + segments * rules.trim;
}

inline double scribner_volume(double diameter, double length,
                              bool corrected) {
  if (diameter < 1.0) {
    return 0.0;
  }
  diameter = std::min(diameter, 120.0);
  int factor_index = static_cast<int>(diameter);
  if (diameter > 5.0 && diameter <= 11.0) {
    if (length > 15.0 && length < 32.0) factor_index += 115;
    if (length > 31.0 && length < 41.0) factor_index += 121;
  }
  const double factor = nsvb_data::scribner_factor[factor_index - 1];
  if (!corrected) {
    return std::floor(length * factor + 0.5);
  }
  double decimal_c = std::floor((length * factor + 5.0) / 10.0);
  const double key = length * 1000.0 + diameter;
  std::size_t low = 0;
  std::size_t high = nsvb_data::countof(nsvb_data::scribner_exception);
  while (low < high) {
    const std::size_t middle = low + (high - low) / 2;
    const double candidate = std::floor(
        nsvb_data::scribner_exception[middle] / 10.0);
    if (candidate < key) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  if (low < nsvb_data::countof(nsvb_data::scribner_exception) &&
      std::floor(nsvb_data::scribner_exception[low] / 10.0) == key) {
    const double encoded = nsvb_data::scribner_exception[low];
    const bool add = encoded / 2.0 - static_cast<int>(encoded / 2.0) > 0.0;
    decimal_c += add ? 1.0 : -1.0;
  }
  return decimal_c * 10.0;
}

inline double international_quarter_volume(double diameter, double length) {
  if (diameter < 4.0) {
    return 0.0;
  }
  const int segments = static_cast<int>(length / 4.0);
  const double fraction = length / 4.0 - segments;
  double volume = 0.0;
  for (int segment = 1; segment <= segments; ++segment) {
    const double small_end = diameter + (segments - segment) / 2.0;
    volume += (0.22 * small_end * small_end - 0.71 * small_end) * 0.905;
  }
  if (fraction > 0.0) {
    volume += fraction * (0.22 * diameter * diameter - 0.71 * diameter) *
        0.905;
  }
  if (volume < 7.5) {
    return 5.0;
  }
  const int tens = static_cast<int>(volume / 10.0);
  const int remainder = static_cast<int>((volume / 10.0 - tens) * 100.0);
  if (remainder < 25) return tens * 10.0;
  if (remainder >= 75) return (tens + 1) * 10.0;
  return tens * 10.0 + 5.0;
}

inline void decay_values(bool hardwood, int decay_class, double* density,
                         double* carbon) {
  static const double soft_density[] = {1.0, 0.97, 1.0, 0.92, 0.55, 0.55};
  static const double soft_carbon[] = {0.0, 0.501, 0.504, 0.506, 0.520, 0.527};
  static const double hard_density[] = {1.0, 0.99, 0.8, 0.54, 0.43, 0.43};
  static const double hard_carbon[] = {0.0, 0.470, 0.473, 0.481, 0.480, 0.472};
  *density = hardwood ? hard_density[decay_class] : soft_density[decay_class];
  *carbon = hardwood ? hard_carbon[decay_class] : soft_carbon[decay_class];
}

inline void green_weight(int region, int forest, int spcd,
                         const nsvb_data::Species& reference,
                         double* live_weight, double* dead_weight) {
  double primary = 0.0;
  double secondary = 0.0;
  double dead = 0.0;
  bool found = false;
  if (region != 0) {
    for (const nsvb_data::RegionWeight& row : nsvb_data::region_weight) {
      if (static_cast<int>(row.region) == region &&
          ((static_cast<int>(row.forest) == forest &&
            static_cast<int>(row.spcd) == spcd) ||
           (static_cast<int>(row.forest) == 0 &&
            static_cast<int>(row.spcd) == spcd))) {
        found = true;
        primary = row.primary;
        secondary = row.secondary;
        dead = row.dead;
        break;
      }
    }
  }
  if (secondary == 0.0) {
    secondary = primary;
  }
  if (!found && primary < 1.0) {
    const nsvb_data::Species* default_reference = default_species();
    primary = reference.green == 0.0 && default_reference != nullptr
        ? default_reference->green : reference.green;
  }
  if (dead < 1.0) {
    double multiplier = 0.7036;
    if (region == 1) multiplier = 0.6749;
    if (region == 2) multiplier = 0.6381;
    if (region == 4) multiplier = 0.6113;
    if (region == 5) multiplier = 0.8254;
    if (region == 7) multiplier = 0.7951;
    dead = primary * multiplier;
  }
  *live_weight = primary;
  *dead_weight = dead;
}

inline BiomassResult biomass(const Equation& input_equation, double dbh,
                             double ht, int input_spcd,
                             const BiomassOptions& options) {
  BiomassResult output;
  Equation equation = input_equation;
  int spcd = input_spcd;
  if (spcd == 204) spcd = 202;
  if (spcd == 2042) spcd = 42;
  if (spcd == 2098) spcd = 98;
  if (spcd == 2242) spcd = 242;
  if (spcd == 2263) spcd = 263;
  equation.species = spcd;
  const nsvb_data::Species* reference = find_species(spcd);
  if (reference == nullptr) {
    output.status = 306;
    return output;
  }
  if (dbh < 1.0) {
    output.status = 303;
    return output;
  }
  if (ht < 5.0) {
    output.status = 304;
    return output;
  }

  const Result total_inside = volume_weight(equation, 1, dbh, ht, *reference);
  const Result total_bark = volume_weight(equation, 2, dbh, ht, *reference);
  const Result taper_outside = volume_weight(equation, 3, dbh, ht, *reference);
  const Result bark_weight = volume_weight(equation, 6, dbh, ht, *reference);
  const Result branch_weight = volume_weight(equation, 7, dbh, ht, *reference);
  const Result agb_weight = volume_weight(equation, 8, dbh, ht, *reference);
  const Result foliage_weight = volume_weight(equation, 9, dbh, ht, *reference);
  const Result results[] = {total_inside, total_bark, taper_outside,
                            bark_weight, branch_weight, agb_weight,
                            foliage_weight};
  for (const Result& result : results) {
    if (result.status != 0) {
      output.status = result.status;
      return output;
    }
  }
  if (!(total_inside.value > 0.0)) {
    output.status = 301;
    return output;
  }

  const bool dead = options.decay_class > 0;
  const int decay_class = dead ? options.decay_class : 0;
  double density_proportion = 1.0;
  double dead_carbon = 0.0;
  double bark_remaining = 1.0;
  double branch_remaining = 1.0;
  if (dead) {
    decay_values(reference->hardwood != 0.0, decay_class,
                 &density_proportion, &dead_carbon);
    static const double bark_values[] = {1.0, 1.0, 0.8, 0.5, 0.2, 0.0};
    static const double branch_values[] = {1.0, 1.0, 0.5, 0.1, 0.0, 0.0};
    bark_remaining = bark_values[decay_class];
    branch_remaining = branch_values[decay_class];
  }
  const double cull_fraction = options.cull / 100.0;
  const nsvb_data::Species* default_reference = default_species();
  const double wood_density =
      reference->wood_dry == 0.0 && default_reference != nullptr
      ? default_reference->wood_dry : reference->wood_dry;
  const double dry_weight =
      reference->dry == 0.0 && default_reference != nullptr
      ? default_reference->dry : reference->dry;
  const double inside_sound = total_inside.value * (1.0 - cull_fraction);
  const double bark_sound = total_bark.value;
  const double inside_outside = std::sqrt(
      total_inside.value / (total_inside.value + total_bark.value));

  double ratio_a = 0.0;
  double ratio_b = 0.0;
  ratio_coefficients(equation, 5, *reference, &ratio_a, &ratio_b);
  const double stump_ratio = ratio(ht, options.stump, ratio_a, ratio_b);
  const double stump_inside = total_inside.value * stump_ratio;
  const double stump_outside = (total_inside.value + total_bark.value) *
      stump_ratio;
  const double stump_bark = stump_outside - stump_inside;
  const double stump_inside_sound = stump_inside * (1.0 - cull_fraction);

  double outside_a = 0.0;
  double outside_b = 0.0;
  ratio_coefficients(equation, 4, *reference, &outside_a, &outside_b);
  double primary_height = 0.0;
  if (options.primary_top < dbh) {
    const double outside_top = options.ctype == 'I' ? options.primary_top :
        options.primary_top / inside_outside;
    primary_height = height_at_diameter(
        taper_outside.value, outside_a, outside_b, ht, outside_top);
    if (primary_height < 5.0 && outside_top < dbh) primary_height = 5.0;
  }
  if (primary_height < options.stump) primary_height = options.stump;

  const MerchRules rules = merch_rules(options.region, options);
  double saw_height = options.stump;
  Segments primary_segments;
  const double primary_length = std::max(0.0, primary_height - options.stump);
  if (primary_length >= rules.minimum_merch_length) {
    primary_segments = segment_logs(
        rules, primary_length, rules.min_length, rules.min_length);
    saw_height += primary_segments.occupied_height(rules.trim);
  }
  const double saw_ratio = ratio(ht, saw_height, ratio_a, ratio_b);
  const double saw_inside = total_inside.value * saw_ratio - stump_inside;
  const double saw_outside = (total_inside.value + total_bark.value) *
      saw_ratio - stump_outside;
  const double saw_bark = saw_outside - saw_inside;

  double secondary_height = 0.0;
  if (options.secondary_top < dbh) {
    const double outside_top = options.ctype == 'I' ? options.secondary_top :
        options.secondary_top / inside_outside;
    secondary_height = height_at_diameter(
        taper_outside.value, outside_a, outside_b, ht, outside_top);
    if (secondary_height < 5.0 && outside_top < dbh) secondary_height = 5.0;
  }
  secondary_height = std::max(secondary_height, saw_height);
  double merch_height = saw_height;
  Segments secondary_segments;
  const double secondary_length = secondary_height - saw_height;
  if (secondary_length >= rules.minimum_top_length) {
    secondary_segments = segment_logs(
        rules, secondary_length, rules.min_length,
        rules.minimum_top_length);
    merch_height += secondary_segments.occupied_height(rules.trim);
  }
  const double merch_ratio = ratio(ht, merch_height, ratio_a, ratio_b);
  const double merch_inside = total_inside.value * merch_ratio - stump_inside;
  const double merch_outside = (total_inside.value + total_bark.value) *
      merch_ratio - stump_outside;
  const double topwood_inside = merch_inside - saw_inside;
  const double topwood_bark = (merch_outside - saw_outside) - topwood_inside;
  const double tip_inside = total_inside.value * (1.0 - merch_ratio);
  const double tip_outside = (total_inside.value + total_bark.value) *
      (1.0 - merch_ratio);
  const double tip_bark = tip_outside - tip_inside;

  std::fill(output.volume, output.volume + 15, 0.0);
  output.n_logs_primary = primary_segments.count;
  output.n_logs_secondary = secondary_segments.count;
  output.volume[0] = inside_sound;
  output.volume[13] = stump_inside_sound;
  output.volume[14] = std::max(0.0, tip_inside * (1.0 - cull_fraction));

  double height_at_log_end = options.stump;
  const double butt_outside = diameter_at_height(
      taper_outside.value, outside_a, outside_b, ht, 4.5);
  double large_end = std::floor(butt_outside * inside_outside + 0.5);
  double raw_log_total = 0.0;
  double primary_rounded_cubic = 0.0;
  double secondary_rounded_cubic = 0.0;
  auto add_log = [&](double log_length, bool primary,
                     bool primary_board_slots) {
    height_at_log_end += rules.trim + log_length;
    const double outside_diameter = diameter_at_height(
        taper_outside.value, outside_a, outside_b, ht, height_at_log_end);
    const double inside_diameter = std::floor(
        outside_diameter * inside_outside + 0.5);
    const double cubic = 0.00272708 *
        (large_end * large_end + inside_diameter * inside_diameter) *
        log_length;
    raw_log_total += cubic;
    const double rounded_cubic = std::floor(cubic * 10.0 + 0.5) / 10.0;
    const double scribner = scribner_volume(
        inside_diameter, log_length, rules.corrected_scribner);
    const double international = international_quarter_volume(
        inside_diameter, log_length);
    if (primary) {
      primary_rounded_cubic += rounded_cubic;
    } else {
      secondary_rounded_cubic += rounded_cubic;
    }
    if (primary_board_slots) {
      output.volume[1] += scribner;
      output.volume[9] += international;
    } else {
      output.volume[11] += scribner;
    }
    large_end = inside_diameter;
  };
  for (int index = 0; index < primary_segments.count; ++index) {
    add_log(primary_segments.length[index], true, true);
  }
  for (int index = 0; index < secondary_segments.count; ++index) {
    add_log(secondary_segments.length[index], false,
            primary_segments.count == 0);
  }
  const double merchantable_inside =
      total_inside.value * merch_ratio - stump_inside;
  const double volume_factor = raw_log_total > 0.0 &&
      merchantable_inside > 0.0 ? merchantable_inside / raw_log_total : 1.0;
  output.volume[3] = std::max(0.0, primary_rounded_cubic * volume_factor);
  output.volume[6] = std::max(0.0, secondary_rounded_cubic * volume_factor);
  if (options.ctype == 'I' || options.ctype == 'B') {
    const double primary_ratio = ratio(ht, primary_height, ratio_a, ratio_b);
    const double secondary_ratio = ratio(ht, secondary_height, ratio_a, ratio_b);
    const double geometric_saw = total_inside.value * primary_ratio -
        stump_inside;
    const double geometric_merchant = total_inside.value * secondary_ratio -
        stump_inside;
    output.volume[3] = std::max(
        0.0, geometric_saw * (1.0 - cull_fraction));
    output.volume[6] = std::max(
        0.0, (geometric_merchant - geometric_saw) *
            (1.0 - cull_fraction));
    if (options.ctype == 'I') {
      output.volume[14] = std::max(
          0.0, total_inside.value * (1.0 - secondary_ratio) *
              (1.0 - cull_fraction));
    }
  }
  output.volume[1] *= 1.0 - cull_fraction;
  output.volume[9] *= 1.0 - cull_fraction;
  output.volume[5] = output.volume[3] /
      ((options.region == 3 || options.region == 8 || options.region == 9)
           ? 79.0 : 90.0);
  output.volume[5] = std::floor(output.volume[5] * 1000.0 + 0.5) / 1000.0;

  const double cull_density = reference->hardwood == 0.0 ? 0.92 : 0.54;
  const double weight_cull = dead ? 0.0 : cull_fraction;
  const double raw_wood = total_inside.value * wood_density;
  const double reduced_wood = raw_wood *
      (1.0 - weight_cull * (1.0 - cull_density)) * density_proportion;
  const double reduced_bark = bark_weight.value * density_proportion *
      bark_remaining;
  const double reduced_branch = branch_weight.value * density_proportion *
      branch_remaining;
  const double component_sum = reduced_wood + reduced_bark + reduced_branch;
  const double raw_component_sum = raw_wood + bark_weight.value +
      branch_weight.value;
  const double predicted_reduced = agb_weight.value *
      component_sum / raw_component_sum;
  const double difference = predicted_reduced - component_sum;
  const double harmonized_wood = reduced_wood + difference *
      reduced_wood / component_sum;
  const double harmonized_bark = reduced_bark + difference *
      reduced_bark / component_sum;
  const double harmonized_branch = reduced_branch + difference *
      reduced_branch / component_sum;
  const double foliage = dead ? 0.0 : foliage_weight.value;

  const double adjusted_wood_density = harmonized_wood / inside_sound;
  const double adjusted_bark_density = harmonized_bark / bark_sound;
  const double saw_wood_weight = std::max(0.0,
      saw_inside * (1.0 - cull_fraction) * adjusted_wood_density);
  const double saw_bark_weight = saw_bark * adjusted_bark_density;
  const double topwood_wood_weight = std::max(0.0,
      topwood_inside * (1.0 - cull_fraction) * adjusted_wood_density);
  const double topwood_bark_weight = std::max(0.0,
      topwood_bark * adjusted_bark_density);
  const double tip_wood_weight = std::max(0.0,
      tip_inside * (1.0 - cull_fraction) * adjusted_wood_density);
  const double tip_bark_weight = std::max(0.0,
      tip_bark * adjusted_bark_density);
  const double stump_wood_weight = stump_inside_sound * adjusted_wood_density;
  const double stump_bark_weight = stump_bark * adjusted_bark_density;

  output.value[0] = predicted_reduced;
  output.value[1] = harmonized_wood;
  output.value[2] = harmonized_bark;
  output.value[3] = stump_wood_weight;
  output.value[4] = stump_bark_weight;
  output.value[5] = saw_wood_weight;
  output.value[6] = saw_bark_weight;
  output.value[7] = topwood_wood_weight;
  output.value[8] = topwood_bark_weight;
  output.value[9] = tip_wood_weight;
  output.value[10] = tip_bark_weight;
  output.value[11] = harmonized_branch;
  output.value[12] = foliage;
  output.value[13] = predicted_reduced - stump_wood_weight - stump_bark_weight -
      saw_wood_weight - saw_bark_weight - topwood_wood_weight -
      topwood_bark_weight;
  const double carbon_fraction = dead ? dead_carbon :
      std::floor(reference->carbon * 1000.0 + 0.5) / 1000.0;
  output.value[14] = output.value[0] * carbon_fraction;
  output.value[15] = output.value[14] * 44.0 / 12.0;

  double live_green = 0.0;
  double dead_green = 0.0;
  green_weight(options.region, options.forest, spcd, *reference,
               &live_green, &dead_green);
  double dry_factor = (harmonized_wood + harmonized_bark) / total_inside.value;
  if (dry_factor < 10.0) dry_factor = dry_weight;
  const double moisture = dead ? (dead_green - dry_factor) / dry_factor :
      (live_green - dry_factor) / dry_factor;
  for (int index = 0; index < 14; ++index) {
    output.value[16 + index] = output.value[index] * (1.0 + moisture);
  }
  for (double value : output.value) {
    if (!std::isfinite(value)) {
      output.status = 54;
      return output;
    }
  }
  for (double value : output.volume) {
    if (!std::isfinite(value)) {
      output.status = 54;
      return output;
    }
  }
  return output;
}

inline double carbon_fraction(int spcd, bool raw = false) {
  std::size_t low = 0;
  std::size_t high = nsvb_data::countof(nsvb_data::carbon);
  while (low < high) {
    const std::size_t middle = low + (high - low) / 2;
    const int candidate = static_cast<int>(nsvb_data::carbon[middle].key);
    if (candidate < spcd) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  if (low >= nsvb_data::countof(nsvb_data::carbon) ||
      static_cast<int>(nsvb_data::carbon[low].key) != spcd) {
    low = nsvb_data::countof(nsvb_data::carbon) - 1;
  }
  const double raw_fraction = nsvb_data::carbon[low].value / 100.0;
  if (raw) return raw_fraction;
  return std::floor(raw_fraction * 1000.0 + 0.5) / 1000.0;
}

}  // namespace nsvb
}  // namespace treevolume

#endif
