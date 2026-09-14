#ifndef TREEVOLUME_KERNELS_HPP
#define TREEVOLUME_KERNELS_HPP

#include <cmath>
#include <limits>
#include <string>

#include <treevolume/clark.hpp>
#include <treevolume/flewelling.hpp>
#include <treevolume/nsvb.hpp>
#include <treevolume/published_tapers.hpp>
#include <treevolume/r10r4.hpp>
#include <treevolume/smalltapers.hpp>

namespace treevolume {

using KernelFunction = double (*)(double, double, double, double, double);

struct Kernel {
  const char* name;
  KernelFunction dib;
  KernelFunction dob;
  KernelFunction height_at_dib;
  KernelFunction volume;
};

inline double missing_value() {
  return std::numeric_limits<double>::quiet_NaN();
}

inline double demo_paraboloid_dib(double dbh, double ht, double h,
                                  double unused, double bark_ratio) {
  (void) unused;
  if (!(dbh > 0.0) || !(ht > 4.5) || h < 0.0 || h > ht ||
      !(bark_ratio > 0.0)) {
    return missing_value();
  }
  return dbh * bark_ratio * std::sqrt((ht - h) / (ht - 4.5));
}

inline double demo_paraboloid_height(double dbh, double ht, double diameter,
                                     double unused, double bark_ratio) {
  (void) unused;
  if (!(dbh > 0.0) || !(ht > 4.5) || !(diameter >= 0.0) ||
      !(bark_ratio > 0.0)) {
    return missing_value();
  }
  const double ratio = diameter / (dbh * bark_ratio);
  return ht - (ht - 4.5) * ratio * ratio;
}

inline double demo_paraboloid_dob(double dbh, double ht, double h,
                                  double unused, double bark_ratio) {
  const double inside = demo_paraboloid_dib(dbh, ht, h, unused, bark_ratio);
  return std::isfinite(inside) ? inside / bark_ratio : missing_value();
}

inline double demo_paraboloid_volume(double dbh, double ht, double lower,
                                     double upper, double bark_ratio) {
  if (!(dbh > 0.0) || !(ht > 4.5) || lower < 0.0 || upper > ht ||
      !(lower < upper) || !(bark_ratio > 0.0)) {
    return missing_value();
  }
  const double squared_diameter = dbh * dbh * bark_ratio * bark_ratio;
  const double area_scale = 3.14159265358979323846 * squared_diameter / 576.0;
  const double antiderivative =
      ht * (upper - lower) - (upper * upper - lower * lower) / 2.0;
  return area_scale * antiderivative / (ht - 4.5);
}

inline const Kernel* find_kernel(const std::string& name) {
  static const Kernel kernels[] = {
      {"demo_paraboloid", demo_paraboloid_dib,
       demo_paraboloid_dob, demo_paraboloid_height, demo_paraboloid_volume}
  };
  for (const Kernel& kernel : kernels) {
    if (name == kernel.name) {
      return &kernel;
    }
  }
  return nullptr;
}

inline bool kernel_exists(const std::string& name) {
  if (find_kernel(name) != nullptr) {
    return true;
  }
  static const std::string flewelling_prefix = "flewelling:";
  if (name.compare(0, flewelling_prefix.size(), flewelling_prefix) == 0) {
    flewelling::Equation equation;
    return flewelling::parse_equation(
        name.substr(flewelling_prefix.size()), &equation);
  }
  static const std::string nsvb_prefix = "nsvb:";
  if (name.compare(0, nsvb_prefix.size(), nsvb_prefix) == 0) {
    nsvb::Equation nsvb_equation;
    return nsvb::parse_equation(name.substr(nsvb_prefix.size()),
                                &nsvb_equation);
  }
  static const std::string clark_prefix = "clark:";
  if (name.compare(0, clark_prefix.size(), clark_prefix) == 0) {
    clark::Equation equation;
    return clark::parse_equation(name.substr(clark_prefix.size()), &equation);
  }
  static const std::string published_prefix = "published:";
  if (name.compare(0, published_prefix.size(), published_prefix) == 0) {
    return published_tapers::parse_form(
        name.substr(published_prefix.size())) != published_tapers::Form::unknown;
  }
  static const std::string small_prefix = "smalltapers:";
  if (name.compare(0, small_prefix.size(), small_prefix) == 0) {
    smalltapers::Equation equation;
    return smalltapers::parse_equation(
        name.substr(small_prefix.size()), &equation);
  }
  r10r4::Equation regional_equation;
  return r10r4::parse_equation(name, &regional_equation);
}

}  // namespace treevolume

#endif
