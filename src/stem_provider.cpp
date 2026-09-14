#include <Rcpp.h>
#include <RcppParallel.h>

#include <treevolume/kernels.hpp>
#include "stem_provider.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace api = treevolume::stem_provider;

namespace {

constexpr const char* kNvelRevision =
    "38548071d5aa652bb90c7f111f86b427f798a1c9";
constexpr double kImperialAreaDivisor = 576.0;
constexpr double kMetricAreaDivisor = 40000.0;

struct ScalarResult {
  double value = std::numeric_limits<double>::quiet_NaN();
  std::int32_t status = api::status_kernel_error;
};

enum class ModelKind {
  r_kernel,
  generic,
  published,
  flewelling,
  clark,
  regional,
  small_taper,
  nsvb
};

struct ModelInfo {
  std::string id;
  std::string key;
  ModelKind kind = ModelKind::r_kernel;
  std::uint32_t units = api::units_imperial;
  std::uint32_t verification = api::verification_not_applicable;
  bool has_dob = false;
  bool has_inverse = false;
  bool has_integral = false;
  double stump_height = 1.0;
  double bark_ratio = std::numeric_limits<double>::quiet_NaN();
  std::vector<int> species;
  std::unordered_set<std::string> required;
  std::unordered_set<std::string> optional;
  std::vector<std::vector<std::string>> pairs;
  std::vector<double> coefficients;
  const treevolume::Kernel* generic = nullptr;
  treevolume::published_tapers::Form published =
      treevolume::published_tapers::Form::unknown;
  treevolume::flewelling::Equation flewelling;
  treevolume::clark::Equation clark;
  treevolume::r10r4::Equation regional{
      treevolume::r10r4::Family::r10, std::string(), 0, -1, false};
  treevolume::smalltapers::Equation small_taper;
  treevolume::nsvb::Equation nsvb;

  bool declares(const std::string& name) const {
    return required.find(name) != required.end() ||
        optional.find(name) != optional.end();
  }
};

void clear_error(api::ErrorBuffer* error) noexcept {
  if (error == nullptr) {
    return;
  }
  error->used = 0;
  if (error->data != nullptr && error->capacity > 0) {
    error->data[0] = '\0';
  }
}

int fail(int code, api::ErrorBuffer* error, const std::string& message) noexcept {
  if (error != nullptr) {
    error->used = message.size();
    if (error->data != nullptr && error->capacity > 0) {
      const std::size_t count = std::min(error->capacity - 1, message.size());
      std::memcpy(error->data, message.data(), count);
      error->data[count] = '\0';
    }
  }
  return code;
}

bool starts_with(const std::string& value, const char* prefix) {
  const std::size_t size = std::strlen(prefix);
  return value.size() >= size && value.compare(0, size, prefix) == 0;
}

void configure_model(ModelInfo* model) {
  if (model->kind == ModelKind::r_kernel) {
    return;
  }
  if (starts_with(model->key, "published:")) {
    model->kind = ModelKind::published;
    model->published = treevolume::published_tapers::parse_form(
        model->key.substr(std::strlen("published:")));
    if (!treevolume::published_tapers::finite_coefficients(
            model->published, model->coefficients.data(),
            model->coefficients.size())) {
      throw std::runtime_error("published taper coefficients are invalid");
    }
    return;
  }
  if (starts_with(model->key, "flewelling:")) {
    model->kind = ModelKind::flewelling;
    if (!treevolume::flewelling::parse_equation(
            model->key.substr(std::strlen("flewelling:")),
            &model->flewelling)) {
      throw std::runtime_error("unknown Flewelling kernel");
    }
    return;
  }
  if (starts_with(model->key, "clark:")) {
    model->kind = ModelKind::clark;
    if (!treevolume::clark::parse_equation(
            model->key.substr(std::strlen("clark:")), &model->clark)) {
      throw std::runtime_error("unknown Clark kernel");
    }
    return;
  }
  if (starts_with(model->key, "r10:") ||
      starts_with(model->key, "r4mat:")) {
    model->kind = ModelKind::regional;
    if (!treevolume::r10r4::parse_equation(model->key, &model->regional)) {
      throw std::runtime_error("unknown regional kernel");
    }
    return;
  }
  if (starts_with(model->key, "smalltapers:")) {
    model->kind = ModelKind::small_taper;
    if (!treevolume::smalltapers::parse_equation(
            model->key.substr(std::strlen("smalltapers:")),
            &model->small_taper)) {
      throw std::runtime_error("unknown small-taper kernel");
    }
    return;
  }
  if (starts_with(model->key, "nsvb:")) {
    model->kind = ModelKind::nsvb;
    if (!treevolume::nsvb::parse_equation(
            model->key.substr(std::strlen("nsvb:")), &model->nsvb)) {
      throw std::runtime_error("unknown NSVB kernel");
    }
    return;
  }
  model->generic = treevolume::find_kernel(model->key);
  if (model->generic == nullptr) {
    throw std::runtime_error("unknown compiled kernel");
  }
  model->kind = ModelKind::generic;
}

std::string scalar_string(SEXP value) {
  if (TYPEOF(value) != STRSXP || Rf_xlength(value) != 1 ||
      STRING_ELT(value, 0) == NA_STRING) {
    return std::string();
  }
  return Rcpp::as<std::string>(value);
}

std::shared_ptr<ModelInfo> copy_model(const Rcpp::List& model) {
  auto copied = std::make_shared<ModelInfo>();
  copied->id = Rcpp::as<std::string>(model["id"]);
  const Rcpp::List kernel = model["kernel"];
  const std::string type = Rcpp::as<std::string>(kernel["type"]);
  copied->kind = type == "compiled" ? ModelKind::generic
                                     : ModelKind::r_kernel;
  copied->key = scalar_string(kernel["key"]);
  copied->has_dob = Rcpp::as<bool>(kernel["has_dob"]);
  copied->has_inverse = Rcpp::as<bool>(kernel["has_inverse"]);
  copied->has_integral = Rcpp::as<bool>(kernel["has_integral"]);
  copied->units = Rcpp::as<std::string>(model["units"]) == "metric"
      ? api::units_metric : api::units_imperial;
  copied->stump_height = Rcpp::as<double>(model["stump_ht"]);
  copied->bark_ratio = Rcpp::as<double>(model["bark_ratio"]);

  const Rcpp::RObject verification = model.attr("oracle_verified");
  if (!verification.isNULL()) {
    const Rcpp::LogicalVector values(verification);
    if (values.size() == 1 && values[0] != NA_LOGICAL) {
      copied->verification = values[0] == TRUE
          ? api::verification_true : api::verification_false;
    }
  }

  const Rcpp::IntegerVector species = model["species"];
  copied->species.assign(species.begin(), species.end());
  const Rcpp::List inputs = model["inputs"];
  const Rcpp::CharacterVector required = inputs["required"];
  const Rcpp::CharacterVector optional = inputs["optional"];
  for (R_xlen_t index = 0; index < required.size(); ++index) {
    copied->required.insert(Rcpp::as<std::string>(required[index]));
  }
  for (R_xlen_t index = 0; index < optional.size(); ++index) {
    copied->optional.insert(Rcpp::as<std::string>(optional[index]));
  }
  const Rcpp::List pairs = inputs["pairs"];
  for (R_xlen_t index = 0; index < pairs.size(); ++index) {
    const Rcpp::CharacterVector pair = pairs[index];
    std::vector<std::string> names;
    names.reserve(static_cast<std::size_t>(pair.size()));
    for (R_xlen_t pair_index = 0; pair_index < pair.size(); ++pair_index) {
      names.push_back(Rcpp::as<std::string>(pair[pair_index]));
    }
    copied->pairs.push_back(std::move(names));
  }
  const SEXP data = model["data"];
  if (Rf_isNumeric(data) && Rf_xlength(data) > 0) {
    copied->coefficients = Rcpp::as<std::vector<double>>(data);
  }
  configure_model(copied.get());
  return copied;
}

api::ModelHandle handle_for_slot(const ModelInfo& model, std::size_t slot) {
  const std::uint32_t raw = static_cast<std::uint32_t>(slot + 1);
  return model.kind == ModelKind::r_kernel
      ? raw | api::R_KERNEL_HANDLE_MASK : raw;
}

bool successful(std::int32_t status) {
  return status == api::status_ok ||
      status == api::status_species_out_of_scope ||
      status == api::status_not_unique;
}

void merge_status(std::int32_t* current, std::int32_t returned) {
  const bool model_before_numeric = *current >= 100 && *current < 150 &&
      returned >= 50 && returned < 100 &&
      returned != api::status_species_out_of_scope;
  if (returned != api::status_ok &&
      (*current == api::status_ok || model_before_numeric ||
       *current == api::status_species_out_of_scope)) {
    *current = returned;
  }
}

template <typename Diameter>
ScalarResult gauss_legendre_volume(double lower, double upper,
                                   Diameter diameter, double segment_size,
                                   double area_divisor) {
  static const double nodes[] = {
      -0.9061798459386640, -0.5384693101056831, 0.0,
      0.5384693101056831, 0.9061798459386640};
  static const double weights[] = {
      0.2369268850561891, 0.4786286704993665, 0.5688888888888889,
      0.4786286704993665, 0.2369268850561891};
  if (lower == upper) {
    return ScalarResult{0.0, api::status_ok};
  }
  if (!(lower < upper)) {
    return ScalarResult{};
  }
  double total = 0.0;
  for (double left = lower; left < upper;) {
    const double right = std::min(left + segment_size, upper);
    const double midpoint = (left + right) / 2.0;
    const double half_width = (right - left) / 2.0;
    for (int node = 0; node < 5; ++node) {
      const ScalarResult evaluated = diameter(
          midpoint + half_width * nodes[node]);
      if (evaluated.status != api::status_ok) {
        return evaluated;
      }
      total += 3.14159265358979323846 * evaluated.value * evaluated.value /
          area_divisor * half_width * weights[node];
    }
    left = right;
  }
  return std::isfinite(total)
      ? ScalarResult{total, api::status_ok} : ScalarResult{};
}

struct AuxAccessor {
  const api::AuxColumn* form_class = nullptr;
  const api::AuxColumn* upper_ht1 = nullptr;
  const api::AuxColumn* upper_d1 = nullptr;
  const api::AuxColumn* upper_ht2 = nullptr;
  const api::AuxColumn* upper_d2 = nullptr;
  const api::AuxColumn* site_index = nullptr;
  const api::AuxColumn* basal_area = nullptr;
  const api::AuxColumn* bark_ratio = nullptr;
  const api::AuxColumn* decay_class = nullptr;
  const api::AuxColumn* cull = nullptr;
  const api::AuxColumn* upper_bark = nullptr;
  const api::AuxColumn* spcd = nullptr;

  const api::AuxColumn* get(const std::string& name) const {
    if (name == "form_class") return form_class;
    if (name == "upper_ht1") return upper_ht1;
    if (name == "upper_d1") return upper_d1;
    if (name == "upper_ht2") return upper_ht2;
    if (name == "upper_d2") return upper_d2;
    if (name == "site_index") return site_index;
    if (name == "basal_area") return basal_area;
    if (name == "bark_ratio") return bark_ratio;
    if (name == "decay_class") return decay_class;
    if (name == "cull") return cull;
    if (name == "upper_bark") return upper_bark;
    if (name == "spcd") return spcd;
    return nullptr;
  }
};

template <typename Value>
Value aux_read(const api::AuxColumn* column, std::size_t index) {
  const char* base = static_cast<const char*>(column->data);
  const std::size_t offset = column->stride == 0 ? 0 : index * column->stride;
  Value value;
  std::memcpy(&value, base + offset, sizeof(Value));
  return value;
}

double aux_double_value(const api::AuxColumn* column, std::size_t index,
                        double fallback) {
  return column == nullptr ? fallback : aux_read<double>(column, index);
}

int prepare_aux(api::AuxView view, AuxAccessor* out,
                api::ErrorBuffer* error) {
  if (view.size > 0 && view.columns == nullptr) {
    return fail(api::call_invalid_argument, error,
                "aux.columns is null for a nonempty view");
  }
  std::unordered_set<std::string> seen;
  for (std::size_t index = 0; index < view.size; ++index) {
    const api::AuxColumn& column = view.columns[index];
    if (column.name == nullptr || column.name[0] == '\0' ||
        column.data == nullptr) {
      return fail(api::call_invalid_argument, error,
                  "an auxiliary column has a missing name or data pointer");
    }
    const std::string name(column.name);
    if (!seen.insert(name).second) {
      return fail(api::call_invalid_argument, error,
                  "auxiliary column names must be unique");
    }
    const bool uint8_column = name == "upper_bark";
    const bool int32_column = name == "spcd";
    const bool known = uint8_column || int32_column ||
        name == "form_class" || name == "upper_ht1" ||
        name == "upper_d1" || name == "upper_ht2" ||
        name == "upper_d2" || name == "site_index" ||
        name == "basal_area" || name == "bark_ratio" ||
        name == "decay_class" || name == "cull";
    if (!known) {
      return fail(api::call_invalid_argument, error,
                  "unknown auxiliary column: " + name);
    }
    const std::uint32_t expected = uint8_column ? api::aux_uint8
        : int32_column ? api::aux_int32 : api::aux_double;
    const std::size_t width = uint8_column ? sizeof(std::uint8_t)
        : int32_column ? sizeof(std::int32_t) : sizeof(double);
    if (column.type != expected ||
        (column.stride != 0 && column.stride < width)) {
      return fail(api::call_invalid_argument, error,
                  "auxiliary column has an invalid type or stride: " + name);
    }
    if (name == "form_class") out->form_class = &column;
    else if (name == "upper_ht1") out->upper_ht1 = &column;
    else if (name == "upper_d1") out->upper_d1 = &column;
    else if (name == "upper_ht2") out->upper_ht2 = &column;
    else if (name == "upper_d2") out->upper_d2 = &column;
    else if (name == "site_index") out->site_index = &column;
    else if (name == "basal_area") out->basal_area = &column;
    else if (name == "bark_ratio") out->bark_ratio = &column;
    else if (name == "decay_class") out->decay_class = &column;
    else if (name == "cull") out->cull = &column;
    else if (name == "upper_bark") out->upper_bark = &column;
    else if (name == "spcd") out->spcd = &column;
  }
  return api::call_ok;
}

class Evaluator {
 public:
  Evaluator(const ModelInfo& model, double dbh, double ht,
            const AuxAccessor& aux, std::size_t tree, bool nvel_compat)
      : model_(model), dbh_(dbh), ht_(ht), aux_(aux), tree_(tree),
        nvel_compat_(nvel_compat) {}

  ScalarResult diameter(bool outside, double height) {
    switch (model_.kind) {
      case ModelKind::r_kernel:
        return ScalarResult{
            std::numeric_limits<double>::quiet_NaN(),
            api::status_capability_missing};
      case ModelKind::generic: {
        const double bark = bark_ratio();
        const double value = outside
            ? model_.generic->dob(dbh_, ht_, height, 0.0, bark)
            : model_.generic->dib(dbh_, ht_, height, 0.0, bark);
        return finite_result(value);
      }
      case ModelKind::published: {
        double value = treevolume::published_tapers::inside_diameter(
            model_.published, model_.coefficients.data(),
            model_.coefficients.size(), dbh_, ht_, height);
        if (outside) {
          const double bark = bark_ratio();
          if (!valid_bark(bark)) return missing_bark();
          value /= bark;
        }
        return finite_result(value);
      }
      case ModelKind::flewelling: {
        const ScalarResult initialized = ensure_flewelling();
        if (initialized.status != api::status_ok) return initialized;
        const double value = outside
            ? treevolume::flewelling::outside_diameter(
                  flewelling_profile_, height)
            : treevolume::flewelling::inside_diameter(
                  flewelling_profile_, height);
        return finite_result(value);
      }
      case ModelKind::clark: {
        const ScalarResult initialized = ensure_clark();
        if (initialized.status != api::status_ok) return initialized;
        double value = treevolume::clark::inside_diameter(
            model_.clark, clark_profile_, height);
        if (outside) {
          const double bark = bark_ratio();
          if (!valid_bark(bark)) return missing_bark();
          value /= bark;
        }
        return finite_result(value);
      }
      case ModelKind::regional: {
        const treevolume::r10r4::Result result =
            treevolume::r10r4::diameter(model_.regional, dbh_, ht_, height);
        if (result.status != api::status_ok) {
          return ScalarResult{result.value, result.status};
        }
        if (!outside) return ScalarResult{result.value, result.status};
        const double bark = bark_ratio();
        if (!valid_bark(bark)) return missing_bark();
        return ScalarResult{result.value / bark, result.status};
      }
      case ModelKind::small_taper: {
        const treevolume::smalltapers::Result result =
            treevolume::smalltapers::diameter(
                model_.small_taper, dbh_, ht_, height,
                small_taper_auxiliary());
        if (result.status != api::status_ok) {
          return ScalarResult{result.value, result.status};
        }
        if (!outside) return ScalarResult{result.value, result.status};
        const double bark = bark_ratio();
        if (!valid_bark(bark)) return missing_bark();
        return ScalarResult{result.value / bark, result.status};
      }
      case ModelKind::nsvb: {
        if (outside && nvel_compat_) {
          return ScalarResult{0.0, api::status_ok};
        }
        const treevolume::nsvb::Result result =
            treevolume::nsvb::profile_value(
                model_.nsvb, dbh_, ht_, height, outside);
        return ScalarResult{result.value, result.status};
      }
    }
    return ScalarResult{};
  }

  ScalarResult volume(bool outside, double lower, double upper) {
    if (lower == upper) {
      return ScalarResult{0.0, api::status_ok};
    }
    const double segment = model_.units == api::units_metric ? 0.3048 : 1.0;
    const double area = model_.units == api::units_metric
        ? kMetricAreaDivisor : kImperialAreaDivisor;
    switch (model_.kind) {
      case ModelKind::r_kernel:
        return ScalarResult{
            std::numeric_limits<double>::quiet_NaN(),
            api::status_capability_missing};
      case ModelKind::generic: {
        const double bark = bark_ratio();
        double value = model_.generic->volume(
            dbh_, ht_, lower, upper, bark);
        if (outside) {
          if (!valid_bark(bark)) return missing_bark();
          value /= bark * bark;
        }
        return finite_result(value);
      }
      case ModelKind::published: {
        ScalarResult result;
        if (model_.published ==
            treevolume::published_tapers::Form::max_burkhart) {
          result = finite_result(
              treevolume::published_tapers::max_burkhart_volume(
                  model_.coefficients.data(), dbh_, ht_, lower, upper));
        } else {
          result = gauss_legendre_volume(
              lower, upper,
              [this](double height) { return diameter(false, height); },
              segment, area);
        }
        if (outside && result.status == api::status_ok) {
          const double bark = bark_ratio();
          if (!valid_bark(bark)) return missing_bark();
          result.value /= bark * bark;
        }
        return result;
      }
      case ModelKind::flewelling: {
        if (!outside) {
          const treevolume::flewelling::Result result =
              treevolume::flewelling::smalian_volume(
                  model_.flewelling, dbh_, ht_, lower, upper,
                  flewelling_auxiliary());
          return ScalarResult{result.value, result.status};
        }
        const ScalarResult initialized = ensure_flewelling();
        if (initialized.status != api::status_ok) return initialized;
        return gauss_legendre_volume(
            lower, upper,
            [this](double height) {
              return finite_result(
                  treevolume::flewelling::outside_diameter(
                      flewelling_profile_, height));
            }, segment, area);
      }
      case ModelKind::clark: {
        const treevolume::clark::Result raw = treevolume::clark::volume(
            model_.clark, dbh_, ht_, lower, upper, clark_auxiliary());
        ScalarResult result{raw.value, raw.status};
        if (outside && result.status == api::status_ok) {
          const double bark = bark_ratio();
          if (!valid_bark(bark)) return missing_bark();
          result.value /= bark * bark;
        }
        return result;
      }
      case ModelKind::regional: {
        const treevolume::r10r4::Result raw = treevolume::r10r4::volume(
            model_.regional, dbh_, ht_, lower, upper);
        ScalarResult result{raw.value, raw.status};
        if (outside && result.status == api::status_ok) {
          const double bark = bark_ratio();
          if (!valid_bark(bark)) return missing_bark();
          result.value /= bark * bark;
        }
        return result;
      }
      case ModelKind::small_taper: {
        const treevolume::smalltapers::Result raw =
            treevolume::smalltapers::volume(
                model_.small_taper, dbh_, ht_, lower, upper,
                small_taper_auxiliary());
        ScalarResult result{raw.value, raw.status};
        if (outside && result.status == api::status_ok) {
          const double bark = bark_ratio();
          if (!valid_bark(bark)) return missing_bark();
          result.value /= bark * bark;
        }
        return result;
      }
      case ModelKind::nsvb: {
        if (!outside) {
          const treevolume::nsvb::Result result =
              treevolume::nsvb::volume_value(
                  model_.nsvb, dbh_, ht_, lower, upper);
          return ScalarResult{result.value, result.status};
        }
        return gauss_legendre_volume(
            lower, upper,
            [this](double height) { return diameter(true, height); },
            segment, area);
      }
    }
    return ScalarResult{};
  }

 private:
  static bool valid_bark(double value) {
    return value > 0.0 && value <= 1.0;
  }

  static ScalarResult missing_bark() {
    return ScalarResult{
        std::numeric_limits<double>::quiet_NaN(),
        api::status_capability_missing};
  }

  static ScalarResult finite_result(double value) {
    return std::isfinite(value)
        ? ScalarResult{value, api::status_ok} : ScalarResult{};
  }

  double bark_ratio() const {
    return aux_double_value(aux_.bark_ratio, tree_, model_.bark_ratio);
  }

  treevolume::flewelling::Auxiliary flewelling_auxiliary() const {
    treevolume::flewelling::Auxiliary auxiliary;
    auxiliary.bark_ratio = aux_.bark_ratio == nullptr
        ? 0.0 : aux_read<double>(aux_.bark_ratio, tree_);
    auxiliary.upper_ht1 = aux_double_value(
        aux_.upper_ht1, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.upper_d1 = aux_double_value(
        aux_.upper_d1, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.upper_ht2 = aux_double_value(
        aux_.upper_ht2, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.upper_d2 = aux_double_value(
        aux_.upper_d2, tree_, std::numeric_limits<double>::quiet_NaN());
    if (aux_.upper_bark != nullptr) {
      const std::uint8_t basis = aux_read<std::uint8_t>(
          aux_.upper_bark, tree_);
      auxiliary.upper_bark = basis == api::bark_outside ? 1 : 2;
    }
    return auxiliary;
  }

  treevolume::clark::Auxiliary clark_auxiliary() const {
    treevolume::clark::Auxiliary auxiliary;
    auxiliary.upper_ht1 = aux_double_value(
        aux_.upper_ht1, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.site_index = aux_double_value(
        aux_.site_index, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.basal_area = aux_double_value(
        aux_.basal_area, tree_, std::numeric_limits<double>::quiet_NaN());
    return auxiliary;
  }

  treevolume::smalltapers::Auxiliary small_taper_auxiliary() const {
    treevolume::smalltapers::Auxiliary auxiliary;
    auxiliary.upper_ht1 = aux_double_value(
        aux_.upper_ht1, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.upper_d1 = aux_double_value(
        aux_.upper_d1, tree_, std::numeric_limits<double>::quiet_NaN());
    auxiliary.form_class = aux_double_value(
        aux_.form_class, tree_, std::numeric_limits<double>::quiet_NaN());
    return auxiliary;
  }

  ScalarResult ensure_flewelling() {
    if (!flewelling_initialized_) {
      const treevolume::flewelling::Result result =
          treevolume::flewelling::initialize_profile(
              model_.flewelling, dbh_, ht_, flewelling_auxiliary(),
              &flewelling_profile_);
      flewelling_result_ = ScalarResult{result.value, result.status};
      flewelling_initialized_ = true;
    }
    return flewelling_result_.status == api::status_ok
        ? ScalarResult{0.0, api::status_ok} : flewelling_result_;
  }

  ScalarResult ensure_clark() {
    if (!clark_initialized_) {
      const treevolume::clark::Result result =
          treevolume::clark::initialize_profile(
              model_.clark, dbh_, ht_, clark_auxiliary(), &clark_profile_);
      clark_result_ = ScalarResult{result.value, result.status};
      clark_initialized_ = true;
    }
    return clark_result_.status == api::status_ok
        ? ScalarResult{0.0, api::status_ok} : clark_result_;
  }

  const ModelInfo& model_;
  double dbh_;
  double ht_;
  const AuxAccessor& aux_;
  std::size_t tree_;
  bool nvel_compat_;
  bool flewelling_initialized_ = false;
  bool clark_initialized_ = false;
  treevolume::flewelling::Profile flewelling_profile_;
  treevolume::clark::Profile clark_profile_;
  ScalarResult flewelling_result_;
  ScalarResult clark_result_;
};

template <typename Diameter>
ScalarResult discover_all_crossings(double lower, double upper, double target,
                                    double grid_step, Diameter diameter,
                                    std::vector<double>* roots) {
  roots->clear();
  const double span = std::max(upper - lower, 0.0);
  const std::size_t intervals = std::max<std::size_t>(
      1, static_cast<std::size_t>(std::ceil(span / grid_step)));
  ScalarResult left_result = diameter(lower);
  if (left_result.status != api::status_ok) return left_result;
  double left_height = lower;
  double left_difference = left_result.value - target;
  const double first_difference = left_difference;
  bool zero_run = left_difference == 0.0;
  double zero_start = lower;
  double zero_end = lower;
  for (std::size_t interval = 1; interval <= intervals; ++interval) {
    const double right_height = interval == intervals
        ? upper : lower + static_cast<double>(interval) * grid_step;
    const ScalarResult right_result = diameter(right_height);
    if (right_result.status != api::status_ok) return right_result;
    const double right_difference = right_result.value - target;
    if (zero_run) {
      if (right_difference == 0.0) {
        zero_end = right_height;
      } else {
        roots->push_back(zero_start);
        if (zero_end != zero_start) roots->push_back(zero_end);
        zero_run = false;
      }
    } else if (right_difference == 0.0) {
      zero_run = true;
      zero_start = right_height;
      zero_end = right_height;
    } else if ((left_difference < 0.0 && right_difference > 0.0) ||
               (left_difference > 0.0 && right_difference < 0.0)) {
      double bracket_lower = left_height;
      double bracket_upper = right_height;
      double lower_difference = left_difference;
      int iteration = 0;
      for (; iteration < 200 && bracket_upper - bracket_lower > 1e-4;
           ++iteration) {
        const double midpoint = (bracket_lower + bracket_upper) / 2.0;
        const ScalarResult midpoint_result = diameter(midpoint);
        if (midpoint_result.status != api::status_ok) return midpoint_result;
        const double midpoint_difference = midpoint_result.value - target;
        if (midpoint_difference == 0.0) {
          bracket_lower = midpoint;
          bracket_upper = midpoint;
        } else if (lower_difference * midpoint_difference <= 0.0) {
          bracket_upper = midpoint;
        } else {
          bracket_lower = midpoint;
          lower_difference = midpoint_difference;
        }
      }
      if (bracket_upper - bracket_lower > 1e-4) {
        return ScalarResult{
            std::numeric_limits<double>::quiet_NaN(),
            api::status_no_convergence};
      }
      roots->push_back((bracket_lower + bracket_upper) / 2.0);
    }
    left_height = right_height;
    left_difference = right_difference;
  }
  if (zero_run) {
    roots->push_back(zero_start);
    if (zero_end != zero_start) roots->push_back(zero_end);
  }
  roots->erase(std::unique(roots->begin(), roots->end()), roots->end());
  if (roots->empty()) {
    const std::int32_t status = left_difference > 0.0
        ? api::status_above_tip
        : first_difference < 0.0
        ? api::status_below_stump : api::status_no_convergence;
    return ScalarResult{
        std::numeric_limits<double>::quiet_NaN(), status};
  }
  return ScalarResult{0.0, api::status_ok};
}

const treevolume::nsvb_data::Species* species_record(std::int32_t spcd) {
  const auto* begin = std::begin(treevolume::nsvb_data::species);
  const auto* end = std::end(treevolume::nsvb_data::species);
  const auto* found = std::lower_bound(
      begin, end, static_cast<double>(spcd),
      [](const treevolume::nsvb_data::Species& value, double code) {
        return value.spcd < code;
      });
  return found != end && found->spcd == spcd ? found : nullptr;
}

}  // namespace

namespace treevolume {
namespace stem_provider {

struct Snapshot {
  std::uint64_t generation = 0;
  int threads = 1;
  bool nvel_compat = false;
  bool registry_active = false;
  mutable std::mutex mutex;
  mutable std::vector<std::shared_ptr<const ModelInfo>> models;
  mutable std::unordered_map<std::string, ModelHandle> handles;
};

}  // namespace stem_provider
}  // namespace treevolume

namespace {

std::shared_ptr<const ModelInfo> model_from_handle(
    const api::Snapshot* snapshot, api::ModelHandle handle) {
  if (snapshot == nullptr || handle == 0U) return nullptr;
  const std::uint32_t raw = handle & ~api::R_KERNEL_HANDLE_MASK;
  if (raw == 0U) return nullptr;
  std::lock_guard<std::mutex> lock(snapshot->mutex);
  const std::size_t slot = static_cast<std::size_t>(raw - 1U);
  if (slot >= snapshot->models.size()) return nullptr;
  const std::shared_ptr<const ModelInfo> model = snapshot->models[slot];
  const bool tagged_r = api::is_r_kernel(handle);
  if (tagged_r != (model->kind == ModelKind::r_kernel)) return nullptr;
  return model;
}

api::ModelHandle insert_model(api::Snapshot* snapshot,
                              std::shared_ptr<ModelInfo> model) {
  std::lock_guard<std::mutex> lock(snapshot->mutex);
  const auto present = snapshot->handles.find(model->id);
  if (present != snapshot->handles.end()) return present->second;
  if (snapshot->models.size() + 1 >= api::R_KERNEL_HANDLE_MASK) {
    throw std::runtime_error("provider model handle space is exhausted");
  }
  const std::size_t slot = snapshot->models.size();
  const api::ModelHandle handle = handle_for_slot(*model, slot);
  snapshot->models.push_back(std::move(model));
  snapshot->handles.emplace(snapshot->models.back()->id, handle);
  return handle;
}

api::ModelHandle resolve_one(api::Snapshot* snapshot, const std::string& id) {
  {
    std::lock_guard<std::mutex> lock(snapshot->mutex);
    const auto found = snapshot->handles.find(id);
    if (found != snapshot->handles.end()) return found->second;
  }
  const Rcpp::Environment ns = Rcpp::Environment::namespace_env("merchandiser");
  const Rcpp::Function lookup = ns[".registry_lookup"];
  const Rcpp::RObject entry = lookup(id);
  if (entry.isNULL()) return 0U;
  const Rcpp::List entry_list(entry);
  const Rcpp::List model = entry_list["model"];
  return insert_model(snapshot, copy_model(model));
}

int collect_models(const api::Snapshot* snapshot, std::size_t n_tree,
                   const api::ModelHandle* handles,
                   std::vector<std::shared_ptr<const ModelInfo>>* models,
                   api::ErrorBuffer* error) {
  models->resize(n_tree);
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    (*models)[tree] = model_from_handle(snapshot, handles[tree]);
    if ((*models)[tree] == nullptr) {
      return fail(api::call_invalid_argument, error,
                  "a model handle is invalid for this snapshot");
    }
  }
  return api::call_ok;
}

bool valid_aux_domain(const std::string& name, const AuxAccessor& aux,
                      std::size_t tree) {
  const api::AuxColumn* column = aux.get(name);
  if (column == nullptr) return true;
  if (name == "upper_bark") {
    const std::uint8_t value = aux_read<std::uint8_t>(column, tree);
    return value == api::bark_inside || value == api::bark_outside;
  }
  if (name == "spcd") {
    return aux_read<std::int32_t>(column, tree) > 0;
  }
  const double value = aux_read<double>(column, tree);
  if (!std::isfinite(value)) return true;
  if (name == "bark_ratio") return value > 0.0 && value <= 1.0;
  if (name == "decay_class") {
    return value >= 1.0 && value <= 5.0 && value == std::floor(value);
  }
  if (name == "cull") return value >= 0.0 && value <= 100.0;
  return value > 0.0;
}

bool missing_aux(const api::AuxColumn* column, std::size_t tree) {
  if (column == nullptr) return true;
  if (column->type == api::aux_double) {
    return !std::isfinite(aux_read<double>(column, tree));
  }
  if (column->type == api::aux_int32) {
    return aux_read<std::int32_t>(column, tree) <= 0;
  }
  return false;
}

int prepare_tree_status(
    std::size_t n_tree, const double* dbh, const double* ht,
    const std::vector<std::shared_ptr<const ModelInfo>>& models,
    const AuxAccessor& aux, std::vector<std::int32_t>* status,
    api::ErrorBuffer* error) {
  status->assign(n_tree, api::status_ok);
  std::unordered_set<std::string> declared{"spcd"};
  for (const auto& model : models) {
    declared.insert(model->required.begin(), model->required.end());
    declared.insert(model->optional.begin(), model->optional.end());
  }
  static const char* names[] = {
      "form_class", "upper_ht1", "upper_d1", "upper_ht2", "upper_d2",
      "site_index", "basal_area", "bark_ratio", "decay_class", "cull",
      "upper_bark", "spcd"};
  for (const char* name : names) {
    if (aux.get(name) != nullptr && declared.find(name) == declared.end()) {
      return fail(api::call_invalid_argument, error,
                  std::string("undeclared auxiliary column: ") + name);
    }
  }

  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    if (!std::isfinite(dbh[tree]) || !std::isfinite(ht[tree])) {
      (*status)[tree] = api::status_na_input;
    } else if (!(dbh[tree] > 0.0)) {
      (*status)[tree] = api::status_dbh_nonpositive;
    } else if (!(ht[tree] > 0.0)) {
      (*status)[tree] = api::status_ht_nonpositive;
    }
    if (aux.spcd != nullptr) {
      const std::int32_t spcd = aux_read<std::int32_t>(aux.spcd, tree);
      if (spcd <= 0 && (*status)[tree] == api::status_ok) {
        (*status)[tree] = api::status_unknown_species;
      }
    }
    const ModelInfo& model = *models[tree];
    for (const std::string& name : model.required) {
      if (aux.get(name) == nullptr && (*status)[tree] == api::status_ok) {
        (*status)[tree] = api::status_missing_input;
      }
    }
    for (const std::string& name : model.required) {
      const api::AuxColumn* column = aux.get(name);
      if (column != nullptr && missing_aux(column, tree) &&
          ((*status)[tree] == api::status_ok ||
           (*status)[tree] == api::status_species_out_of_scope)) {
        (*status)[tree] = api::status_na_input;
      }
    }
    for (const std::string& name : model.optional) {
      const api::AuxColumn* column = aux.get(name);
      if (column != nullptr && missing_aux(column, tree) &&
          ((*status)[tree] == api::status_ok ||
           (*status)[tree] == api::status_species_out_of_scope)) {
        (*status)[tree] = api::status_na_input;
      }
    }
    for (const auto& pair : model.pairs) {
      int supplied = 0;
      for (const std::string& name : pair) {
        supplied += aux.get(name) != nullptr ? 1 : 0;
      }
      if (supplied > 0 && supplied != static_cast<int>(pair.size()) &&
          (*status)[tree] == api::status_ok) {
        (*status)[tree] = api::status_missing_input;
      }
    }
    for (const std::string& name : model.required) {
      if (!valid_aux_domain(name, aux, tree)) {
        return fail(api::call_invalid_argument, error,
                    "auxiliary value is out of domain: " + name);
      }
    }
    for (const std::string& name : model.optional) {
      if (!valid_aux_domain(name, aux, tree)) {
        return fail(api::call_invalid_argument, error,
                    "auxiliary value is out of domain: " + name);
      }
    }
    if (aux.spcd != nullptr && !model.species.empty() &&
        (*status)[tree] == api::status_ok) {
      const int spcd = aux_read<std::int32_t>(aux.spcd, tree);
      if (std::find(model.species.begin(), model.species.end(), spcd) ==
          model.species.end()) {
        (*status)[tree] = api::status_species_out_of_scope;
      }
    }
    if (model.kind == ModelKind::r_kernel &&
        successful((*status)[tree])) {
      (*status)[tree] = api::status_capability_missing;
    }
  }
  return api::call_ok;
}

struct QueryWorker : public RcppParallel::Worker {
  const std::vector<std::shared_ptr<const ModelInfo>>& models;
  const std::vector<std::int32_t>& tree_status;
  const double* dbh;
  const double* ht;
  AuxAccessor aux;
  const std::uint64_t* tree_index;
  const double* height;
  double* dib;
  double* dob;
  double* cum_ib;
  double* cum_ob;
  std::int32_t* status;
  bool nvel_compat;

  QueryWorker(
      const std::vector<std::shared_ptr<const ModelInfo>>& models_,
      const std::vector<std::int32_t>& tree_status_, const double* dbh_,
      const double* ht_, AuxAccessor aux_, const std::uint64_t* tree_index_,
      const double* height_, double* dib_, double* dob_, double* cum_ib_,
      double* cum_ob_, std::int32_t* status_, bool nvel_compat_)
      : models(models_), tree_status(tree_status_), dbh(dbh_), ht(ht_),
        aux(aux_), tree_index(tree_index_), height(height_), dib(dib_),
        dob(dob_), cum_ib(cum_ib_), cum_ob(cum_ob_), status(status_),
        nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::uint64_t cached_tree = std::numeric_limits<std::uint64_t>::max();
    std::unique_ptr<Evaluator> evaluator;
    for (std::size_t query = begin; query < end; ++query) {
      const std::size_t tree = static_cast<std::size_t>(tree_index[query]);
      std::int32_t current = tree_status[tree];
      if (!std::isfinite(height[query])) {
        current = api::status_na_input;
      } else if (successful(current) &&
                 (height[query] < 0.0 || height[query] > ht[tree])) {
        current = api::status_height_out_of_range;
      }
      if (successful(current)) {
        if (cached_tree != tree) {
          evaluator = std::make_unique<Evaluator>(
              *models[tree], dbh[tree], ht[tree], aux, tree, nvel_compat);
          cached_tree = tree;
        }
        if (dib != nullptr) {
          const ScalarResult result = evaluator->diameter(false, height[query]);
          dib[query] = result.value;
          merge_status(&current, result.status);
        }
        if (dob != nullptr) {
          const ScalarResult result = evaluator->diameter(true, height[query]);
          dob[query] = result.value;
          merge_status(&current, result.status);
        }
        if (cum_ib != nullptr) {
          const ScalarResult result = evaluator->volume(
              false, 0.0, height[query]);
          cum_ib[query] = result.value;
          merge_status(&current, result.status);
        }
        if (cum_ob != nullptr) {
          const ScalarResult result = evaluator->volume(
              true, 0.0, height[query]);
          cum_ob[query] = result.value;
          merge_status(&current, result.status);
        }
      }
      if (!successful(current)) {
        const double missing = std::numeric_limits<double>::quiet_NaN();
        if (dib != nullptr) dib[query] = missing;
        if (dob != nullptr) dob[query] = missing;
        if (cum_ib != nullptr) cum_ib[query] = missing;
        if (cum_ob != nullptr) cum_ob[query] = missing;
      }
      status[query] = current;
    }
  }
};

struct CrossingWorker : public RcppParallel::Worker {
  const std::vector<std::shared_ptr<const ModelInfo>>& models;
  const std::vector<std::int32_t>& tree_status;
  const double* dbh;
  const double* ht;
  AuxAccessor aux;
  const double* target;
  const std::uint8_t* basis;
  const double* lower;
  const double* upper;
  std::vector<std::vector<double>>& roots;
  std::int32_t* status;
  bool nvel_compat;

  CrossingWorker(
      const std::vector<std::shared_ptr<const ModelInfo>>& models_,
      const std::vector<std::int32_t>& tree_status_, const double* dbh_,
      const double* ht_, AuxAccessor aux_, const double* target_,
      const std::uint8_t* basis_, const double* lower_, const double* upper_,
      std::vector<std::vector<double>>& roots_, std::int32_t* status_,
      bool nvel_compat_)
      : models(models_), tree_status(tree_status_), dbh(dbh_), ht(ht_),
        aux(aux_), target(target_), basis(basis_), lower(lower_), upper(upper_),
        roots(roots_), status(status_), nvel_compat(nvel_compat_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t tree = begin; tree < end; ++tree) {
      std::int32_t current = tree_status[tree];
      if (!std::isfinite(target[tree]) || !std::isfinite(lower[tree]) ||
          !std::isfinite(upper[tree])) {
        current = api::status_na_input;
      } else if (successful(current) && !(target[tree] > 0.0)) {
        current = api::status_diameter_nonpositive;
      } else if (successful(current) &&
                 (lower[tree] < 0.0 || upper[tree] > ht[tree])) {
        current = api::status_height_out_of_range;
      } else if (successful(current) && !(lower[tree] < upper[tree])) {
        current = api::status_empty_bounds;
      }
      if (successful(current)) {
        Evaluator evaluator(
            *models[tree], dbh[tree], ht[tree], aux, tree, nvel_compat);
        const bool outside = basis[tree] == api::bark_outside;
        const double step = models[tree]->units == api::units_metric
            ? 0.0254 / 16.0 : 1.0 / (12.0 * 16.0);
        const ScalarResult result = discover_all_crossings(
            lower[tree], upper[tree], target[tree], step,
            [&evaluator, outside](double height) {
              return evaluator.diameter(outside, height);
            }, &roots[tree]);
        merge_status(&current, result.status);
      }
      if (!successful(current)) roots[tree].clear();
      status[tree] = current;
    }
  }
};

struct GreenWeightWorker : public RcppParallel::Worker {
  const double* volume;
  const std::int32_t* spcd;
  const std::uint8_t* basis;
  const std::uint8_t* component;
  const std::uint8_t* moisture;
  double* weight;
  std::int32_t* status;

  GreenWeightWorker(const double* volume_, const std::int32_t* spcd_,
                    const std::uint8_t* basis_,
                    const std::uint8_t* component_,
                    const std::uint8_t* moisture_, double* weight_,
                    std::int32_t* status_)
      : volume(volume_), spcd(spcd_), basis(basis_), component(component_),
        moisture(moisture_), weight(weight_), status(status_) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t index = begin; index < end; ++index) {
      weight[index] = std::numeric_limits<double>::quiet_NaN();
      if (!std::isfinite(volume[index]) || volume[index] < 0.0) {
        status[index] = api::status_na_input;
        continue;
      }
      const treevolume::nsvb_data::Species* reference =
          species_record(spcd[index]);
      if (reference == nullptr) {
        status[index] = api::status_unknown_species;
        continue;
      }
      const bool needs_wood = component[index] != api::component_bark;
      const bool needs_bark = component[index] != api::component_wood;
      const bool needs_ratio = needs_bark || basis[index] == api::bark_outside;
      const bool green = moisture[index] == api::moisture_green;
      const bool missing_wood = needs_wood &&
          (!(reference->wood_dry > 0.0) ||
           (green && !(reference->wood_moisture >= 0.0)));
      const bool missing_bark = needs_bark &&
          (!(reference->bark_dry > 0.0) ||
           (green && !(reference->bark_moisture >= 0.0)));
      const bool missing_ratio = needs_ratio && !(reference->bark_volume > 0.0);
      if (missing_wood || missing_bark || missing_ratio) {
        status[index] = api::status_na_input;
        continue;
      }
      const double ratio = reference->bark_volume / 100.0;
      const double wood_volume = basis[index] == api::bark_inside
          ? volume[index] : volume[index] / (1.0 + ratio);
      const double bark_volume = basis[index] == api::bark_inside
          ? volume[index] * ratio : volume[index] * ratio / (1.0 + ratio);
      const double wood_multiplier = green
          ? 1.0 + reference->wood_moisture / 100.0 : 1.0;
      const double bark_multiplier = green
          ? 1.0 + reference->bark_moisture / 100.0 : 1.0;
      const double wood_weight =
          wood_volume * reference->wood_dry * wood_multiplier;
      const double bark_weight =
          bark_volume * reference->bark_dry * bark_multiplier;
      weight[index] = component[index] == api::component_stem
          ? wood_weight + bark_weight
          : component[index] == api::component_wood
          ? wood_weight : bark_weight;
      status[index] = api::status_ok;
    }
  }
};

}  // namespace


const char* api::nvel_source_revision() noexcept {
  return kNvelRevision;
}

int api::snapshot_open(
    api::Snapshot** out, std::uint64_t* generation,
    api::ErrorBuffer* error) noexcept {
  clear_error(error);
  if (out == nullptr || generation == nullptr) {
    return fail(api::call_invalid_argument, error,
                "snapshot output pointers must not be null");
  }
  *out = nullptr;
  *generation = 0;
  bool entered = false;
  std::uint64_t entered_generation = 0;
  try {
    const Rcpp::Environment ns =
        Rcpp::Environment::namespace_env("merchandiser");
    const Rcpp::Function enter = ns[".registry_enter"];
    entered_generation = static_cast<std::uint64_t>(Rcpp::as<int>(enter()));
    entered = true;
    auto snapshot = std::make_unique<api::Snapshot>();
    snapshot->generation = entered_generation;
    snapshot->registry_active = true;
    const Rcpp::Environment runtime = ns[".tv_runtime"];
    snapshot->threads = std::max(1, Rcpp::as<int>(runtime["threads"]));
    const Rcpp::Function compat = ns[".treevolume_compat"];
    snapshot->nvel_compat = Rcpp::as<std::string>(compat()) == "nvel";
    const Rcpp::Environment registry = ns[".tv_registry"];
    const Rcpp::List entries = registry["models"];
    for (R_xlen_t index = 0; index < entries.size(); ++index) {
      const Rcpp::List entry = entries[index];
      const Rcpp::List model = entry["model"];
      insert_model(snapshot.get(), copy_model(model));
    }
    *generation = snapshot->generation;
    *out = snapshot.release();
    return api::call_ok;
  } catch (const std::exception& exception) {
    if (entered) {
      try {
        const Rcpp::Environment ns =
            Rcpp::Environment::namespace_env("merchandiser");
        const Rcpp::Function exit = ns[".registry_exit"];
        exit(static_cast<int>(entered_generation));
      } catch (...) {
      }
    }
    return fail(api::call_internal_error, error, exception.what());
  } catch (...) {
    if (entered) {
      try {
        const Rcpp::Environment ns =
            Rcpp::Environment::namespace_env("merchandiser");
        const Rcpp::Function exit = ns[".registry_exit"];
        exit(static_cast<int>(entered_generation));
      } catch (...) {
      }
    }
    return fail(api::call_internal_error, error,
                "snapshot creation failed");
  }
}

void api::snapshot_close(api::Snapshot* snapshot) noexcept {
  if (snapshot == nullptr) return;
  const bool active = snapshot->registry_active;
  const std::uint64_t generation = snapshot->generation;
  delete snapshot;
  if (active) {
    try {
      const Rcpp::Environment ns =
          Rcpp::Environment::namespace_env("merchandiser");
      const Rcpp::Function exit = ns[".registry_exit"];
      exit(static_cast<int>(generation));
    } catch (...) {
    }
  }
}

int api::resolve_models(
    const api::Snapshot* snapshot, const char* const* ids, std::size_t n,
    api::ModelHandle* out, std::int32_t* status,
    api::ErrorBuffer* error) noexcept {
  clear_error(error);
  if (snapshot == nullptr || (n > 0 &&
      (ids == nullptr || out == nullptr || status == nullptr))) {
    return fail(api::call_invalid_argument, error,
                "resolve_models received a null required pointer");
  }
  try {
    api::Snapshot* mutable_snapshot = const_cast<api::Snapshot*>(snapshot);
    for (std::size_t index = 0; index < n; ++index) {
      out[index] = 0U;
      if (ids[index] == nullptr || ids[index][0] == '\0') {
        status[index] = api::status_na_input;
        continue;
      }
      out[index] = resolve_one(mutable_snapshot, ids[index]);
      status[index] = out[index] == 0U
          ? api::status_unknown_model : api::status_ok;
    }
    return api::call_ok;
  } catch (const std::exception& exception) {
    return fail(api::call_internal_error, error, exception.what());
  } catch (...) {
    return fail(api::call_internal_error, error,
                "model resolution failed");
  }
}

int api::query_ragged(
    const api::Snapshot* snapshot, std::size_t n_tree, const double* dbh,
    const double* ht, const api::ModelHandle* handles, api::AuxView aux_view,
    std::size_t n_query, const std::uint64_t* tree_index, const double* height,
    double* dib, double* dob, double* cum_ib_ground, double* cum_ob_ground,
    std::int32_t* status, api::ErrorBuffer* error) noexcept {
  clear_error(error);
  if (snapshot == nullptr ||
      (n_tree > 0 && (dbh == nullptr || ht == nullptr || handles == nullptr)) ||
      (n_query > 0 && (tree_index == nullptr || height == nullptr ||
                       status == nullptr)) ||
      (n_query > 0 && dib == nullptr && dob == nullptr &&
       cum_ib_ground == nullptr && cum_ob_ground == nullptr)) {
    return fail(api::call_invalid_argument, error,
                "query_ragged received a null required pointer");
  }
  try {
    AuxAccessor aux;
    int code = prepare_aux(aux_view, &aux, error);
    if (code != api::call_ok) return code;
    for (std::size_t query = 0; query < n_query; ++query) {
      if (tree_index[query] >= n_tree) {
        return fail(api::call_invalid_argument, error,
                    "a ragged tree index is out of range");
      }
    }
    std::vector<std::shared_ptr<const ModelInfo>> models;
    code = collect_models(snapshot, n_tree, handles, &models, error);
    if (code != api::call_ok) return code;
    std::vector<std::int32_t> tree_status;
    code = prepare_tree_status(
        n_tree, dbh, ht, models, aux, &tree_status, error);
    if (code != api::call_ok) return code;
    QueryWorker worker{
        models, tree_status, dbh, ht, aux, tree_index, height, dib, dob,
        cum_ib_ground, cum_ob_ground, status, snapshot->nvel_compat};
    RcppParallel::parallelFor(
        0, n_query, worker, 1, snapshot->threads);
    return api::call_ok;
  } catch (const std::exception& exception) {
    return fail(api::call_internal_error, error, exception.what());
  } catch (...) {
    return fail(api::call_internal_error, error, "ragged query failed");
  }
}

int api::all_crossings(
    const api::Snapshot* snapshot, std::size_t n_tree, const double* dbh,
    const double* ht, const api::ModelHandle* handles, api::AuxView aux_view,
    const double* target, const std::uint8_t* bark_basis, const double* lower,
    const double* upper, std::uint64_t* offsets, double* roots,
    std::size_t roots_capacity, std::size_t* roots_needed,
    std::int32_t* status, api::ErrorBuffer* error) noexcept {
  clear_error(error);
  if (snapshot == nullptr || offsets == nullptr || roots_needed == nullptr ||
      (n_tree > 0 && (dbh == nullptr || ht == nullptr || handles == nullptr ||
                     target == nullptr || bark_basis == nullptr ||
                     lower == nullptr || upper == nullptr ||
                     status == nullptr)) ||
      (roots_capacity > 0 && roots == nullptr)) {
    return fail(api::call_invalid_argument, error,
                "all_crossings received a null required pointer");
  }
  *roots_needed = 0;
  try {
    for (std::size_t tree = 0; tree < n_tree; ++tree) {
      if (bark_basis[tree] != api::bark_inside &&
          bark_basis[tree] != api::bark_outside) {
        return fail(api::call_invalid_argument, error,
                    "a crossing bark basis is invalid");
      }
    }
    AuxAccessor aux;
    int code = prepare_aux(aux_view, &aux, error);
    if (code != api::call_ok) return code;
    std::vector<std::shared_ptr<const ModelInfo>> models;
    code = collect_models(snapshot, n_tree, handles, &models, error);
    if (code != api::call_ok) return code;
    std::vector<std::int32_t> tree_status;
    code = prepare_tree_status(
        n_tree, dbh, ht, models, aux, &tree_status, error);
    if (code != api::call_ok) return code;
    std::vector<std::vector<double>> tree_roots(n_tree);
    CrossingWorker worker{
        models, tree_status, dbh, ht, aux, target, bark_basis, lower, upper,
        tree_roots, status, snapshot->nvel_compat};
    RcppParallel::parallelFor(
        0, n_tree, worker, 1, snapshot->threads);
    std::uint64_t total = 0;
    offsets[0] = 0;
    for (std::size_t tree = 0; tree < n_tree; ++tree) {
      const std::size_t count = tree_roots[tree].size();
      if (count > std::numeric_limits<std::uint64_t>::max() - total) {
        return fail(api::call_internal_error, error,
                    "crossing root count overflowed");
      }
      total += static_cast<std::uint64_t>(count);
      offsets[tree + 1] = total;
    }
    if (total > std::numeric_limits<std::size_t>::max()) {
      return fail(api::call_internal_error, error,
                  "crossing root count cannot fit in size_t");
    }
    *roots_needed = static_cast<std::size_t>(total);
    if (roots_capacity < *roots_needed) {
      return fail(api::call_buffer_too_small, error,
                  "crossing root buffer is too small");
    }
    std::size_t position = 0;
    for (const auto& values : tree_roots) {
      if (!values.empty()) {
        std::copy(values.begin(), values.end(), roots + position);
      }
      position += values.size();
    }
    return api::call_ok;
  } catch (const std::exception& exception) {
    return fail(api::call_internal_error, error, exception.what());
  } catch (...) {
    return fail(api::call_internal_error, error, "crossing query failed");
  }
}

int api::green_weight_batch(
    std::size_t n, const double* volume, const std::int32_t* spcd,
    const std::uint8_t* volume_basis, const std::uint8_t* component,
    const std::uint8_t* moisture, double* weight, std::int32_t* status,
    api::ErrorBuffer* error) noexcept {
  clear_error(error);
  if (n > 0 && (volume == nullptr || spcd == nullptr ||
                volume_basis == nullptr || component == nullptr ||
                moisture == nullptr || weight == nullptr || status == nullptr)) {
    return fail(api::call_invalid_argument, error,
                "green_weight_batch received a null required pointer");
  }
  try {
    for (std::size_t index = 0; index < n; ++index) {
      if ((volume_basis[index] != api::bark_inside &&
           volume_basis[index] != api::bark_outside) ||
          component[index] > api::component_bark ||
          moisture[index] > api::moisture_dry) {
        return fail(api::call_invalid_argument, error,
                    "a green-weight enumeration value is invalid");
      }
    }
    GreenWeightWorker worker{
        volume, spcd, volume_basis, component, moisture, weight, status};
    const Rcpp::Environment ns =
        Rcpp::Environment::namespace_env("merchandiser");
    const Rcpp::Environment runtime = ns[".tv_runtime"];
    const int threads = std::max(1, Rcpp::as<int>(runtime["threads"]));
    RcppParallel::parallelFor(0, n, worker, 1, threads);
    return api::call_ok;
  } catch (const std::exception& exception) {
    return fail(api::call_internal_error, error, exception.what());
  } catch (...) {
    return fail(api::call_internal_error, error,
                "green-weight evaluation failed");
  }
}

std::uint64_t api::snapshot_generation(
    const api::Snapshot* snapshot) noexcept {
  return snapshot == nullptr ? 0 : snapshot->generation;
}

int api::model_metadata(
    const api::Snapshot* snapshot, api::ModelHandle handle,
    api::ModelMetadata* out, api::ErrorBuffer* error) noexcept {
  clear_error(error);
  if (snapshot == nullptr || out == nullptr) {
    return fail(api::call_invalid_argument, error,
                "model_metadata received a null required pointer");
  }
  try {
    const std::shared_ptr<const ModelInfo> model =
        model_from_handle(snapshot, handle);
    if (model == nullptr) {
      return fail(api::call_invalid_argument, error,
                  "the model handle is invalid for this snapshot");
    }
    out->struct_size = sizeof(api::ModelMetadata);
    out->kernel_type = model->kind == ModelKind::r_kernel
        ? api::kernel_r : api::kernel_compiled;
    out->units = model->units;
    out->oracle_verified = model->verification;
    out->has_dob = model->has_dob || std::isfinite(model->bark_ratio);
    out->has_inverse = model->has_inverse;
    out->has_integral = model->has_integral;
    out->stump_height = model->stump_height;
    out->bark_ratio = model->bark_ratio;
    out->id = model->id.c_str();
    out->kernel_key = model->key.empty() ? nullptr : model->key.c_str();
    return api::call_ok;
  } catch (const std::exception& exception) {
    return fail(api::call_internal_error, error, exception.what());
  } catch (...) {
    return fail(api::call_internal_error, error,
                "model metadata query failed");
  }
}

