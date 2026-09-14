#include <Rcpp.h>
#include "stem_provider.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace provider = treevolume::stem_provider;

namespace {

using ProfileClock = std::chrono::steady_clock;

double profile_seconds(ProfileClock::time_point begin,
                       ProfileClock::time_point end) {
  return std::chrono::duration<double>(end - begin).count();
}

std::uint64_t mix_hash(std::uint64_t hash, std::uint64_t value) {
  hash ^= value + UINT64_C(0x9e3779b97f4a7c15) + (hash << 6) + (hash >> 2);
  return hash;
}

std::uint64_t hash_bytes(const char* data, std::size_t size) {
  std::uint64_t hash = UINT64_C(1469598103934665603);
  for (std::size_t index = 0; index < size; ++index) {
    hash ^= static_cast<unsigned char>(data[index]);
    hash *= UINT64_C(1099511628211);
  }
  return hash;
}

std::uint64_t column_hash(SEXP column, R_xlen_t row) {
  switch (TYPEOF(column)) {
  case REALSXP: {
    const double value = REAL(column)[row];
    if (R_IsNA(value) || R_IsNaN(value)) return UINT64_C(0x7ff8000000000000);
    if (value == 0.0) return 0;
    std::uint64_t bits = 0;
    std::memcpy(&bits, &value, sizeof(bits));
    return bits;
  }
  case INTSXP:
  case LGLSXP:
    return static_cast<std::uint64_t>(
      static_cast<std::uint32_t>(INTEGER(column)[row])
    );
  case STRSXP: {
    SEXP value = STRING_ELT(column, row);
    if (value == NA_STRING) return UINT64_C(0xffffffffffffffff);
    const char* text = Rf_translateCharUTF8(value);
    return hash_bytes(text, std::strlen(text));
  }
  default:
    Rcpp::stop("Duplicate-map columns must be double, integer, logical, or character.");
  }
  return 0;
}

bool column_equal(SEXP column, R_xlen_t left, R_xlen_t right) {
  switch (TYPEOF(column)) {
  case REALSXP: {
    const double lhs = REAL(column)[left];
    const double rhs = REAL(column)[right];
    if ((R_IsNA(lhs) || R_IsNaN(lhs)) && (R_IsNA(rhs) || R_IsNaN(rhs))) {
      return true;
    }
    return lhs == rhs;
  }
  case INTSXP:
  case LGLSXP:
    return INTEGER(column)[left] == INTEGER(column)[right];
  case STRSXP: {
    SEXP lhs = STRING_ELT(column, left);
    SEXP rhs = STRING_ELT(column, right);
    if (lhs == NA_STRING || rhs == NA_STRING) return lhs == rhs;
    return std::strcmp(Rf_translateCharUTF8(lhs), Rf_translateCharUTF8(rhs)) == 0;
  }
  default:
    return false;
  }
}

std::uint64_t stem_hash(const Rcpp::List& columns,
                        const Rcpp::IntegerVector& defect_offsets,
                        const Rcpp::List& defect_columns, R_xlen_t row) {
  std::uint64_t hash = UINT64_C(0xcbf29ce484222325);
  for (R_xlen_t column = 0; column < columns.size(); ++column) {
    hash = mix_hash(hash, column_hash(columns[column], row));
  }
  const int begin = defect_offsets[row];
  const int end = defect_offsets[row + 1];
  hash = mix_hash(hash, static_cast<std::uint64_t>(end - begin));
  for (int defect = begin; defect < end; ++defect) {
    for (R_xlen_t column = 0; column < defect_columns.size(); ++column) {
      hash = mix_hash(hash, column_hash(defect_columns[column], defect));
    }
  }
  return hash;
}

bool stem_equal(const Rcpp::List& columns,
                const Rcpp::IntegerVector& defect_offsets,
                const Rcpp::List& defect_columns, R_xlen_t left,
                R_xlen_t right) {
  for (R_xlen_t column = 0; column < columns.size(); ++column) {
    if (!column_equal(columns[column], left, right)) return false;
  }
  const int left_begin = defect_offsets[left];
  const int left_end = defect_offsets[left + 1];
  const int right_begin = defect_offsets[right];
  const int right_end = defect_offsets[right + 1];
  if (left_end - left_begin != right_end - right_begin) return false;
  for (int offset = 0; offset < left_end - left_begin; ++offset) {
    for (R_xlen_t column = 0; column < defect_columns.size(); ++column) {
      if (!column_equal(
          defect_columns[column], left_begin + offset, right_begin + offset
      )) return false;
    }
  }
  return true;
}

struct ProviderSession {
  provider::Snapshot* snapshot = nullptr;
  std::uint64_t generation = 0;
  std::vector<provider::ModelHandle> handles;
  std::vector<provider::ModelMetadata> metadata;

  ~ProviderSession() {
    if (snapshot != nullptr) provider::snapshot_close(snapshot);
  }
};

struct ErrorStorage {
  std::vector<char> bytes;
  provider::ErrorBuffer view;

  ErrorStorage() : bytes(4096, '\0'), view{bytes.data(), bytes.size(), 0} {}

  std::string message(const char* operation, int code) const {
    std::string result(operation);
    result += " failed with provider call code ";
    result += std::to_string(code);
    if (view.used > 0) {
      result += ": ";
      result.append(bytes.data(), std::min(view.used, bytes.size()));
    }
    return result;
  }
};

inline double diameter_to_native(double value, int caller_units,
                                 std::uint32_t native_units) {
  if (caller_units == static_cast<int>(native_units)) return value;
  return caller_units == provider::units_imperial ? value * 2.54 : value / 2.54;
}

inline double diameter_from_native(double value, int caller_units,
                                   std::uint32_t native_units) {
  if (caller_units == static_cast<int>(native_units)) return value;
  return caller_units == provider::units_imperial ? value / 2.54 : value * 2.54;
}

inline double height_to_native(double value, int caller_units,
                               std::uint32_t native_units) {
  if (caller_units == static_cast<int>(native_units)) return value;
  return caller_units == provider::units_imperial ? value * 0.3048 : value / 0.3048;
}

inline double height_from_native(double value, int caller_units,
                                 std::uint32_t native_units) {
  if (caller_units == static_cast<int>(native_units)) return value;
  return caller_units == provider::units_imperial ? value / 0.3048 : value * 0.3048;
}

inline double volume_from_native(double value, int caller_units,
                                 std::uint32_t native_units) {
  if (caller_units == static_cast<int>(native_units)) return value;
  return caller_units == provider::units_imperial
    ? value / 0.028316846592
    : value * 0.028316846592;
}

ProviderSession& session_from(SEXP pointer) {
  Rcpp::XPtr<ProviderSession> session(pointer);
  if (session.get() == nullptr || session->snapshot == nullptr) {
    Rcpp::stop("The treevolume provider session is closed.");
  }
  return *session;
}

struct AuxStorage {
  std::vector<std::string> names;
  std::vector<std::vector<double>> doubles;
  std::vector<std::vector<std::int32_t>> integers;
  std::vector<std::vector<std::uint8_t>> bytes;
  std::vector<provider::AuxColumn> columns;
};

AuxStorage prepare_aux(const Rcpp::List& aux, std::size_t n_tree,
                       const Rcpp::IntegerVector& model_index,
                       const ProviderSession& session, int caller_units) {
  AuxStorage storage;
  if (aux.size() == 0) return storage;
  Rcpp::CharacterVector names = aux.names();
  storage.names.reserve(aux.size());
  storage.doubles.reserve(aux.size());
  storage.integers.reserve(aux.size());
  storage.bytes.reserve(aux.size());
  storage.columns.reserve(aux.size());

  for (R_xlen_t column = 0; column < aux.size(); ++column) {
    std::string name = Rcpp::as<std::string>(names[column]);
    storage.names.push_back(name);
    if (name == "spcd") {
      Rcpp::IntegerVector value(aux[column]);
      if (static_cast<std::size_t>(value.size()) != n_tree) {
        Rcpp::stop("Every provider auxiliary column must have tree length.");
      }
      storage.integers.emplace_back(value.begin(), value.end());
      storage.columns.push_back({
        storage.names.back().c_str(), provider::aux_int32,
        storage.integers.back().data(), sizeof(std::int32_t)
      });
      continue;
    }
    if (name == "upper_bark") {
      Rcpp::CharacterVector value(aux[column]);
      if (static_cast<std::size_t>(value.size()) != n_tree) {
        Rcpp::stop("Every provider auxiliary column must have tree length.");
      }
      storage.bytes.emplace_back(n_tree, provider::bark_inside);
      for (std::size_t row = 0; row < n_tree; ++row) {
        if (value[row] == NA_STRING) continue;
        const std::string basis = Rcpp::as<std::string>(value[row]);
        storage.bytes.back()[row] = basis == "ob"
          ? provider::bark_outside
          : provider::bark_inside;
      }
      storage.columns.push_back({
        storage.names.back().c_str(), provider::aux_uint8,
        storage.bytes.back().data(), sizeof(std::uint8_t)
      });
      continue;
    }
    Rcpp::NumericVector value(aux[column]);
    if (static_cast<std::size_t>(value.size()) != n_tree) {
      Rcpp::stop("Every provider auxiliary column must have tree length.");
    }
    storage.doubles.emplace_back(value.begin(), value.end());
    std::vector<double>& output = storage.doubles.back();
    for (std::size_t row = 0; row < n_tree; ++row) {
      if (!R_finite(output[row])) continue;
      const int model = model_index[row] - 1;
      const std::uint32_t native = session.metadata[model].units;
      if (name == "upper_ht1" || name == "upper_ht2" || name == "site_index") {
        output[row] = height_to_native(output[row], caller_units, native);
      } else if (name == "upper_d1" || name == "upper_d2") {
        output[row] = diameter_to_native(output[row], caller_units, native);
      } else if (name == "basal_area" && caller_units != static_cast<int>(native)) {
        const double imperial_to_metric = 0.09290304 / 0.40468564224;
        const double factor = caller_units == provider::units_imperial
          ? imperial_to_metric
          : 1.0 / imperial_to_metric;
        output[row] *= factor;
      }
    }
    storage.columns.push_back({
      storage.names.back().c_str(), provider::aux_double,
      output.data(), sizeof(double)
    });
  }
  return storage;
}

}  // namespace

// [[Rcpp::export]]
Rcpp::List mc_duplicate_map_cpp(
    Rcpp::List columns, Rcpp::IntegerVector defect_offsets,
    Rcpp::List defect_columns) {
  if (columns.size() == 0) return Rcpp::List::create();
  const R_xlen_t size = Rf_xlength(columns[0]);
  for (R_xlen_t column = 0; column < columns.size(); ++column) {
    if (Rf_xlength(columns[column]) != size) {
      Rcpp::stop("Duplicate-map stem columns have inconsistent sizes.");
    }
  }
  if (defect_offsets.size() != size + 1 || defect_offsets[0] != 0) {
    Rcpp::stop("Duplicate-map defect offsets are invalid.");
  }
  const int defect_count = defect_offsets[size];
  for (R_xlen_t column = 0; column < defect_columns.size(); ++column) {
    if (Rf_xlength(defect_columns[column]) != defect_count) {
      Rcpp::stop("Duplicate-map defect columns have inconsistent sizes.");
    }
  }
  if (size < 32) return Rcpp::List::create();

  const R_xlen_t sample_size = std::min<R_xlen_t>(size, 1024);
  std::unordered_map<std::uint64_t, int> sample;
  sample.reserve(static_cast<std::size_t>(sample_size));
  for (R_xlen_t row = 0; row < sample_size; ++row) {
    sample.emplace(stem_hash(columns, defect_offsets, defect_columns, row), 0);
  }
  if (sample.size() * 2 > static_cast<std::size_t>(sample_size)) {
    return Rcpp::List::create();
  }

  std::unordered_map<std::uint64_t, std::vector<R_xlen_t>> dictionary;
  dictionary.reserve(sample.size() * 2);
  std::vector<int> unique;
  unique.reserve(sample.size() * 2);
  Rcpp::IntegerVector map(size);
  for (R_xlen_t row = 0; row < size; ++row) {
    const std::uint64_t hash = stem_hash(
      columns, defect_offsets, defect_columns, row
    );
    std::vector<R_xlen_t>& candidates = dictionary[hash];
    int matched = -1;
    for (R_xlen_t representative : candidates) {
      if (stem_equal(
          columns, defect_offsets, defect_columns, row, representative
      )) {
        matched = map[representative] - 1;
        break;
      }
    }
    if (matched < 0) {
      matched = static_cast<int>(unique.size());
      unique.push_back(static_cast<int>(row));
      candidates.push_back(row);
    }
    map[row] = matched + 1;
  }
  if (unique.size() * 2 > static_cast<std::size_t>(size)) {
    return Rcpp::List::create();
  }
  Rcpp::IntegerVector unique_rows(unique.size());
  for (std::size_t index = 0; index < unique.size(); ++index) {
    unique_rows[index] = unique[index] + 1;
  }
  return Rcpp::List::create(
    Rcpp::_["unique"] = unique_rows, Rcpp::_["map"] = map
  );
}

// [[Rcpp::export]]
int mc_hash_raw_cpp(Rcpp::RawVector value) {
  double hash = 2166136261.0;
  constexpr double modulus = 2147483647.0;
  for (R_xlen_t index = 0; index < value.size(); ++index) {
    const int current = static_cast<int>(std::fmod(hash, modulus));
    const int mixed = current ^ static_cast<unsigned char>(value[index]);
    hash = std::fmod(static_cast<double>(mixed) * 16777619.0, modulus);
  }
  return static_cast<int>(hash);
}

// [[Rcpp::export]]
int mc_hash_character_cpp(Rcpp::CharacterVector value) {
  std::uint64_t hash = UINT64_C(1469598103934665603);
  for (R_xlen_t index = 0; index < value.size(); ++index) {
    SEXP item = value[index];
    if (item == NA_STRING) {
      hash = mix_hash(hash, UINT64_C(0xffffffffffffffff));
    } else {
      const char* text = Rf_translateCharUTF8(item);
      hash = mix_hash(hash, hash_bytes(text, std::strlen(text)));
    }
    hash = mix_hash(hash, static_cast<std::uint64_t>(index));
  }
  return static_cast<int>(hash % UINT64_C(2147483647));
}

// [[Rcpp::export]]
Rcpp::List mc_expand_index_cpp(Rcpp::IntegerVector unique_tree,
                               Rcpp::IntegerVector map) {
  int unique_count = 0;
  for (int value : map) unique_count = std::max(unique_count, value);
  std::vector<std::vector<int>> rows(static_cast<std::size_t>(unique_count));
  for (R_xlen_t row = 0; row < unique_tree.size(); ++row) {
    const int tree = unique_tree[row] - 1;
    if (tree < 0 || tree >= unique_count) {
      Rcpp::stop("Expansion tree index is out of range.");
    }
    rows[static_cast<std::size_t>(tree)].push_back(static_cast<int>(row));
  }
  std::size_t total = 0;
  for (int value : map) {
    if (value < 1 || value > unique_count) {
      Rcpp::stop("Expansion map index is out of range.");
    }
    total += rows[static_cast<std::size_t>(value - 1)].size();
  }
  Rcpp::IntegerVector source(total), tree(total);
  std::size_t at = 0;
  for (R_xlen_t original = 0; original < map.size(); ++original) {
    const std::vector<int>& selected = rows[
      static_cast<std::size_t>(map[original] - 1)
    ];
    for (int row : selected) {
      source[at] = row + 1;
      tree[at] = original + 1;
      ++at;
    }
  }
  return Rcpp::List::create(
    Rcpp::_["source"] = source, Rcpp::_["tree"] = tree
  );
}

// [[Rcpp::export]]
Rcpp::List mc_provider_open(Rcpp::CharacterVector model_ids) {
  std::unique_ptr<ProviderSession> session(new ProviderSession());
  ErrorStorage error;
  const int open_code = provider::snapshot_open(
    &session->snapshot, &session->generation, &error.view
  );
  if (open_code != provider::call_ok) {
    Rcpp::stop(error.message("snapshot_open", open_code));
  }
  const std::size_t n = model_ids.size();
  std::vector<std::string> id_storage(n);
  std::vector<const char*> ids(n);
  for (std::size_t i = 0; i < n; ++i) {
    if (model_ids[i] == NA_STRING) Rcpp::stop("model ids may not be missing.");
    id_storage[i] = Rcpp::as<std::string>(model_ids[i]);
    ids[i] = id_storage[i].c_str();
  }
  session->handles.resize(n);
  std::vector<std::int32_t> status(n, 0);
  const int resolve_code = provider::resolve_models(
    session->snapshot, ids.data(), n, session->handles.data(), status.data(),
    &error.view
  );
  if (resolve_code != provider::call_ok) {
    Rcpp::stop(error.message("resolve_models", resolve_code));
  }
  session->metadata.resize(n);
  Rcpp::IntegerVector units(n), kernel_type(n), has_dob(n), has_inverse(n),
    has_integral(n), model_status(n);
  Rcpp::NumericVector stump_height(n), bark_ratio(n);
  for (std::size_t i = 0; i < n; ++i) {
    model_status[i] = status[i];
    provider::ModelMetadata metadata{};
    metadata.struct_size = sizeof(provider::ModelMetadata);
    if (status[i] == 0 || status[i] == provider::status_species_out_of_scope) {
      const int metadata_code = provider::model_metadata(
        session->snapshot, session->handles[i], &metadata, &error.view
      );
      if (metadata_code != provider::call_ok) {
        Rcpp::stop(error.message("model_metadata", metadata_code));
      }
    }
    session->metadata[i] = metadata;
    units[i] = metadata.units;
    kernel_type[i] = metadata.kernel_type;
    has_dob[i] = metadata.has_dob;
    has_inverse[i] = metadata.has_inverse;
    has_integral[i] = metadata.has_integral;
    stump_height[i] = metadata.stump_height;
    bark_ratio[i] = metadata.bark_ratio;
  }
  const std::uint64_t generation = session->generation;
  Rcpp::XPtr<ProviderSession> pointer(session.release(), true);
  return Rcpp::List::create(
    Rcpp::_["pointer"] = pointer,
    Rcpp::_["status"] = model_status,
    Rcpp::_["units"] = units,
    Rcpp::_["kernel_type"] = kernel_type,
    Rcpp::_["has_dob"] = has_dob,
    Rcpp::_["has_inverse"] = has_inverse,
    Rcpp::_["has_integral"] = has_integral,
    Rcpp::_["stump_height"] = stump_height,
    Rcpp::_["bark_ratio"] = bark_ratio,
    Rcpp::_["generation"] = static_cast<double>(generation),
    Rcpp::_["nvel_source_revision"] = provider::nvel_source_revision()
  );
}

// [[Rcpp::export]]
Rcpp::List mc_provider_query(
    SEXP pointer, Rcpp::NumericVector dbh, Rcpp::NumericVector ht,
    Rcpp::IntegerVector model_index, Rcpp::List aux,
    Rcpp::IntegerVector tree_index, Rcpp::NumericVector height,
    int caller_units, bool need_dob = true, bool need_cum_ob = true) {
  const ProfileClock::time_point begin = ProfileClock::now();
  ProviderSession& session = session_from(pointer);
  const std::size_t n_tree = dbh.size();
  const std::size_t n_query = height.size();
  if (ht.size() != static_cast<R_xlen_t>(n_tree) ||
      model_index.size() != static_cast<R_xlen_t>(n_tree) ||
      tree_index.size() != static_cast<R_xlen_t>(n_query)) {
    Rcpp::stop("Provider query columns have inconsistent sizes.");
  }
  std::vector<double> native_dbh(n_tree), native_ht(n_tree), native_h(n_query);
  std::vector<provider::ModelHandle> handles(n_tree);
  for (std::size_t tree = 0; tree < n_tree; ++tree) {
    const int model = model_index[tree] - 1;
    if (model < 0 || static_cast<std::size_t>(model) >= session.handles.size()) {
      Rcpp::stop("model_index is out of range.");
    }
    handles[tree] = session.handles[model];
    native_dbh[tree] = diameter_to_native(dbh[tree], caller_units,
                                          session.metadata[model].units);
    native_ht[tree] = height_to_native(ht[tree], caller_units,
                                       session.metadata[model].units);
  }
  std::vector<std::uint64_t> query_tree(n_query);
  for (std::size_t query = 0; query < n_query; ++query) {
    const int tree = tree_index[query] - 1;
    if (tree < 0 || static_cast<std::size_t>(tree) >= n_tree) {
      Rcpp::stop("tree_index is out of range.");
    }
    query_tree[query] = static_cast<std::uint64_t>(tree);
    const int model = model_index[tree] - 1;
    native_h[query] = height_to_native(height[query], caller_units,
                                       session.metadata[model].units);
  }
  AuxStorage aux_storage = prepare_aux(
    aux, n_tree, model_index, session, caller_units
  );
  provider::AuxView aux_view{
    aux_storage.columns.data(), aux_storage.columns.size()
  };
  std::vector<double> dib(n_query), cum_ib(n_query);
  std::vector<double> dob(n_query, NA_REAL), cum_ob(n_query, NA_REAL);
  std::vector<std::int32_t> status(n_query);
  ErrorStorage error;
  const ProfileClock::time_point prepared = ProfileClock::now();
  const int code = provider::query_ragged(
    session.snapshot, n_tree, native_dbh.data(), native_ht.data(), handles.data(),
    aux_view, n_query, query_tree.data(), native_h.data(), dib.data(),
    need_dob ? dob.data() : nullptr, cum_ib.data(),
    need_cum_ob ? cum_ob.data() : nullptr, status.data(), &error.view
  );
  if (code != provider::call_ok) Rcpp::stop(error.message("query_ragged", code));
  const ProfileClock::time_point queried = ProfileClock::now();
  for (std::size_t query = 0; query < n_query; ++query) {
    const std::size_t tree = query_tree[query];
    const int model = model_index[tree] - 1;
    const std::uint32_t native = session.metadata[model].units;
    dib[query] = diameter_from_native(dib[query], caller_units, native);
    if (need_dob) {
      dob[query] = diameter_from_native(dob[query], caller_units, native);
    }
    cum_ib[query] = volume_from_native(cum_ib[query], caller_units, native);
    if (need_cum_ob) {
      cum_ob[query] = volume_from_native(cum_ob[query], caller_units, native);
    }
  }
  const ProfileClock::time_point converted = ProfileClock::now();
  return Rcpp::List::create(
    Rcpp::_["dib"] = dib, Rcpp::_["dob"] = dob,
    Rcpp::_["cum_ib"] = cum_ib, Rcpp::_["cum_ob"] = cum_ob,
    Rcpp::_["status"] = status,
    Rcpp::_["timing"] = Rcpp::NumericVector::create(
      Rcpp::_["input_conversion"] = profile_seconds(begin, prepared),
      Rcpp::_["provider_query"] = profile_seconds(prepared, queried),
      Rcpp::_["output_conversion"] = profile_seconds(queried, converted)
    )
  );
}

// [[Rcpp::export]]
Rcpp::List mc_provider_crossings(
    SEXP pointer, Rcpp::NumericVector dbh, Rcpp::NumericVector ht,
    Rcpp::IntegerVector model_index, Rcpp::List aux,
    Rcpp::NumericVector target, Rcpp::IntegerVector bark_basis,
    Rcpp::NumericVector lower, Rcpp::NumericVector upper, int caller_units) {
  const ProfileClock::time_point begin = ProfileClock::now();
  ProviderSession& session = session_from(pointer);
  const std::size_t n = dbh.size();
  if (ht.size() != static_cast<R_xlen_t>(n) ||
      model_index.size() != static_cast<R_xlen_t>(n) ||
      target.size() != static_cast<R_xlen_t>(n) ||
      bark_basis.size() != static_cast<R_xlen_t>(n) ||
      lower.size() != static_cast<R_xlen_t>(n) ||
      upper.size() != static_cast<R_xlen_t>(n)) {
    Rcpp::stop("Provider crossing columns have inconsistent sizes.");
  }
  std::vector<double> native_dbh(n), native_ht(n), native_target(n),
    native_lower(n), native_upper(n);
  std::vector<std::uint8_t> basis(n);
  std::vector<provider::ModelHandle> handles(n);
  for (std::size_t row = 0; row < n; ++row) {
    const int model = model_index[row] - 1;
    if (model < 0 || static_cast<std::size_t>(model) >= session.handles.size()) {
      Rcpp::stop("model_index is out of range.");
    }
    const std::uint32_t native = session.metadata[model].units;
    handles[row] = session.handles[model];
    native_dbh[row] = diameter_to_native(dbh[row], caller_units, native);
    native_ht[row] = height_to_native(ht[row], caller_units, native);
    native_target[row] = diameter_to_native(target[row], caller_units, native);
    native_lower[row] = height_to_native(lower[row], caller_units, native);
    native_upper[row] = height_to_native(upper[row], caller_units, native);
    basis[row] = bark_basis[row] == 1 ? provider::bark_outside : provider::bark_inside;
  }
  AuxStorage aux_storage = prepare_aux(aux, n, model_index, session, caller_units);
  provider::AuxView aux_view{aux_storage.columns.data(), aux_storage.columns.size()};
  std::vector<std::uint64_t> offsets(n + 1);
  std::vector<std::int32_t> status(n);
  std::size_t needed = 0;
  ErrorStorage error;
  const ProfileClock::time_point prepared = ProfileClock::now();
  int code = provider::all_crossings(
    session.snapshot, n, native_dbh.data(), native_ht.data(), handles.data(), aux_view,
    native_target.data(), basis.data(), native_lower.data(), native_upper.data(),
    offsets.data(), nullptr, 0, &needed, status.data(), &error.view
  );
  if (code != provider::call_ok && code != provider::call_buffer_too_small) {
    Rcpp::stop(error.message("all_crossings size query", code));
  }
  std::vector<double> roots(needed);
  std::fill(error.bytes.begin(), error.bytes.end(), '\0');
  error.view.used = 0;
  code = provider::all_crossings(
    session.snapshot, n, native_dbh.data(), native_ht.data(), handles.data(), aux_view,
    native_target.data(), basis.data(), native_lower.data(), native_upper.data(),
    offsets.data(), roots.data(), roots.size(), &needed, status.data(), &error.view
  );
  if (code != provider::call_ok) Rcpp::stop(error.message("all_crossings", code));
  const ProfileClock::time_point queried = ProfileClock::now();
  for (std::size_t row = 0; row < n; ++row) {
    const int model = model_index[row] - 1;
    for (std::size_t at = offsets[row]; at < offsets[row + 1]; ++at) {
      roots[at] = height_from_native(
        roots[at], caller_units, session.metadata[model].units
      );
    }
  }
  Rcpp::NumericVector offset_output(offsets.size());
  for (std::size_t i = 0; i < offsets.size(); ++i) {
    offset_output[i] = static_cast<double>(offsets[i]);
  }
  const ProfileClock::time_point converted = ProfileClock::now();
  return Rcpp::List::create(
    Rcpp::_["offsets"] = offset_output,
    Rcpp::_["roots"] = roots,
    Rcpp::_["status"] = status,
    Rcpp::_["timing"] = Rcpp::NumericVector::create(
      Rcpp::_["input_conversion"] = profile_seconds(begin, prepared),
      Rcpp::_["provider_crossings_two_pass"] =
        profile_seconds(prepared, queried),
      Rcpp::_["output_conversion"] = profile_seconds(queried, converted)
    )
  );
}

// [[Rcpp::export]]
Rcpp::List mc_provider_green_weight(
    Rcpp::NumericVector volume, Rcpp::IntegerVector spcd,
    Rcpp::IntegerVector bark_basis, int caller_units) {
  const std::size_t n = volume.size();
  if (spcd.size() != static_cast<R_xlen_t>(n) ||
      bark_basis.size() != static_cast<R_xlen_t>(n)) {
    Rcpp::stop("Provider weight columns have inconsistent sizes.");
  }
  std::vector<double> volume_ft3(n), weight(n);
  std::vector<std::int32_t> species(n), status(n);
  std::vector<std::uint8_t> basis(n), component(n, provider::component_stem),
    moisture(n, provider::moisture_green);
  for (std::size_t row = 0; row < n; ++row) {
    volume_ft3[row] = caller_units == provider::units_imperial
      ? volume[row]
      : volume[row] / 0.028316846592;
    species[row] = spcd[row];
    basis[row] = bark_basis[row] == 1
      ? provider::bark_outside
      : provider::bark_inside;
  }
  ErrorStorage error;
  const int code = provider::green_weight_batch(
    n, volume_ft3.data(), species.data(), basis.data(), component.data(),
    moisture.data(), weight.data(), status.data(), &error.view
  );
  if (code != provider::call_ok) {
    Rcpp::stop(error.message("green_weight_batch", code));
  }
  if (caller_units == provider::units_metric) {
    for (double& value : weight) value *= 0.45359237;
  }
  return Rcpp::List::create(
    Rcpp::_["value"] = weight,
    Rcpp::_["status"] = status
  );
}

// [[Rcpp::export]]
double mc_provider_generation(SEXP pointer) {
  ProviderSession& session = session_from(pointer);
  return static_cast<double>(provider::snapshot_generation(session.snapshot));
}

// [[Rcpp::export]]
void mc_provider_close(SEXP pointer) {
  Rcpp::XPtr<ProviderSession> session(pointer);
  if (session.get() != nullptr && session->snapshot != nullptr) {
    provider::snapshot_close(session->snapshot);
    session->snapshot = nullptr;
  }
}
