
#include <Rcpp.h>
#include "stem_provider.h"

#include <cstdint>
#include <string>
#include <vector>

namespace provider = treevolume::stem_provider;

namespace {

struct SnapshotGuard {
  provider::Snapshot* value = nullptr;
  ~SnapshotGuard() {
    provider::snapshot_close(value);
  }
};

void check_call(int code, const char* operation,
                const provider::ErrorBuffer& error) {
  if (code == provider::call_ok) return;
  Rcpp::stop(
      std::string(operation) + " failed with code " +
      std::to_string(code) + ": " +
      (error.data == nullptr ? std::string() : std::string(error.data)));
}

}  // namespace

// [[Rcpp::export]]
Rcpp::List stem_provider_probe(
    Rcpp::CharacterVector ids, Rcpp::NumericVector dbh,
    Rcpp::NumericVector ht, Rcpp::IntegerVector tree_index,
    Rcpp::NumericVector height, Rcpp::NumericVector crossing_target,
    Rcpp::IntegerVector crossing_basis, Rcpp::NumericVector crossing_lower,
    Rcpp::NumericVector crossing_upper, Rcpp::NumericVector weight_volume,
    Rcpp::IntegerVector weight_spcd, Rcpp::IntegerVector weight_basis,
    Rcpp::IntegerVector weight_component,
    Rcpp::IntegerVector weight_moisture) {
  const std::size_t n_tree = static_cast<std::size_t>(ids.size());
  const std::size_t n_query = static_cast<std::size_t>(height.size());
  const std::size_t n_weight = static_cast<std::size_t>(weight_volume.size());
  if (dbh.size() != ids.size() || ht.size() != ids.size() ||
      crossing_target.size() != ids.size() ||
      crossing_basis.size() != ids.size() ||
      crossing_lower.size() != ids.size() ||
      crossing_upper.size() != ids.size() ||
      tree_index.size() != height.size() ||
      weight_spcd.size() != weight_volume.size() ||
      weight_basis.size() != weight_volume.size() ||
      weight_component.size() != weight_volume.size() ||
      weight_moisture.size() != weight_volume.size()) {
    Rcpp::stop("probe inputs have incompatible lengths");
  }

  char message[512] = {};
  provider::ErrorBuffer error{message, sizeof(message), 0};
  SnapshotGuard snapshot;
  std::uint64_t generation = 0;
  check_call(
      provider::snapshot_open(&snapshot.value, &generation, &error),
      "snapshot_open", error);

  std::vector<std::string> id_storage(n_tree);
  std::vector<const char*> id_pointer(n_tree);
  for (std::size_t index = 0; index < n_tree; ++index) {
    id_storage[index] = Rcpp::as<std::string>(ids[index]);
    id_pointer[index] = id_storage[index].c_str();
  }
  std::vector<provider::ModelHandle> handles(n_tree);
  Rcpp::IntegerVector resolve_status(n_tree);
  check_call(
      provider::resolve_models(
          snapshot.value, id_pointer.data(), n_tree, handles.data(),
          reinterpret_cast<std::int32_t*>(resolve_status.begin()), &error),
      "resolve_models", error);

  Rcpp::IntegerVector metadata_kernel(n_tree);
  Rcpp::IntegerVector metadata_units(n_tree);
  Rcpp::IntegerVector metadata_verified(n_tree);
  Rcpp::IntegerVector metadata_has_dob(n_tree);
  Rcpp::IntegerVector metadata_has_inverse(n_tree);
  Rcpp::IntegerVector metadata_has_integral(n_tree);
  Rcpp::NumericVector metadata_stump(n_tree);
  Rcpp::NumericVector metadata_bark(n_tree);
  Rcpp::CharacterVector metadata_id(n_tree);
  Rcpp::CharacterVector metadata_key(n_tree);
  Rcpp::LogicalVector r_kernel(n_tree);
  for (std::size_t index = 0; index < n_tree; ++index) {
    provider::ModelMetadata metadata{};
    check_call(
        provider::model_metadata(
            snapshot.value, handles[index], &metadata, &error),
        "model_metadata", error);
    metadata_kernel[index] = metadata.kernel_type;
    metadata_units[index] = metadata.units;
    metadata_verified[index] = metadata.oracle_verified;
    metadata_has_dob[index] = metadata.has_dob;
    metadata_has_inverse[index] = metadata.has_inverse;
    metadata_has_integral[index] = metadata.has_integral;
    metadata_stump[index] = metadata.stump_height;
    metadata_bark[index] = metadata.bark_ratio;
    metadata_id[index] = metadata.id;
    metadata_key[index] = metadata.kernel_key == nullptr
        ? NA_STRING : Rcpp::String(metadata.kernel_key);
    r_kernel[index] = provider::is_r_kernel(handles[index]);
  }

  std::vector<std::uint64_t> query_tree(n_query);
  for (std::size_t index = 0; index < n_query; ++index) {
    query_tree[index] = static_cast<std::uint64_t>(tree_index[index]);
  }
  Rcpp::NumericVector query_dib(n_query);
  Rcpp::NumericVector query_dob(n_query);
  Rcpp::NumericVector query_cum_ib(n_query);
  Rcpp::NumericVector query_cum_ob(n_query);
  Rcpp::IntegerVector query_status(n_query);
  const provider::AuxView no_aux{nullptr, 0};
  check_call(
      provider::query_ragged(
          snapshot.value, n_tree, dbh.begin(), ht.begin(), handles.data(),
          no_aux, n_query, query_tree.data(), height.begin(),
          query_dib.begin(), query_dob.begin(), query_cum_ib.begin(),
          query_cum_ob.begin(),
          reinterpret_cast<std::int32_t*>(query_status.begin()), &error),
      "query_ragged", error);

  std::vector<std::uint8_t> crossing_basis_native(n_tree);
  for (std::size_t index = 0; index < n_tree; ++index) {
    crossing_basis_native[index] =
        static_cast<std::uint8_t>(crossing_basis[index]);
  }
  std::vector<std::uint64_t> offsets(n_tree + 1);
  Rcpp::IntegerVector crossing_status(n_tree);
  std::size_t roots_needed = 0;
  const int short_code = provider::all_crossings(
      snapshot.value, n_tree, dbh.begin(), ht.begin(), handles.data(), no_aux,
      crossing_target.begin(), crossing_basis_native.data(),
      crossing_lower.begin(), crossing_upper.begin(), offsets.data(), nullptr,
      0, &roots_needed,
      reinterpret_cast<std::int32_t*>(crossing_status.begin()), &error);
  if (short_code != provider::call_buffer_too_small && roots_needed != 0) {
    check_call(short_code, "all_crossings size query", error);
  }
  Rcpp::NumericVector roots(roots_needed);
  check_call(
      provider::all_crossings(
          snapshot.value, n_tree, dbh.begin(), ht.begin(), handles.data(),
          no_aux, crossing_target.begin(), crossing_basis_native.data(),
          crossing_lower.begin(), crossing_upper.begin(), offsets.data(),
          roots.begin(), roots_needed, &roots_needed,
          reinterpret_cast<std::int32_t*>(crossing_status.begin()), &error),
      "all_crossings", error);
  Rcpp::NumericVector offsets_r(n_tree + 1);
  for (std::size_t index = 0; index <= n_tree; ++index) {
    offsets_r[index] = static_cast<double>(offsets[index]);
  }

  std::vector<std::int32_t> weight_spcd_native(n_weight);
  std::vector<std::uint8_t> weight_basis_native(n_weight);
  std::vector<std::uint8_t> weight_component_native(n_weight);
  std::vector<std::uint8_t> weight_moisture_native(n_weight);
  for (std::size_t index = 0; index < n_weight; ++index) {
    weight_spcd_native[index] = weight_spcd[index];
    weight_basis_native[index] =
        static_cast<std::uint8_t>(weight_basis[index]);
    weight_component_native[index] =
        static_cast<std::uint8_t>(weight_component[index]);
    weight_moisture_native[index] =
        static_cast<std::uint8_t>(weight_moisture[index]);
  }
  Rcpp::NumericVector weight(n_weight);
  Rcpp::IntegerVector weight_status(n_weight);
  check_call(
      provider::green_weight_batch(
          n_weight, weight_volume.begin(), weight_spcd_native.data(),
          weight_basis_native.data(), weight_component_native.data(),
          weight_moisture_native.data(), weight.begin(),
          reinterpret_cast<std::int32_t*>(weight_status.begin()), &error),
      "green_weight_batch", error);

  return Rcpp::List::create(
      Rcpp::Named("nvel_revision") = provider::nvel_source_revision(),
      Rcpp::Named("generation") = static_cast<double>(generation),
      Rcpp::Named("runtime_generation") =
          static_cast<double>(provider::snapshot_generation(snapshot.value)),
      Rcpp::Named("resolve_status") = resolve_status,
      Rcpp::Named("r_kernel") = r_kernel,
      Rcpp::Named("metadata") = Rcpp::DataFrame::create(
          Rcpp::Named("id") = metadata_id,
          Rcpp::Named("key") = metadata_key,
          Rcpp::Named("kernel") = metadata_kernel,
          Rcpp::Named("units") = metadata_units,
          Rcpp::Named("oracle_verified") = metadata_verified,
          Rcpp::Named("has_dob") = metadata_has_dob,
          Rcpp::Named("has_inverse") = metadata_has_inverse,
          Rcpp::Named("has_integral") = metadata_has_integral,
          Rcpp::Named("stump_height") = metadata_stump,
          Rcpp::Named("bark_ratio") = metadata_bark),
      Rcpp::Named("query") = Rcpp::DataFrame::create(
          Rcpp::Named("dib") = query_dib,
          Rcpp::Named("dob") = query_dob,
          Rcpp::Named("cum_ib") = query_cum_ib,
          Rcpp::Named("cum_ob") = query_cum_ob,
          Rcpp::Named("status") = query_status),
      Rcpp::Named("crossings") = Rcpp::List::create(
          Rcpp::Named("short_code") = short_code,
          Rcpp::Named("offsets") = offsets_r,
          Rcpp::Named("roots") = roots,
          Rcpp::Named("status") = crossing_status),
      Rcpp::Named("green_weight") = weight,
      Rcpp::Named("green_weight_status") = weight_status);
}
