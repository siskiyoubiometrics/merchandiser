#ifndef MERCHANDISER_STEM_PROVIDER_HPP
#define MERCHANDISER_STEM_PROVIDER_HPP

#include <R_ext/Rdynload.h>

#include <cstddef>
#include <cstdint>

namespace treevolume {
namespace stem_provider {

inline constexpr std::uint32_t R_KERNEL_HANDLE_MASK = UINT32_C(0x80000000);

enum CallStatus : int {
  call_ok = 0,
  call_invalid_argument = 1,
  call_buffer_too_small = 2,
  call_internal_error = 3,
  call_generation_mismatch = 4
};

enum TreeStatus : std::int32_t {
  status_ok = 0,
  status_na_input = 1,
  status_dbh_nonpositive = 2,
  status_ht_nonpositive = 3,
  status_height_out_of_range = 4,
  status_diameter_nonpositive = 5,
  status_empty_bounds = 6,
  status_unknown_species = 7,
  status_unknown_model = 50,
  status_missing_input = 51,
  status_species_out_of_scope = 52,
  status_capability_missing = 53,
  status_kernel_error = 54,
  status_above_tip = 100,
  status_below_stump = 101,
  status_not_unique = 102,
  status_no_convergence = 103,
  status_library_error_base = 300
};

enum AuxType : std::uint32_t {
  aux_double = 1U,
  aux_int32 = 2U,
  aux_uint8 = 3U
};

enum BarkBasis : std::uint8_t {
  bark_inside = 0U,
  bark_outside = 1U
};

enum WeightComponent : std::uint8_t {
  component_stem = 0U,
  component_wood = 1U,
  component_bark = 2U
};

enum MoistureBasis : std::uint8_t {
  moisture_green = 0U,
  moisture_dry = 1U
};

enum UnitSystem : std::uint32_t {
  units_imperial = 1U,
  units_metric = 2U
};

enum KernelType : std::uint32_t {
  kernel_compiled = 1U,
  kernel_r = 2U
};

enum VerificationState : std::uint32_t {
  verification_not_applicable = 0U,
  verification_false = 1U,
  verification_true = 2U
};

struct ErrorBuffer {
  char* data;
  std::size_t capacity;
  std::size_t used;
};

struct AuxColumn {
  const char* name;
  std::uint32_t type;
  const void* data;
  std::size_t stride;
};

struct AuxView {
  const AuxColumn* columns;
  std::size_t size;
};

struct ModelMetadata {
  std::uint32_t struct_size;
  std::uint32_t kernel_type;
  std::uint32_t units;
  std::uint32_t oracle_verified;
  std::uint32_t has_dob;
  std::uint32_t has_inverse;
  std::uint32_t has_integral;
  double stump_height;
  double bark_ratio;
  const char* id;
  const char* kernel_key;
};

struct Snapshot;
using ModelHandle = std::uint32_t;


inline bool is_r_kernel(ModelHandle handle) noexcept {
  return (handle & R_KERNEL_HANDLE_MASK) != 0U;
}


const char* nvel_source_revision() noexcept;

int snapshot_open(Snapshot** out, std::uint64_t* generation,
                         ErrorBuffer* error) noexcept;

void snapshot_close(Snapshot* snapshot) noexcept;

int resolve_models(const Snapshot* snapshot, const char* const* ids,
                          std::size_t n, ModelHandle* out,
                          std::int32_t* status,
                          ErrorBuffer* error) noexcept;

int query_ragged(
    const Snapshot* snapshot, std::size_t n_tree, const double* dbh,
    const double* ht, const ModelHandle* model, AuxView aux,
    std::size_t n_query, const std::uint64_t* tree_index, const double* height,
    double* dib, double* dob, double* cum_ib_ground, double* cum_ob_ground,
    std::int32_t* status, ErrorBuffer* error) noexcept;

int all_crossings(
    const Snapshot* snapshot, std::size_t n_tree, const double* dbh,
    const double* ht, const ModelHandle* model, AuxView aux,
    const double* target, const std::uint8_t* bark_basis, const double* lower,
    const double* upper, std::uint64_t* offsets, double* roots,
    std::size_t roots_capacity, std::size_t* roots_needed,
    std::int32_t* status, ErrorBuffer* error) noexcept;

int green_weight_batch(
    std::size_t n, const double* volume, const std::int32_t* spcd,
    const std::uint8_t* volume_basis, const std::uint8_t* component,
    const std::uint8_t* moisture, double* weight, std::int32_t* status,
    ErrorBuffer* error) noexcept;

std::uint64_t snapshot_generation(const Snapshot* snapshot) noexcept;

int model_metadata(const Snapshot* snapshot, ModelHandle model,
                          ModelMetadata* out,
                          ErrorBuffer* error) noexcept;

}  // namespace stem_provider
}  // namespace treevolume

#endif
