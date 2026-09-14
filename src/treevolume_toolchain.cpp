#include <Rcpp.h>
#include <treevolume/kernels.hpp>

// This probe is intentionally compiled into the package. It proves that
// LinkingTo exposes treevolume/kernels.hpp and that a kernel entry point can
// be evaluated from merchandiser's compiled code.

// [[Rcpp::export]]
double treevolume_toolchain_check() {
  const treevolume::Kernel* kernel =
      treevolume::find_kernel("demo_paraboloid");
  return kernel->dib(10.0, 80.0, 4.5, 0.0, 1.0);
}
