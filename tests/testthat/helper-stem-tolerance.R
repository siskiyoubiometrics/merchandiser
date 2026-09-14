# MAINTAINER-OWNED. Changes to tolerances need maintainer review.
#
# This file defines the tolerance table used by the differential (oracle)
# tests, per IMPLEMENTATION_PLAN.md section 3.3 and 3.4: "The tolerance
# table and the differential test harness are owned by the maintainer
# (CODEOWNERS). A pull request that touches them is rejected by CI unless
# opened by the maintainer." See CODEOWNERS at the repository root.
#
# The Flewelling diameter, height, and volume values below are the
# maintainer-approved tolerances used by the differential tests. They were
# derived by measuring maximum relative differences across the full double-
# and single-precision oracle fixtures, then rounding upward to leave a small
# margin for platform variation. Tolerances for families not yet represented
# by full fixtures remain marked as placeholders. Discrete outputs compare
# exactly, subject to the boundary-case handling in section 3.3.

tv_tolerance <- list(
  # Continuous outputs, relative tolerance against the double-precision
  # oracle build.
  diameter_double_rel   = 2e-4,
  height_double_rel     = 4e-5,
  volume_double_rel     = 1e-12,
  biomass_double_rel    = 1e-6,  # PLACEHOLDER

  # Continuous outputs, relative tolerance against the single-precision
  # oracle build (looser: a precision effect, not a bug, per section 3.3).
  diameter_single_rel   = 3e-4,
  height_single_rel     = 2e-3,
  volume_single_rel     = 2e-3,
  biomass_single_rel    = 1e-4,  # PLACEHOLDER

  # Discrete outputs (board feet, log counts, log lengths, Decimal C)
  # compare exactly against the double build; single-build boundary cases
  # are counted and reported, not treated as failures (section 3.3).
  discrete_exact        = 0      # PLACEHOLDER, exact match required
)
