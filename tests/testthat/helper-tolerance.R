# MAINTAINER-OWNED. Changes to tolerances need maintainer review.
#
# This file defines the tolerance table used by differential tests. The table
# and differential harness are owned by the maintainer through CODEOWNERS. A
# pull request that touches them is rejected by CI unless the maintainer
# opened it.
#
# All values remain placeholders until independent fixtures establish measured
# agreement for a complete implementation. Discrete outputs compare exactly.

mc_tolerance <- list(
  diameter_double_rel = 2e-4,       # PLACEHOLDER
  cubic_volume_double_rel = 1e-12,  # PLACEHOLDER
  weight_double_rel = 1e-6,         # PLACEHOLDER
  diameter_single_rel = 3e-4,       # PLACEHOLDER
  cubic_volume_single_rel = 2e-3,   # PLACEHOLDER
  weight_single_rel = 1e-4,         # PLACEHOLDER
  discrete_exact = 0                # PLACEHOLDER
)
