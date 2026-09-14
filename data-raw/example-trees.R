# Synthetic data generator for the regional examples
#
# IMPORTANT: The two working tree lists made here are synthetic.
# The required FIA DataMart attempt failed while receiving RI_TREE.csv.
# The alternative datasets::Loblolly data loaded successfully, but it has
# height and age only. It has no DBH and cannot support taper or bucking calls.
#
# Published height equation used for both synthetic stands:
# Height follows 4.5 + P2 * exp(-P3 * diameter^P4).
# DBH is in inches and HT is in feet. This is the Curtis-Arney form used by
# the USDA Forest Service Forest Vegetation Simulator.
#
# Pacific Northwest coefficients are from the FVS Pacific Northwest Coast
# Variant Overview, equation 4.1.1 and table 4.1.1a. We use the Siuslaw
# location column. Douglas-fir uses P2 = 407.1595, P3 = 7.2885, P4 = -0.5908.
# Western hemlock uses P2 = 1196.619, P3 = 5.7904, P4 = -0.2906.
# https://www.fs.usda.gov/fmsc/ftp/fvs/docs/overviews/FVSpn_Overview.pdf
#
# Loblolly coefficients are from the FVS Southern Variant Overview,
# equation 4.1.1 and table 4.1.1. Loblolly pine uses P2 = 243.860648,
# P3 = 4.28460566, and P4 = -0.47130185.
# https://www.fs.usda.gov/sites/default/files/forest-management/fvs-sn-overview.pdf
#
# The equation form is attributed there to Curtis (1967) and Arney (1985).
# Curtis, R.O. 1967. Height-diameter and height-diameter-age equations for
# second-growth Douglas-fir. Forest Science 13(4):365-375.
# https://doi.org/10.1093/forestscience/13.4.365

# Draw from a distribution while keeping every observation in working bounds.
# This is true rejection sampling, so the accepted values follow the named
# distribution truncated to the interval rather than being piled at a limit.
draw_truncated <- function(n, draw, lower, upper) {
  answer <- numeric()
  while (length(answer) < n) {
    candidate <- draw(max(100L, n - length(answer)))
    answer <- c(answer, candidate[candidate >= lower & candidate <= upper])
  }
  answer[seq_len(n)]
}

# Generate the two regional example stands.
#
# All arguments not tied to a published FVS coefficient are operating
# assumptions. They are exposed here so a user can replace them directly.
#
# @param pnw_n Number of Pacific Northwest tree records. Default 300.
# @param south_n Number of loblolly records. Default 400.
# @param seed Random seed used for reproducible output.
# @param pnw_dbh_shape Assumed Weibull shape for a 60-year natural stand.
# @param pnw_dbh_scale Assumed Weibull scale in inches.
# @param pnw_dbh_bounds Assumed truncation limits in inches.
# @param pnw_species_probability Assumed Douglas-fir and hemlock proportions.
# @param pnw_age_mean Assumed mean breast-height age in years.
# @param pnw_age_sd Assumed between-tree age standard deviation in years.
# @param pnw_age_bounds Assumed truncated age limits in years.
# @param pnw_height_sd Assumed individual height residual SD in feet.
# @param pnw_minimum_height Assumed lower height bound in feet.
# @param pnw_pruned_probability Assumed fraction recorded as pruned.
# @param south_age Assumed plantation age in years.
# @param south_dbh_mean Assumed loblolly mean DBH in inches at age 25.
# @param south_dbh_sd Assumed loblolly DBH standard deviation in inches.
# @param south_dbh_bounds Assumed loblolly DBH truncation limits in inches.
# @param south_height_sd Assumed individual height residual SD in feet.
# @param south_minimum_height Assumed lower height bound in feet.
# @param south_pruned_probability Assumed fraction recorded as pruned.
# @param saw_stop_fraction Required fraction with a saw stopper.
# @param pulp_stop_fraction Required fraction with a pulp stopper.
# @param saw_stop_height_fraction Assumed saw-stop fraction of total height.
# @param pulp_stop_height_fraction Assumed pulp-stop fraction of total height.
# @param jump_butt_height Assumed jump-butt height in feet.
# @return A named list containing pnw and south data frames.
generate_walkthrough_stands <- function(
    pnw_n = 300L,
    south_n = 400L,
    seed = 20260908L,
    pnw_dbh_shape = 2.4,
    pnw_dbh_scale = 17,
    pnw_dbh_bounds = c(5, 40),
    pnw_species_probability = c(`202` = 0.75, `263` = 0.25),
    pnw_age_mean = 60,
    pnw_age_sd = 6,
    pnw_age_bounds = c(45, 80),
    pnw_height_sd = 5,
    pnw_minimum_height = 20,
    pnw_pruned_probability = 0.08,
    south_age = 25L,
    south_dbh_mean = 9,
    south_dbh_sd = 1.8,
    south_dbh_bounds = c(4.5, 16),
    south_height_sd = 4,
    south_minimum_height = 20,
    south_pruned_probability = 0.70,
    saw_stop_fraction = 0.15,
    pulp_stop_fraction = 0.08,
    saw_stop_height_fraction = 0.58,
    pulp_stop_height_fraction = 0.80,
    jump_butt_height = 3.5) {
  set.seed(seed)

  pnw_spcd <- sample(
    as.integer(names(pnw_species_probability)),
    size = pnw_n,
    replace = TRUE,
    prob = unname(pnw_species_probability)
  )
  pnw_dbh <- draw_truncated(
    pnw_n,
    function(n) stats::rweibull(n, shape = pnw_dbh_shape, scale = pnw_dbh_scale),
    pnw_dbh_bounds[1],
    pnw_dbh_bounds[2]
  )
  pnw_age <- round(draw_truncated(
    pnw_n,
    function(n) stats::rnorm(n, mean = pnw_age_mean, sd = pnw_age_sd),
    pnw_age_bounds[1],
    pnw_age_bounds[2]
  ))
  pnw_coefficients <- data.frame(
    spcd = c(202L, 263L),
    p2 = c(407.1595, 1196.619),
    p3 = c(7.2885, 5.7904),
    p4 = c(-0.5908, -0.2906)
  )
  pnw_match <- match(pnw_spcd, pnw_coefficients$spcd)
  pnw_height_mean <- 4.5 + pnw_coefficients$p2[pnw_match] * exp(
    -pnw_coefficients$p3[pnw_match] *
      pnw_dbh^pnw_coefficients$p4[pnw_match]
  )
  pnw_height_simulated <- pmax(
    pnw_minimum_height,
    pnw_height_mean + stats::rnorm(pnw_n, mean = 0, sd = pnw_height_sd)
  )
  measured <- seq_len(pnw_n) %% 3L == 1L
  pnw_ht_observed <- ifelse(measured, pnw_height_simulated, NA_real_)
  pnw <- data.frame(
    stand = "pnw_west",
    plot = sprintf("PNW-%02d", rep(seq_len(10L), length.out = pnw_n)),
    id = sprintf("example-pnw-%03d", seq_len(pnw_n)),
    spcd = pnw_spcd,
    species = ifelse(pnw_spcd == 202L, "Douglas-fir", "western hemlock"),
    dbh = round(pnw_dbh, 2),
    ht_observed = round(pnw_ht_observed, 1),
    ht_simulated = round(pnw_height_simulated, 1),
    age = as.integer(pnw_age),
    pruned = stats::rbinom(pnw_n, 1L, pnw_pruned_probability) == 1L,
    region = 6L,
    forest = 12L,
    district = 0L,
    longitude = -122.25,
    latitude = 45.73,
    expansion_factor = rep(c(0.8, 1.2), length.out = pnw_n),
    stringsAsFactors = FALSE
  )

  south_dbh <- draw_truncated(
    south_n,
    function(n) stats::rnorm(n, mean = south_dbh_mean, sd = south_dbh_sd),
    south_dbh_bounds[1],
    south_dbh_bounds[2]
  )
  south_height_mean <- 4.5 + 243.860648 * exp(
    -4.28460566 * south_dbh^-0.47130185
  )
  south_height <- pmax(
    south_minimum_height,
    south_height_mean + stats::rnorm(south_n, mean = 0, sd = south_height_sd)
  )

  saw_count <- as.integer(round(south_n * saw_stop_fraction))
  pulp_count <- as.integer(round(south_n * pulp_stop_fraction))
  available <- sample(seq_len(south_n))
  saw_index <- available[seq_len(saw_count)]
  pulp_index <- available[saw_count + seq_len(pulp_count)]
  jump_index <- available[saw_count + pulp_count + 1L]
  saw_stop <- rep(NA_real_, south_n)
  pulp_stop <- rep(NA_real_, south_n)
  jump_butt <- rep(NA_real_, south_n)
  saw_stop[saw_index] <- round(
    saw_stop_height_fraction * south_height[saw_index],
    1
  )
  pulp_stop[pulp_index] <- round(
    pulp_stop_height_fraction * south_height[pulp_index],
    1
  )
  jump_butt[jump_index] <- jump_butt_height

  south <- data.frame(
    stand = "south_loblolly",
    plot = sprintf("SOUTH-%02d", rep(seq_len(10L), length.out = south_n)),
    id = sprintf("example-south-%03d", seq_len(south_n)),
    spcd = 131L,
    species = "loblolly pine",
    dbh = round(south_dbh, 2),
    ht_observed = round(south_height, 1),
    ht_simulated = round(south_height, 1),
    age = as.integer(south_age),
    pruned = stats::rbinom(south_n, 1L, south_pruned_probability) == 1L,
    saw_stop = saw_stop,
    pulp_stop = pulp_stop,
    jump_butt = jump_butt,
    region = 8L,
    forest = 8L,
    district = 0L,
    longitude = -84.40,
    latitude = 33.70,
    expansion_factor = rep(c(0.8, 1.2), length.out = south_n),
    stringsAsFactors = FALSE
  )

  list(pnw = pnw, south = south)
}

library(dplyr)

generated <- generate_walkthrough_stands()
example_trees_pnw <- generated$pnw |>
  mutate(tree = row_number()) |>
  select(-id) |>
  relocate(tree, .after = plot)
example_trees_south <- generated$south |>
  mutate(tree = row_number()) |>
  select(-id) |>
  relocate(tree, .after = plot)
example_trees <- data.frame(
  tree = 1:6,
  spcd = c(202, 263, 202, 263, 202, 263),
  species = rep(c("Douglas-fir", "western hemlock"), 3),
  dbh = c(12, 14, 16, 18, 20, 24),
  ht = c(60, 70, 80, 90, 100, 120),
  model = c("F00FW2W202", "F03FW2W263")[rep(1:2, 3)]
)
save(example_trees_pnw, file = "data/example_trees_pnw.rda", version = 2)
save(example_trees_south, file = "data/example_trees_south.rda", version = 2)
save(example_trees, file = "data/example_trees.rda", version = 2)

# Modeled upper-stem measurements for fitting examples, not field observations.
library(merchandiser)
example_stem_measurements <- example_trees |>
  cross_join(data.frame(relative_height = seq(0.05, 0.95, length.out = 15))) |>
  mutate(h = ht * relative_height,
         dib = dib(dbh = dbh, ht = ht, h = h, model = model)) |>
  select(tree, species, dbh, ht, h, dib)
save(example_stem_measurements, file = "data/example_stem_measurements.rda", version = 2)
