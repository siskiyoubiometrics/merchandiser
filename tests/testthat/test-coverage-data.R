# Sources for all dimensions and names: shipped data/*.rda fixtures and
# R/example-data.R. data-raw/example-trees.R constructs 6 example trees,
# 300 PNW trees, 400 southern trees, every seventh PNW tree's defect (42),
# and 15 measurements on each example tree (6 * 15 = 90).
coverage_schemas <- list(
  example_trees = list(rows = 6L, columns = c("tree_id", "spcd", "dbh", "ht", "model")),
  example_trees_pnw = list(rows = 300L, columns = c(
    "stand", "plot", "tree_id", "spcd", "dbh", "ht", "ht_status", "age", "longitude", "latitude"
  )),
  example_trees_south = list(rows = 400L, columns = c(
    "stand", "plot", "tree_id", "spcd", "dbh", "ht", "age", "saw_stop", "pulp_stop", "jump_butt",
    "longitude", "latitude"
  )),
  example_defects_pnw = list(rows = 42L, columns = c(
    "tree_id", "start_height", "end_height", "effect", "product", "percent"
  )),
  example_stem_measurements = list(rows = 90L, columns = c(
    "tree_id", "spcd", "dbh", "ht", "h", "dib"
  )),
  # Source: data/default_taper_models.rda contains 460 species-specific rows.
  default_taper_models = list(rows = 460L, columns = c("spcd", "model", "source"))
)
for (name in names(coverage_schemas)) {
  test_that(paste("coverage shipped schema", name), {
    schema <- coverage_schemas[[name]]
    data <- get(name)
    expect_identical(dim(data), c(schema$rows, length(schema$columns)))
    expect_identical(names(data), schema$columns)
  })
}

test_that("coverage every shipped mapped defect belongs to its tree list", {
  expect_gt(nrow(example_defects_pnw), 0)
  expect_true(all(example_defects_pnw$tree_id %in% example_trees_pnw$tree_id))
})
