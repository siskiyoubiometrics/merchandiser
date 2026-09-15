## Derive changed scaling expectations with the unchanged 0.4.0 numerical routines.  The
## only changed input to a scale rule is nominal length rounded down to one foot.
library(merchandiser, lib.loc = "/tmp/merchandiser-interface2-old-lib")
recording <- readRDS("tests/interface-identity-0.4.0.rds")
ns <- asNamespace("merchandiser")
expected <- list()
for (key in names(recording$cascade)) {
  logs <- recording$cascade[[key]]$logs
  values <- logs$gross_scale
  for (r in seq_len(nrow(logs))) {
    z <- logs[r, ]
    tree <- recording$trees[match(z$id, recording$trees$tree), ]
    scaling_length <- floor(z$nominal_length)
    if (scaling_length == z$nominal_length)
      next
    if (z$volume_unit == "doyle") {
      diameter <- dib(tree$dbh, tree$ht, z$nominal_end_height, tree$model)
      values[r] <- max(diameter - 4, 0)^2 * scaling_length / 16
    }
    if (z$volume_unit == "international") {
      diameter <- floor(dib(tree$dbh, tree$ht, z$nominal_end_height, tree$model) + 0.5)
      values[r] <- get(".mc_intl14", ns)(diameter, scaling_length)
    }
    if (z$volume_unit == "scribner") {
      segments <- get(".mc_nvel_segments", ns)(scaling_length, "whole_40")
      diameters <- floor(dib(
        tree$dbh, tree$ht, z$start_height + cumsum(segments),
        tree$model
      ) +
        0.5)
      quantities <- numeric(length(segments))
      for (s in seq_along(segments)) {
        quantities[s] <- get(".mc_scribner", ns)(diameters[s], segments[s], TRUE)
      }
      values[r] <- sum(quantities)
    }
  }
  expected[[key]] <- values
}
fork_boundaries <- c(10, 26, 42, 58, 74, 90, 106)
fork_volume <- stem_volume(
  dbh = 24, ht = 120, model = "F00FW2W202", lower = 0,
  lower_type = "height", upper = fork_boundaries, upper_type = "height", status = TRUE
)
body_volume <- list()
for (key in names(recording$cascade)) {
  logs <- recording$cascade[[key]]$logs
  rows <- match(logs$id, recording$trees$tree)
  trees <- recording$trees[rows, ]
  body_volume[[key]] <- list()
  for (basis in c("inside", "outside")) {
    upper <- stem_volume(
      dbh = trees$dbh, ht = trees$ht, model = trees$model,
      lower = 0, lower_type = "height", upper = logs$nominal_end_height,
      upper_type = "height", bark = basis, status = TRUE
    )
    lower <- stem_volume(
      dbh = trees$dbh, ht = trees$ht, model = trees$model,
      lower = 0, lower_type = "height", upper = logs$start_height,
      upper_type = "height", bark = basis, status = TRUE
    )
    body_volume[[key]][[basis]] <- upper$value - lower$value
  }
}
recording$rulings <- list(
  scale = expected, fork_scale = diff(fork_volume$value), body_volume = body_volume
)
saveRDS(recording, "tests/interface-identity-0.4.0.rds", version = 2)
