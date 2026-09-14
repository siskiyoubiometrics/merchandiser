.mc_oracle_columns <- c(
  "ROW_ID", "CALL_KIND", "BUILD", "REGN", "VOLEQ", "ERRFLAG", "NOLOGP",
  "VOL10",
  paste0("LOGLEN", 1:20), paste0("LOGDIA_", 2:21, "_1"),
  paste0("LOGDIA_", 2:21, "_2"), paste0("LOGVOL_1_", 1:20)
)

.mc_oracle_products <- function() {
  products(
    .mc_legacy_product(
      "scribner_decimal", 1L, lengths = 16, min_sed = 0,
      diameter_basis = "ib", scale_rule = "scribner_decimal_c_split_20",
      measurement_quantity = "board_foot", scale_unit = "board_foot",
      scale_bark_basis = "ib", allow_lower_products = FALSE
    ),
    .mc_legacy_product(
      "scribner_factor", 2L, lengths = 16, min_sed = 0,
      diameter_basis = "ib", scale_rule = "scribner_factor_split_20",
      measurement_quantity = "board_foot", scale_unit = "board_foot",
      scale_bark_basis = "ib", allow_lower_products = FALSE
    ),
    .mc_legacy_product(
      "international", 3L, lengths = 16, min_sed = 0,
      diameter_basis = "ib", scale_rule = "international_1_4_4ft",
      measurement_quantity = "board_foot", scale_unit = "board_foot",
      scale_bark_basis = "ib", allow_lower_products = FALSE
    )
  )
}

.mc_oracle_native_scale <- function(products, product, diameter, length) {
  if (length(product) == 1L) {
    product <- rep.int(as.integer(product), length(length))
  }
  if (length(product) != length(length)) {
    stop("Native oracle product indices have an inconsistent size.",
         call. = FALSE)
  }
  mc_nvel_log_scale_cpp(
    .mc_cpp_products(products, NULL), as.integer(product),
    as.double(diameter), as.double(length)
  )
}

.mc_oracle_manifest <- function(root, expected_files) {
  root <- normalizePath(root, mustWork = FALSE)
  manifest_root <- if (basename(root) == "full") dirname(root) else root
  manifest_path <- file.path(manifest_root, "MANIFEST.csv")
  if (!file.exists(manifest_path)) {
    stop("Oracle fixture manifest is missing: ", manifest_path, call. = FALSE)
  }
  manifest <- utils::read.csv(
    manifest_path, colClasses = c("character", "integer", "character"),
    stringsAsFactors = FALSE
  )
  selected <- manifest[match(file.path("full", expected_files), manifest$file), ]
  if (anyNA(selected$file) || !identical(basename(selected$file), expected_files)) {
    stop("Oracle fixture manifest does not list every required full fixture.",
         call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required to verify oracle SHA-256 checksums.",
         call. = FALSE)
  }
  paths <- file.path(manifest_root, selected$file)
  if (any(!file.exists(paths))) {
    stop(
      "Oracle fixture inventory is incomplete. Missing: ",
      paste(basename(paths[!file.exists(paths)]), collapse = ", "),
      call. = FALSE
    )
  }
  observed <- vapply(paths, function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1L))
  mismatch <- observed != selected$sha256
  list(
    table = selected, paths = paths, checksum_mismatch = mismatch,
    observed_checksum = observed
  )
}

.mc_oracle_empty_metrics <- function(family, precision) {
  list(
    family = family, precision = precision, source_rows_scanned = 0,
    rows_compared = 0, source_logs = 0, included_logs = 0,
    scribner_compared = 0, scribner_source_logs = 0,
    scribner_within_tolerance = 0, international_compared = 0,
    international_within_tolerance = 0, excluded_not_volumelibrary = 0,
    excluded_oracle_error = 0, excluded_no_log_table = 0,
    excluded_missing_log_fields = 0, excluded_f32_unpaired_logs = 0,
    excluded_f32_incomplete_logs = 0,
    excluded_scribner_non_split_logs = 0,
    excluded_international_family = 0,
    excluded_international_incomplete = 0, invalid_build_rows = 0
  )
}

.mc_oracle_add <- function(metrics, name, value) {
  metrics[[name]] <- metrics[[name]] + as.double(value)
  metrics
}

.mc_oracle_process_logs <- function(chunk, family, products, metrics) {
  volume_call <- !is.na(chunk$CALL_KIND) &
    chunk$CALL_KIND == "VOLUMELIBRARY"
  successful <- volume_call & !is.na(chunk$ERRFLAG) & chunk$ERRFLAG == 0
  has_logs <- successful & is.finite(chunk$NOLOGP) & chunk$NOLOGP > 0
  metrics <- .mc_oracle_add(
    metrics, "excluded_not_volumelibrary", sum(!volume_call)
  )
  metrics <- .mc_oracle_add(
    metrics, "excluded_oracle_error", sum(volume_call & !successful)
  )
  metrics <- .mc_oracle_add(
    metrics, "excluded_no_log_table", sum(successful & !has_logs)
  )
  if (!any(has_logs)) return(metrics)

  rows <- chunk[has_logs, , drop = FALSE]
  count <- pmin(as.integer(rows$NOLOGP), 20L)
  lengths <- as.matrix(rows[paste0("LOGLEN", 1:20)])
  diameters <- as.matrix(rows[paste0("LOGDIA_", 2:21, "_1")])
  observed <- as.matrix(rows[paste0("LOGVOL_1_", 1:20)])
  slot <- matrix(
    rep(seq_len(20L), each = nrow(rows)), nrow = nrow(rows)
  ) <= count
  valid <- slot & is.finite(lengths) & lengths > 0 &
    is.finite(diameters) & diameters > 0 & is.finite(observed)
  source_logs <- sum(count)
  included_logs <- sum(valid)
  metrics <- .mc_oracle_add(metrics, "rows_compared", nrow(rows))
  metrics <- .mc_oracle_add(metrics, "source_logs", source_logs)
  metrics <- .mc_oracle_add(metrics, "included_logs", included_logs)
  metrics <- .mc_oracle_add(
    metrics, "excluded_missing_log_fields", source_logs - included_logs
  )

  if (identical(family, "clark_r9")) {
    metrics <- .mc_oracle_add(
      metrics, "excluded_scribner_non_split_logs", included_logs
    )
  }
  if (identical(family, "flewelling_3pt")) {
    special <- substr(rows$VOLEQ, 4L, 6L) %in% c("F32", "F33")
    ordinary <- valid & !special
    if (any(ordinary)) {
      expected <- .mc_oracle_native_scale(
        products, 1L, diameters[ordinary], lengths[ordinary]
      )
      actual <- observed[ordinary]
      metrics <- .mc_oracle_add(
        metrics, "scribner_compared", length(expected)
      )
      metrics <- .mc_oracle_add(
        metrics, "scribner_source_logs", length(expected)
      )
      metrics <- .mc_oracle_add(
        metrics, "scribner_within_tolerance", sum(actual == expected)
      )
    }
    for (top in seq.int(2L, 20L, by = 2L)) {
      expected_pair <- special & count >= top
      raw <- rows[[paste0("LOGDIA_", top + 1L, "_2")]]
      usable <- expected_pair & valid[, top - 1L] & valid[, top] &
        is.finite(raw)
      incomplete <- expected_pair & !usable
      metrics <- .mc_oracle_add(
        metrics, "excluded_f32_incomplete_logs",
        sum(valid[incomplete, top - 1L]) + sum(valid[incomplete, top])
      )
      if (any(usable)) {
        pair_length <- lengths[usable, top - 1L] + lengths[usable, top]
        expected <- .mc_oracle_native_scale(
          products, 1L, trunc(raw[usable]), pair_length
        )
        actual <- observed[usable, top - 1L] + observed[usable, top]
        metrics <- .mc_oracle_add(
          metrics, "scribner_compared", length(expected)
        )
        metrics <- .mc_oracle_add(
          metrics, "scribner_source_logs", 2L * length(expected)
        )
        metrics <- .mc_oracle_add(
          metrics, "scribner_within_tolerance", sum(actual == expected)
        )
      }
    }
    odd <- which(special & count %% 2L == 1L)
    unpaired <- if (length(odd)) valid[cbind(odd, count[odd])] else logical()
    metrics <- .mc_oracle_add(
      metrics, "excluded_f32_unpaired_logs", sum(unpaired)
    )
  } else if (included_logs && !identical(family, "clark_r9")) {
    product <- if (identical(family, "flewelling_2pt")) {
      regions <- rep(rows$REGN, times = 20L)[valid]
      ifelse(regions == 6L, 2L, 1L)
    } else {
      2L
    }
    expected <- .mc_oracle_native_scale(
      products, product, diameters[valid], lengths[valid]
    )
    actual <- observed[valid]
    metrics <- .mc_oracle_add(
      metrics, "scribner_compared", length(expected)
    )
    metrics <- .mc_oracle_add(
      metrics, "scribner_source_logs", length(expected)
    )
    metrics <- .mc_oracle_add(
      metrics, "scribner_within_tolerance", sum(actual == expected)
    )
  }

  if (startsWith(family, "clark_")) {
    metrics <- .mc_oracle_add(
      metrics, "excluded_international_family", nrow(rows)
    )
  } else {
    complete <- rowSums(valid) == count & is.finite(rows$VOL10)
    metrics <- .mc_oracle_add(
      metrics, "excluded_international_incomplete", sum(!complete)
    )
    if (any(complete)) {
      indices <- which(complete)
      actual <- numeric(length(indices))
      expected <- numeric(length(indices))
      for (at in seq_along(indices)) {
        row <- indices[at]
        logs <- seq_len(count[row])
        expected[at] <- sum(.mc_oracle_native_scale(
          products, 3L, diameters[row, logs], lengths[row, logs]
        ))
        actual[at] <- rows$VOL10[row]
      }
      metrics <- .mc_oracle_add(
        metrics, "international_compared", length(expected)
      )
      metrics <- .mc_oracle_add(
        metrics, "international_within_tolerance", sum(actual == expected)
      )
    }
  }
  metrics
}

.mc_oracle_stream_file <- function(path, family, precision, products,
                                   chunk_size = 50000L) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The data.table package is required for full oracle streaming.",
         call. = FALSE)
  }
  connection <- gzfile(path, "rt")
  on.exit(close(connection), add = TRUE)
  header <- readLines(connection, n = 1L, warn = FALSE)
  metrics <- .mc_oracle_empty_metrics(family, precision)
  repeat {
    lines <- readLines(connection, n = chunk_size, warn = FALSE)
    if (!length(lines)) break
    chunk <- data.table::fread(
      text = paste(c(header, lines), collapse = "\n"),
      select = .mc_oracle_columns, na.strings = c("", "NA", "NaN", "-nan"),
      showProgress = FALSE, data.table = FALSE
    )
    metrics <- .mc_oracle_add(
      metrics, "source_rows_scanned", nrow(chunk)
    )
    metrics <- .mc_oracle_add(
      metrics, "invalid_build_rows",
      sum(is.na(chunk$BUILD) | chunk$BUILD != precision)
    )
    metrics <- .mc_oracle_process_logs(chunk, family, products, metrics)
  }
  as.data.frame(metrics, stringsAsFactors = FALSE)
}

mc_full_oracle_agreement <- function(root) {
  families <- c("flewelling_2pt", "flewelling_3pt", "clark_r8", "clark_r9")
  precisions <- c("single", "double")
  expected_files <- as.vector(vapply(families, function(family) {
    paste0(family, ".", precisions, ".csv.gz")
  }, character(length(precisions))))
  manifest <- .mc_oracle_manifest(root, expected_files)
  if (any(manifest$checksum_mismatch)) {
    mismatch <- expected_files[manifest$checksum_mismatch]
    stop(paste(
      "Oracle fixture checksum mismatch:",
      paste(mismatch, collapse = ", ")
    ), call. = FALSE)
  }
  expected_revision <- "38548071d5aa652bb90c7f111f86b427f798a1c9"
  observed_revision <- unname(as.character(merchandiser::nvel_source_revision()))
  if (!identical(observed_revision, expected_revision)) {
    stop("The installed treevolume NVEL source revision does not match the fixture pin.",
         call. = FALSE)
  }
  products <- .mc_oracle_products()
  output <- vector("list", length(expected_files))
  for (at in seq_along(expected_files)) {
    name <- expected_files[at]
    precision <- sub(".*[.]([^.]+)[.]csv[.]gz$", "\\1", name)
    family <- sub("[.](single|double)[.]csv[.]gz$", "", name)
    output[[at]] <- .mc_oracle_stream_file(
      manifest$paths[at], family, precision, products
    )
  }
  result <- do.call(rbind, output)
  result$manifest_rows <- manifest$table$row_count
  result$tolerance <- "0 (exact)"
  rownames(result) <- NULL
  result
}

.mc_oracle_public_case <- function(root) {
  root <- normalizePath(root, mustWork = TRUE)
  full <- if (basename(root) == "full") root else file.path(root, "full")
  path <- file.path(full, "flewelling_2pt.double.csv.gz")
  connection <- gzfile(path, "rt")
  on.exit(close(connection), add = TRUE)
  header <- readLines(connection, n = 1L, warn = FALSE)
  columns <- c(
    "ROW_ID", "CALL_KIND", "REGN", "FIASPCD", "VOLEQ", "DBHOB", "HTTOT",
    "MTOPP", "MTOPS", "STUMP", "MRULEMOD", "ERRFLAG", "NOLOGP",
    "LOGLEN1", "BOLHT2", "LOGVOL_1_1", "VOL10"
  )
  repeat {
    lines <- readLines(connection, n = 5000L, warn = FALSE)
    if (!length(lines)) break
    chunk <- data.table::fread(
      text = paste(c(header, lines), collapse = "\n"), select = columns,
      showProgress = FALSE, data.table = FALSE
    )
    selected <- with(chunk,
      CALL_KIND == "VOLUMELIBRARY" & REGN == 1L & MRULEMOD == "N" &
        ERRFLAG == 0L & NOLOGP == 1 & is.finite(LOGLEN1) & LOGLEN1 > 0 &
        is.finite(BOLHT2) & is.finite(LOGVOL_1_1) & is.finite(VOL10)
    )
    if (any(selected)) return(chunk[which(selected)[1L], , drop = FALSE])
  }
  stop("No deterministic public-path oracle case was found.", call. = FALSE)
}
