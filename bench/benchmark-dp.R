script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
package_root <- dirname(dirname(script_path))

load_merchandiser <- function() {
  if (file.exists(file.path(package_root, "DESCRIPTION"))) {
    pkgload::load_all(package_root, quiet = TRUE)
  } else {
    library(merchandiser)
  }
}

positive_integer <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, default)))
  if (is.na(value) || value < 1L) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  value
}

benchmark_algorithms <- function() {
  values <- strsplit(
    Sys.getenv("MERCH_BENCH_ALGORITHMS", "cascade,dp"), ",", fixed = TRUE
  )[[1L]]
  values <- trimws(values)
  if (!length(values) || any(!values %in% c("cascade", "dp")) ||
      anyDuplicated(values)) {
    stop(
      "MERCH_BENCH_ALGORITHMS must contain cascade, dp, or both.",
      call. = FALSE
    )
  }
  values
}

benchmark_thread_set <- function() {
  values <- strsplit(
    Sys.getenv("MERCH_BENCH_THREAD_SET", "1,4,8"), ",", fixed = TRUE
  )[[1L]]
  values <- suppressWarnings(as.integer(trimws(values)))
  if (!length(values) || anyNA(values) || any(values < 1L) ||
      anyDuplicated(values)) {
    stop(
      "MERCH_BENCH_THREAD_SET must contain unique positive integers.",
      call. = FALSE
    )
  }
  values
}

peak_rss_mb <- function() {
  path <- "/proc/self/status"
  if (!file.exists(path)) return(NA_real_)
  line <- grep("^VmHWM:", readLines(path, warn = FALSE), value = TRUE)
  if (!length(line)) return(NA_real_)
  as.double(gsub("[^0-9]", "", line[[1L]])) / 1024
}

benchmark_products <- function() {
  make <- function(product, priority, lengths, max_logs_per_segment, allow_lower_products,
                   price) {
    product(
      product, priority, lengths = as.double(lengths), trim = 0, min_sed = 0,
      max_logs_per_segment = max_logs_per_segment, allow_lower_products = allow_lower_products,
      diameter_basis = "ib", scale_rule = "cubic",
      measurement_quantity = "cubic", scale_unit = "ft3",
      scale_bark_basis = "ib", price = price
    )
  }
  products(
    make("primary", 1L, c(16, 18, 20), 1L, TRUE, 100),
    make("secondary", 2L, c(8, 10, 12), 1L, TRUE, 80),
    make("pulp", 3L, c(4, 6), NA_integer_, FALSE, 30)
  )
}

run_worker <- function() {
  load_merchandiser()
  stem_count <- positive_integer("MERCH_BENCH_STEMS", "1000000")
  chunk_size <- positive_integer(
    "MERCH_BENCH_CHUNK", as.character(stem_count)
  )
  thread_count <- positive_integer("MERCH_BENCH_THREADS", "1")
  algorithm <- Sys.getenv("MERCH_BENCH_ALGORITHM", "cascade")
  if (!algorithm %in% c("cascade", "dp")) {
    stop("MERCH_BENCH_ALGORITHM must be cascade or dp.", call. = FALSE)
  }
  output_path <- Sys.getenv("MERCH_BENCH_OUTPUT")
  if (!nzchar(output_path)) {
    stop("MERCH_BENCH_OUTPUT is required in worker mode.", call. = FALSE)
  }
  products <- benchmark_products()
  processed <- 0L
  log_count <- 0L
  product_count <- setNames(integer(nrow(products)), products$product)
  failure_count <- 0L
  gc()
  elapsed <- merchandiser::with_threads(thread_count, {
    started <- proc.time()[["elapsed"]]
    while (processed < stem_count) {
      size <- min(chunk_size, stem_count - processed)
      sequence <- processed + seq_len(size)
      model <- ifelse(sequence %% 2L == 0L, "200FW2W108", "900CLKE001")
      buck <- if (algorithm == "cascade") merchandise else optimize_bucking
      result <- buck(
        dbh = 18 + (sequence %% 5L), ht = 76 + (sequence %% 9L),
        model = model, products = products, id = sequence,
        stump_ht = 1, utilization_height = 37, currency = "USD",
        status = TRUE
      )
      log_count <- log_count + nrow(result$logs)
      product_count <- product_count + tabulate(
        match(result$logs$product, names(product_count)),
        nbins = length(product_count)
      )
      failure_count <- failure_count + sum(
        !result$trees$status %in% c(0L, 410L)
      )
      processed <- processed + size
    }
    proc.time()[["elapsed"]] - started
  })
  row <- data.frame(
    trees = stem_count, products = 3L,
    models = "Flewelling 200FW2W108 and Clark 900CLKE001",
    algorithm = algorithm, threads = thread_count,
    elapsed_seconds = elapsed, stems_per_second = stem_count / elapsed,
    extrapolated_100_million_seconds = elapsed * 100000000 / stem_count,
    peak_rss_mb = peak_rss_mb(), logs = log_count,
    primary_pieces = unname(product_count[["primary"]]),
    secondary_pieces = unname(product_count[["secondary"]]),
    pulp_pieces = unname(product_count[["pulp"]]),
    failed_trees = failure_count, stringsAsFactors = FALSE
  )
  saveRDS(row, output_path)
  invisible(row)
}

hardware_table <- function() {
  cpu <- if (file.exists("/proc/cpuinfo")) {
    lines <- grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)
    if (length(lines)) sub("^[^:]+: *", "", lines[[1L]]) else NA_character_
  } else {
    NA_character_
  }
  memory <- if (file.exists("/proc/meminfo")) {
    line <- grep("^MemTotal:", readLines("/proc/meminfo"), value = TRUE)
    if (length(line)) {
      sprintf("%.1f GiB", as.double(gsub("[^0-9]", "", line[[1L]])) /
        1024^2)
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }
  data.frame(
    field = c(
      "sysname", "release", "machine", "cpu_model", "logical_cores",
      "memory", "R_version"
    ),
    value = c(
      Sys.info()[["sysname"]], Sys.info()[["release"]],
      Sys.info()[["machine"]], cpu,
      as.character(parallel::detectCores(logical = TRUE)), memory,
      R.version.string
    ),
    stringsAsFactors = FALSE
  )
}

run_parent <- function() {
  stem_count <- positive_integer("MERCH_BENCH_STEMS", "1000000")
  chunk_size <- positive_integer(
    "MERCH_BENCH_CHUNK", as.character(stem_count)
  )
  algorithms <- benchmark_algorithms()
  threads <- benchmark_thread_set()
  settings <- expand.grid(
    algorithm = algorithms, threads = threads, stringsAsFactors = FALSE
  )
  output_paths <- paste0(
    tempfile("merch-benchmark-", fileext = ".rds"), "-",
    settings$algorithm, "-", settings$threads
  )
  on.exit(unlink(output_paths), add = TRUE)
  results <- lapply(seq_along(output_paths), function(index) {
    thread_count <- settings$threads[index]
    algorithm <- settings$algorithm[index]
    status <- system2(
      file.path(R.home("bin"), "Rscript"), script_path,
      env = c(
        "MERCH_BENCH_WORKER=true",
        paste0("MERCH_BENCH_STEMS=", stem_count),
        paste0("MERCH_BENCH_CHUNK=", chunk_size),
        paste0("MERCH_BENCH_THREADS=", thread_count),
        paste0("MERCHANDISER_THREADS=", thread_count),
        paste0("MERCH_BENCH_ALGORITHM=", algorithm),
        paste0("MERCH_BENCH_OUTPUT=", output_paths[index])
      )
    )
    if (!identical(status, 0L) || !file.exists(output_paths[index])) {
      stop(
        "Benchmark worker failed for ", algorithm, " at ", thread_count,
        " threads.", call. = FALSE
      )
    }
    row <- readRDS(output_paths[index])
    cat("completed setting:", algorithm, "at", thread_count, "threads\n")
    row
  })
  hardware <- hardware_table()
  results <- do.call(rbind, results)
  print(hardware, row.names = FALSE)
  cat("workload: public cascade and dynamic program, one million trees by ",
      "default, three products, mixed Flewelling and Clark models\n", sep = "")
  cat("chunk_size:", chunk_size, "\n")
  print(results, row.names = FALSE)
  report_path <- Sys.getenv("MERCH_BENCH_REPORT")
  if (nzchar(report_path)) {
    saveRDS(list(
      hardware = hardware, trees = stem_count, chunk_size = chunk_size,
      results = results
    ), report_path)
  }
}

if (identical(Sys.getenv("MERCH_BENCH_WORKER"), "true")) {
  run_worker()
} else {
  run_parent()
}
