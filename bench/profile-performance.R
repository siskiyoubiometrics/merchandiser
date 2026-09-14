script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_argument[[1L]]))
package_root <- dirname(dirname(script_path))

positive_integer <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, default)))
  if (is.na(value) || value < 1L) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  value
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

pkgload::load_all(package_root, quiet = TRUE)
stem_count <- positive_integer("MERCH_PROFILE_STEMS", "100000")
chunk_size <- positive_integer("MERCH_PROFILE_CHUNK", "10000")
thread_count <- positive_integer("MERCH_PROFILE_THREADS", "8")
output_path <- Sys.getenv(
  "MERCH_PROFILE_OUTPUT", "/tmp/mc-performance-profile.rds"
)
rprof_path <- Sys.getenv(
  "MERCH_RPROF_OUTPUT", "/tmp/mc-performance-profile.out"
)

native <- new.env(parent = emptyenv())
native$elapsed <- numeric()
native$calls <- integer()
namespace <- asNamespace("merchandiser")
instrument <- function(name) {
  original <- get(name, envir = namespace, inherits = FALSE)
  replacement <- local({
    key <- name
    target <- original
    function(...) {
      started <- proc.time()[["elapsed"]]
      answer <- target(...)
      elapsed <- proc.time()[["elapsed"]] - started
      native$elapsed[key] <- ifelse(
        is.na(native$elapsed[key]), elapsed, native$elapsed[key] + elapsed
      )
      native$calls[key] <- ifelse(
        is.na(native$calls[key]), 1L, native$calls[key] + 1L
      )
      timing <- answer$timing
      if (!is.null(timing)) {
        for (part in names(timing)) {
          full <- paste(key, part, sep = ":")
          native$elapsed[full] <- ifelse(
            is.na(native$elapsed[full]), timing[[part]],
            native$elapsed[full] + timing[[part]]
          )
          native$calls[full] <- ifelse(
            is.na(native$calls[full]), 1L, native$calls[full] + 1L
          )
        }
      }
      answer
    }
  })
  assignInNamespace(name, replacement, ns = "merchandiser")
}
for (name in c("mc_provider_crossings", "mc_provider_query", "mc_buck_cpp")) {
  instrument(name)
}

products <- benchmark_products()
processed <- 0L
log_count <- 0L
unique_stems <- identical(Sys.getenv("MERCH_PROFILE_UNIQUE"), "true")
gc()
Rprof(rprof_path, interval = 0.01, memory.profiling = TRUE,
      line.profiling = TRUE)
started <- proc.time()[["elapsed"]]
merchandiser::with_threads(thread_count, {
  while (processed < stem_count) {
    size <- min(chunk_size, stem_count - processed)
    sequence <- processed + seq_len(size)
    model <- ifelse(sequence %% 2L == 0L, "200FW2W108", "900CLKE001")
    benchmark_dbh <- if (unique_stems) {
      18 + sequence / stem_count
    } else {
      18 + (sequence %% 5L)
    }
    result <- merchandise(
      dbh = benchmark_dbh, ht = 76 + (sequence %% 9L),
      model = model, products = products, id = sequence,
      stump_ht = 1, utilization_height = 37, currency = "USD",
      status = TRUE
    )
    log_count <- log_count + nrow(result$logs)
    processed <- processed + size
  }
})
elapsed <- proc.time()[["elapsed"]] - started
Rprof(NULL)
profile <- summaryRprof(rprof_path, memory = "both", lines = "show")
native_table <- data.frame(
  operation = names(native$elapsed),
  calls = as.integer(native$calls[names(native$elapsed)]),
  elapsed_seconds = as.double(native$elapsed),
  stringsAsFactors = FALSE
)
native_table <- native_table[
  order(native_table$elapsed_seconds, decreasing = TRUE), , drop = FALSE
]
result <- list(
  trees = stem_count, chunk_size = chunk_size, threads = thread_count,
  elapsed_seconds = elapsed, logs = log_count,
  rprof_path = rprof_path, by_self = profile$by.self,
  by_total = profile$by.total, native = native_table
)
saveRDS(result, output_path)
cat("elapsed_seconds:", elapsed, "\n")
cat("logs:", log_count, "\n")
cat("\nRprof by self:\n")
print(utils::head(profile$by.self, 15L))
cat("\nRprof by total:\n")
print(utils::head(profile$by.total, 15L))
cat("\nNative timing:\n")
print(native_table, row.names = FALSE)
