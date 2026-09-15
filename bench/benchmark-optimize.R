# Run with Rscript bench/benchmark-optimize.R 100000.
# Timing includes public input preparation, bucking, scaling, and result assembly.
args <- commandArgs(trailingOnly = TRUE)
stems <- if (length(args)) as.integer(args[1]) else 100000L
if (is.na(stems) || stems < 1L) stop("The first argument must be a positive stem count.")
library(merchandiser)
invisible(threads(8))
cpu <- readLines("/proc/cpuinfo")
cpu <- sub("^[^:]+: *", "", cpu[startsWith(cpu, "model name")][1])
cat("CPU:", cpu, "\nThreads:", threads(), "\nR:", R.version.string, "\n")
cat("Workload: synthetic Douglas-fir, dbh 18 to 22 inches, height 76 to 84 feet.\n")
cat("Profiles repeat in a 45-tree cycle. Public caching is included.\n")
cat("Each timed call processes up to 1000 trees. No max_logs limits or defects.\n")
rows <- list()
for (count in c(3L, 10L)) {
  p <- vector("list", count)
  for (i in seq_len(count)) {
    p[[i]] <- product(paste0("product_", i), min_length = 8, max_length = 20,
                      trim = 0.5, min_sed = i - 1, volume_unit = "cubic", price = i)
  }
  p <- do.call(products, p)
  for (strategy in c("cascade", "optimize")) {
    gc()
    elapsed <- system.time({
      for (first in seq.int(1L, stems, by = 1000L)) {
        ids <- seq.int(first, min(stems, first + 999L))
        result <- merchandise(ids, 18 + ids %% 5L, 76 + ids %% 9L, 202, p,
                              model = "F00FW2W202", strategy = strategy, quiet = TRUE)
        if (nrow(result$status)) stop("Benchmark tree returned a nonzero status.")
      }
    })[["elapsed"]]
    row <- data.frame(stems = stems, products = count, strategy = strategy, threads = threads(),
                       seconds = elapsed, stems_per_second = stems / elapsed,
                       million_seconds = elapsed * 1000000 / stems)
    print(row, row.names = FALSE, digits = 10)
    rows[[length(rows) + 1L]] <- row
  }
}
results <- do.call(rbind, rows)
write.csv(results, Sys.getenv("MERCH_BENCH_CSV", "/tmp/benchmark-optimize.csv"), row.names = FALSE)
