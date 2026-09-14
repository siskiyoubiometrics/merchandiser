.mc_prepare_merch_call <- function(dbh, ht, model, products, id, spcd, age,
                                   pruned, defects, stump_ht, utilization_top,
                                   utilization_top_basis, utilization_height,
                                   curvature_scale, scaling, currency,
                                   algorithm, objective, unpriced, dots,
                                   units, status, model_status = 0L,
                                   assumptions = NULL) {
  units <- .mc_scalar_choice(units, "units", c("imperial", "metric"))
  algorithm <- .mc_scalar_choice(
    algorithm, "algorithm", c("cascade", "dp")
  )
  objective <- .mc_scalar_choice(
    objective, "objective", c("auto", "value", "net_cubic_ib")
  )
  unpriced <- .mc_scalar_choice(
    unpriced, "unpriced", c("zero", "exclude", "error")
  )
  utilization_top_basis <- .mc_scalar_choice(
    utilization_top_basis, "utilization_top_basis", c("ib", "ob")
  )
  status <- .mc_as_logical(status, "status", allow_na = FALSE)
  if (length(status) != 1L) {
    stop(
      "status must be one TRUE/FALSE or 0/1 value. ",
      "Supply one flag and try again.", call. = FALSE
    )
  }
  invalid_dots <- is.null(names(dots)) || any(!nzchar(names(dots))) ||
    anyDuplicated(names(dots))
  if (length(dots) && invalid_dots) {
    stop(
      "Model auxiliary arguments must be uniquely named. ",
      "Name every value in ... once and try again.", call. = FALSE
    )
  }
  if (!is.null(id)) .mc_require_atomic_id(id)
  if (is.null(id) && !is.null(defects) && nrow(defects)) {
    stop(
      "id is required when a keyed defect table is supplied. ",
      "Pass the tree ids used in defects and try again.", call. = FALSE
    )
  }
  optional <- Filter(Negate(is.null), list(
    id = id, spcd = spcd, age = age, pruned = pruned, stump_ht = stump_ht,
    utilization_top = utilization_top, utilization_height = utilization_height
  ))
  values <- c(list(dbh = dbh, ht = ht, model = model), optional, dots)
  size <- .mc_common_size(values)
  values <- Map(function(value, name) .mc_recycle(value, size, name), values, names(values))
  values$dbh <- .mc_as_double(values$dbh, "dbh")
  values$ht <- .mc_as_double(values$ht, "ht")
  if (!(is.character(values$model) || is.factor(values$model))) {
    stop(
      "model must contain registered taper model ids. ",
      "Supply model ids or omit model and supply species.", call. = FALSE
    )
  }
  values$model <- as.character(values$model)
  if (is.null(id)) values$id <- seq_len(size)
  .mc_require_atomic_id(values$id)
  if (anyDuplicated(values$id)) {
    stop("id must be unique. Give each tree a different id and try again.", call. = FALSE)
  }
  if (!is.null(values$spcd)) {
    values$spcd <- .mc_resolve_species(values$spcd, "spcd")
  }
  for (name in intersect(c(
    "age", "stump_ht", "utilization_top", "utilization_height"
  ), names(values))) {
    values[[name]] <- .mc_as_double(values[[name]], name)
  }
  if (!is.null(values$pruned)) {
    values$pruned <- .mc_as_logical(values$pruned, "pruned")
  }
  if (is.factor(currency)) currency <- as.character(currency)
  invalid_currency <- !is.character(currency) || length(currency) != 1L ||
    is.na(currency) || !nzchar(currency)
  if (!is.null(currency) && invalid_currency) {
    stop(
      "currency must be NULL or one nonempty character value. ",
      "Supply a label such as 'USD' and try again.", call. = FALSE
    )
  }
  products <- .validate_products_units(products, units)
  original_products <- products
  effective_objective <- if (objective == "auto") {
    if (any(!is.na(products$price))) "value" else "net_cubic_ib"
  } else {
    objective
  }
  if (algorithm == "dp" && effective_objective == "value") {
    if (unpriced == "exclude") {
      products <- .mc_subset_products(products, !is.na(products$price))
      if (!nrow(products)) {
        stop(
          "No products remain after excluding unpriced products. ",
          "Add a product price or use unpriced = 'zero'.", call. = FALSE
        )
      }
    } else if (unpriced == "error" && anyNA(products$price)) {
      stop(
        "Every remaining product must be priced when unpriced is error. ",
        "Add prices or choose unpriced = 'zero' or 'exclude'.",
        call. = FALSE
      )
    }
  }
  if (any(!is.na(products$price)) && is.null(currency)) {
    stop(
      "currency is required when any product has a price. ",
      "Supply a label such as currency = 'USD'.", call. = FALSE
    )
  }
  default_stump <- if (units == "imperial") 1 else 0.3
  stump <- if (is.null(values$stump_ht)) rep(default_stump, size) else
    as.double(values$stump_ht)
  bad_stump <- !is.na(stump) & (!is.finite(stump) | stump < 0 |
                                  (!is.na(values$ht) & stump >= values$ht))
  if (any(bad_stump)) {
    stop(
      "stump_ht must satisfy 0 <= stump_ht < ht. ",
      "Correct the stump heights and try again.", call. = FALSE
    )
  }
  if (!is.null(values$utilization_height) && any(
    !is.na(values$utilization_height) &
      (!is.finite(values$utilization_height) | values$utilization_height < 0)
  )) {
    stop(
      "utilization_height must contain finite nonnegative values or NA. ",
      "Correct the height cap and try again.", call. = FALSE
    )
  }
  if (!is.null(values$utilization_top) && any(
    !is.na(values$utilization_top) &
      (!is.finite(values$utilization_top) | values$utilization_top < 0)
  )) {
    stop(
      "utilization_top must contain finite nonnegative values or NA. ",
      "Correct the top diameter and try again.", call. = FALSE
    )
  }
  aux_names <- setdiff(names(values), c(
    "dbh", "ht", "model", "id", "spcd", "age", "pruned", "stump_ht",
    "utilization_top", "utilization_height"
  ))
  model_status <- as.integer(.mc_recycle(model_status, size, "model_status"))
  assumptions <- .mc_bind_assumptions(
    assumptions,
    if (is.null(values$stump_ht)) .mc_assumption_rows(
      "stump_height", value = default_stump, units = units,
      source = "package_default"
    ),
    .mc_assumption_rows("unit_system", units = units, source = "call")
  )
  list(
    size = size, dbh = values$dbh, ht = values$ht, model = values$model,
    id = values$id, spcd = values$spcd, age = values$age,
    pruned = values$pruned, stump = stump,
    utilization_top = values$utilization_top,
    utilization_top_basis = utilization_top_basis,
    utilization_height = values$utilization_height,
    curvature_scale = curvature_scale, scaling = .mc_validate_scaling(scaling),
    currency = currency, algorithm = algorithm, objective = objective,
    effective_objective = effective_objective, unpriced = unpriced,
    aux = values[aux_names], units = units, include_status = status,
    model_status = model_status, assumptions = assumptions,
    utilization_top_defaulted = is.null(values$utilization_top),
    products = products, original_products = original_products,
    defects = defects
  )
}

.mc_static_product <- function(products, row, tree, call, entry = TRUE) {
  missing <- FALSE
  eligible <- TRUE
  codes <- attr(products, "species_codes")[[row]]
  if (products$species[row] != "all") {
    if (is.null(call$spcd) || is.na(call$spcd[tree])) missing <- TRUE
    else eligible <- eligible && as.integer(call$spcd[tree]) %in% codes
  }
  if (entry) {
    eligible <- eligible && call$dbh[tree] >= products$min_dbh[row] &&
      (is.na(products$max_dbh[row]) || call$dbh[tree] < products$max_dbh[row])
  }
  if (!is.na(products$min_age[row]) || !is.na(products$max_age[row])) {
    if (is.null(call$age) || is.na(call$age[tree])) missing <- TRUE
    else eligible <- eligible &&
      (is.na(products$min_age[row]) || call$age[tree] >= products$min_age[row]) &&
      (is.na(products$max_age[row]) || call$age[tree] < products$max_age[row])
  }
  if (!is.na(products$pruned[row])) {
    if (is.null(call$pruned) || is.na(call$pruned[tree])) missing <- TRUE
    else eligible <- eligible && identical(call$pruned[tree], products$pruned[row])
  }
  list(eligible = isTRUE(eligible) && !missing, missing = missing && isTRUE(eligible))
}

.mc_static_product_vector <- function(products, row, call, entry = TRUE) {
  missing <- rep(FALSE, call$size)
  eligible <- rep(TRUE, call$size)
  codes <- attr(products, "species_codes")[[row]]
  if (products$species[row] != "all") {
    if (is.null(call$spcd)) {
      missing[] <- TRUE
    } else {
      absent <- is.na(call$spcd)
      missing[absent] <- TRUE
      eligible[!absent] <- as.integer(call$spcd[!absent]) %in% codes
    }
  }
  if (entry) {
    eligible <- eligible & call$dbh >= products$min_dbh[row] &
      (is.na(products$max_dbh[row]) | call$dbh < products$max_dbh[row])
  }
  if (!is.na(products$min_age[row]) || !is.na(products$max_age[row])) {
    if (is.null(call$age)) {
      missing[] <- TRUE
    } else {
      absent <- is.na(call$age)
      missing[absent] <- TRUE
      eligible[!absent] <- eligible[!absent] &
        (is.na(products$min_age[row]) |
           call$age[!absent] >= products$min_age[row]) &
        (is.na(products$max_age[row]) |
           call$age[!absent] < products$max_age[row])
    }
  }
  if (!is.na(products$pruned[row])) {
    if (is.null(call$pruned)) {
      missing[] <- TRUE
    } else {
      absent <- is.na(call$pruned)
      missing[absent] <- TRUE
      eligible[!absent] <- eligible[!absent] &
        call$pruned[!absent] == products$pruned[row]
    }
  }
  output_eligible <- eligible & !missing
  output_missing <- missing & eligible
  output_eligible[is.na(output_eligible)] <- FALSE
  output_missing[is.na(output_missing)] <- FALSE
  list(eligible = output_eligible, missing = output_missing)
}

.mc_build_chains <- function(call, tree_status) {
  products <- call$products
  order <- order(products$priority, products$product, method = "radix")
  n_product <- length(order)
  entry_eligible <- entry_missing <- matrix(
    FALSE, nrow = call$size, ncol = n_product
  )
  fallback_eligible <- matrix(FALSE, nrow = call$size, ncol = n_product)
  for (position in seq_along(order)) {
    row <- order[position]
    entry <- .mc_static_product_vector(products, row, call, TRUE)
    fallback <- .mc_static_product_vector(products, row, call, FALSE)
    entry_eligible[, position] <- entry$eligible
    entry_missing[, position] <- entry$missing
    fallback_eligible[, position] <- fallback$eligible
  }
  entry_at <- rep(NA_integer_, call$size)
  for (position in seq_along(order)) {
    take <- is.na(entry_at) & entry_eligible[, position]
    entry_at[take] <- position
  }
  absent <- tree_status == 0L & is.na(entry_at)
  if (any(absent)) {
    missing <- rowSums(entry_missing[absent, , drop = FALSE]) > 0
    tree_status[absent] <- ifelse(missing, 408L, 400L)
  }
  key_columns <- c(
    list(valid = tree_status == 0L, entry = entry_at),
    lapply(seq_len(ncol(fallback_eligible)), function(position) {
      fallback_eligible[, position]
    })
  )
  key <- do.call(paste, c(key_columns, sep = "\r"))
  unique_key <- unique(key)
  pattern_row <- match(unique_key, key)
  pattern_chains <- lapply(pattern_row, function(tree) {
    if (tree_status[tree] != 0L) return(NULL)
    current_entry <- entry_at[tree]
    entry <- order[current_entry]
    chain <- entry
    if (products$allow_lower_products[entry]) {
      for (position in seq_along(order)[seq_along(order) > current_entry]) {
        row <- order[position]
        if (!products$fallback[row]) next
        if (!fallback_eligible[tree, position]) next
        chain <- c(chain, row)
        if (!products$allow_lower_products[row]) break
      }
    }
    chain
  })
  chains <- pattern_chains[match(key, unique_key)]
  list(chains = chains, status = tree_status)
}

.mc_default_top_assumptions <- function(call, chains) {
  if (!call$utilization_top_defaulted) return(.mc_empty_assumptions())
  tree <- which(lengths(chains) > 0L)
  if (!length(tree)) return(.mc_empty_assumptions())
  product_row <- vapply(chains[tree], utils::tail, integer(1L), 1L)
  spcd <- if (is.null(call$spcd)) rep(NA_integer_, length(tree)) else
    call$spcd[tree]
  result <- .mc_assumption_rows(
    "utilization_top", size = length(tree), spcd = spcd,
    species = .mc_species_names(spcd),
    value = call$products$min_sed[product_row], units = call$units,
    basis = call$products$diameter_basis[product_row],
    product = call$products$product[product_row],
    source = "product_chain_default"
  )
  unique(result)
}

.mc_validate_curvature <- function(scale, products, defects) {
  required <- any(!is.na(products$max_sweep) | !is.na(products$max_crook)) ||
    (!is.null(defects) && any(defects$effect %in% c("sweep", "crook")))
  if (!required && is.null(scale)) return(NULL)
  if (is.list(scale) && identical(names(scale), c("order", "ratio_upper"))) {
    if (is.factor(scale$order)) scale$order <- as.character(scale$order)
    if (.mc_numeric(scale$ratio_upper)) {
      scale$ratio_upper <- as.double(scale$ratio_upper)
    }
  }
  if (!is.list(scale) ||
        !identical(names(scale), c("order", "ratio_upper")) ||
        !is.character(scale$order) || !is.double(scale$ratio_upper) ||
        length(scale$order) != length(scale$ratio_upper) || !length(scale$order) ||
        anyNA(scale$order) || any(!nzchar(scale$order)) ||
        anyDuplicated(scale$order) || scale$ratio_upper[1L] != 0 ||
        !is.infinite(utils::tail(scale$ratio_upper, 1L)) ||
        any(!is.finite(utils::head(scale$ratio_upper, -1L))) ||
        is.unsorted(scale$ratio_upper, strictly = TRUE)) {
    stop(
      paste(
        "curvature_scale must contain unique order labels and strictly",
        "increasing ratio_upper bounds from zero through Inf. Correct both vectors and try again."
      ),
      call. = FALSE
    )
  }
  declared <- unique(c(products$max_sweep, products$max_crook))
  declared <- declared[!is.na(declared)]
  if (any(!declared %in% scale$order)) {
    stop(
      "A product curvature maximum is absent from curvature_scale. ",
      "Add the missing category to order and ratio_upper.", call. = FALSE
    )
  }
  scale
}

.mc_union_intervals <- function(from, to, lower, upper) {
  if (!length(from)) return(data.frame(from = double(), to = double()))
  from <- pmax(from, lower)
  to <- pmin(to, upper)
  keep <- from < to
  if (!any(keep)) return(data.frame(from = double(), to = double()))
  from <- from[keep]
  to <- to[keep]
  ordering <- order(from, to)
  from <- from[ordering]
  to <- to[ordering]
  out_from <- out_to <- numeric()
  for (i in seq_along(from)) {
    if (!length(out_from) || from[i] > utils::tail(out_to, 1L)) {
      out_from <- c(out_from, from[i])
      out_to <- c(out_to, to[i])
    } else {
      out_to[length(out_to)] <- max(out_to[length(out_to)], to[i])
    }
  }
  data.frame(from = out_from, to = out_to)
}

.mc_sound_segments <- function(stump, top, cull) {
  if (!(top > stump)) return(data.frame(lo = double(), hi = double()))
  union <- .mc_union_intervals(cull$from, cull$to, stump, top)
  if (!nrow(union)) return(data.frame(lo = stump, hi = top))
  cursor <- stump
  lo <- hi <- numeric()
  for (row in seq_len(nrow(union))) {
    if (union$from[row] > cursor) {
      lo <- c(lo, cursor)
      hi <- c(hi, union$from[row])
    }
    cursor <- max(cursor, union$to[row])
  }
  if (cursor < top) {
    lo <- c(lo, cursor)
    hi <- c(hi, top)
  }
  data.frame(lo = lo, hi = hi)
}

.mc_plan_crossings <- function(call, session, chains, tree_status) {
  request_type <- request_basis <- character()
  request_tree <- request_product <- integer()
  request_target <- numeric()
  for (tree in seq_len(call$size)) {
    if (tree_status[tree] != 0L || !length(chains[[tree]])) next
    if (is.null(call$utilization_top)) {
      controlling <- utils::tail(chains[[tree]], 1L)
      target <- call$products$min_sed[controlling]
      basis <- call$products$diameter_basis[controlling]
    } else {
      target <- call$utilization_top[tree]
      basis <- call$utilization_top_basis
    }
    request_type <- c(request_type, "top")
    request_tree <- c(request_tree, tree)
    request_product <- c(request_product, NA_integer_)
    request_target <- c(request_target, target)
    request_basis <- c(request_basis, basis)
    for (product in chains[[tree]]) {
      for (name in c("min_sed", "max_sed", "min_led", "max_led")) {
        value <- call$products[[name]][product]
        if (is.na(value) || value <= 0) next
        request_type <- c(request_type, "boundary")
        request_tree <- c(request_tree, tree)
        request_product <- c(request_product, product)
        request_target <- c(request_target, value)
        request_basis <- c(
          request_basis, call$products$diameter_basis[product]
        )
      }
    }
  }
  entries <- data.frame(
    type = request_type, tree = request_tree, product = request_product,
    target = request_target, basis = request_basis,
    stringsAsFactors = FALSE
  )
  roots <- vector("list", nrow(entries))
  query <- which(!is.na(entries$target) & entries$target > 0)
  if (length(query)) {
    key <- paste(
      entries$tree[query], entries$target[query], entries$basis[query],
      sep = "\034"
    )
    unique_query <- query[!duplicated(key)]
    answer <- .mc_all_crossings(
      session, call, entries$tree[unique_query], entries$target[unique_query],
      entries$basis[unique_query], call$stump[entries$tree[unique_query]],
      call$ht[entries$tree[unique_query]]
    )
    answer_index <- match(key, key[!duplicated(key)])
    roots[query] <- answer[answer_index]
  }
  list(entries = entries, roots = roots)
}

.mc_resolve_top <- function(call, chains, defects, tree_status, crossings) {
  break_height <- rep(Inf, call$size)
  if (nrow(defects)) {
    breaks <- defects[defects$status == 0L & defects$effect == "break", , drop = FALSE]
    if (nrow(breaks)) {
      values <- tapply(breaks$from, match(breaks$id, call$id), min)
      break_height[as.integer(names(values))] <- values
    }
  }
  top <- rep(NA_real_, call$size)
  top_rows <- which(crossings$entries$type == "top")
  for (row in top_rows) {
    tree <- crossings$entries$tree[row]
    if (tree_status[tree] != 0L) next
    target <- crossings$entries$target[row]
    if (is.na(target)) {
      tree_status[tree] <- 409L
    } else if (target <= 0) {
      top[tree] <- call$ht[tree]
    } else {
      roots <- crossings$roots[[row]]
      root_status <- attr(roots, "status")
      if (!root_status %in% c(0L, 52L, 102L)) {
        tree_status[tree] <- root_status
      } else if (!length(roots) || !is.finite(utils::tail(roots, 1L))) {
        tree_status[tree] <- 409L
      } else {
        top[tree] <- utils::tail(roots, 1L)
      }
    }
  }
  caps <- call$ht
  if (!is.null(call$utilization_height)) {
    caps <- pmin(caps, call$utilization_height, na.rm = TRUE)
  }
  top <- pmin(top, caps, break_height)
  unusable <- tree_status == 0L & (!is.finite(top) | top < call$stump)
  tree_status[unusable] <- 409L
  list(top = top, break_height = break_height, status = tree_status)
}

.mc_product_upper_length <- function(products, product) {
  if (!is.null(products$lengths[[product]])) max(products$lengths[[product]])
  else products$max_length[product]
}

.mc_boundary_sets <- function(call, chains, segments, defects, top,
                              tree_status, crossings) {
  n_product <- nrow(call$products)
  sets <- rep(list(numeric()), call$size * n_product)
  for (tree in seq_len(call$size)) {
    if (tree_status[tree] != 0L) next
    defect_rows <- which(defects$id == call$id[tree] & defects$status == 0L)
    internal_defect <- unique(c(defects$from[defect_rows], defects$to[defect_rows]))
    for (product in chains[[tree]]) {
      key <- (tree - 1L) * n_product + product
      sets[[key]] <- sort(unique(c(segments[[tree]]$hi, internal_defect)))
    }
  }
  rows <- which(crossings$entries$type == "boundary")
  if (length(rows)) {
    for (row in rows) {
      tree <- crossings$entries$tree[row]
      roots <- crossings$roots[[row]]
      status <- attr(roots, "status")
      if (!status %in% c(0L, 52L, 102L) && tree_status[tree] == 0L) {
        tree_status[tree] <- status
        next
      }
      roots <- roots[roots >= call$stump[tree] & roots <= top[tree]]
      key <- (tree - 1L) * n_product + crossings$entries$product[row]
      sets[[key]] <- sort(unique(c(sets[[key]], roots)))
    }
  }
  list(sets = sets, status = tree_status)
}

.mc_lengths_at <- function(products, product, available, q) {
  if (!is.null(products$lengths[[product]])) {
    return(products$lengths[[product]][products$lengths[[product]] +
                                         products$trim[product] <= available])
  }
  max_tick <- floor((available - products$trim[product]) / q)
  minimum <- attr(products, "min_length_tick")[product]
  maximum <- attr(products, "max_length_tick")[product]
  step <- attr(products, "length_step_tick")[product]
  if (!is.na(maximum)) max_tick <- min(max_tick, maximum)
  if (max_tick < minimum) return(numeric())
  ticks <- seq.int(minimum, max_tick, by = step)
  values <- ticks * q
  if (products$length_parity[product] == "even") {
    values <- values[values == round(values) & round(values) %% 2 == 0]
  } else if (products$length_parity[product] == "odd") {
    values <- values[values == round(values) & round(values) %% 2 == 1]
  }
  values
}

.mc_query_heights <- function(call, chains, segments, boundaries, defects,
                              top, break_height, tree_status) {
  q <- attr(call$products, "quantum")
  n_product <- nrow(call$products)
  all_heights <- vector("list", call$size)
  cache_allowed <- !nrow(defects)
  cache_signatures <- list()
  cache_heights <- list()
  for (tree in seq_len(call$size)) {
    if (is.na(call$model[tree])) {
      all_heights[[tree]] <- numeric()
      next
    }
    if (cache_allowed) {
      signature <- list(
        status = tree_status[tree], stump = call$stump[tree], ht = call$ht[tree],
        top = top[tree], break_height = break_height[tree],
        chain = chains[[tree]], segments = segments[[tree]],
        boundaries = boundaries[
          (tree - 1L) * n_product + seq_len(n_product)
        ]
      )
      cached <- which(vapply(
        cache_signatures, identical, logical(1L), signature
      ))
      if (length(cached)) {
        all_heights[[tree]] <- cache_heights[[cached[[1L]]]]
        next
      }
    }
    break_point <- if (is.finite(break_height[tree])) {
      break_height[tree]
    } else {
      numeric()
    }
    base <- unique(c(
      0, call$stump[tree], call$ht[tree], top[tree], break_point
    ))
    defect_rows <- which(defects$id == call$id[tree] & defects$status == 0L)
    base <- c(base, defects$from[defect_rows], defects$to[defect_rows])
    if (tree_status[tree] == 0L) {
      for (segment in seq_len(nrow(segments[[tree]]))) {
        lo <- segments[[tree]]$lo[segment]
        hi <- segments[[tree]]$hi[segment]
        nodes <- lo
        scale_points <- numeric()
        cursor <- 1L
        while (cursor <= length(nodes)) {
          a <- nodes[cursor]
          additions <- numeric()
          for (product in chains[[tree]]) {
            lengths <- .mc_lengths_at(call$products, product, hi - a, q)
            if (length(lengths)) {
              additions <- c(
                additions, a + lengths + call$products$trim[product]
              )
              for (length in lengths) {
                scale_points <- c(
                  scale_points,
                  .mc_candidate_scale_points(call, product, a, length)
                )
              }
            }
            if (!is.na(call$products$min_boundary_length[product])) {
              key <- (tree - 1L) * n_product + product
              candidate <- boundaries[[key]]
              candidate <- candidate[candidate > a & candidate <= hi]
              if (length(candidate)) {
                length <- candidate - a - call$products$trim[product]
                upper <- .mc_product_upper_length(call$products, product)
                keep <- length > 0 &
                  length >= call$products$min_boundary_length[product] &
                  (is.na(upper) | length <= upper)
                additions <- c(additions, candidate[keep])
                for (boundary_length in length[keep]) {
                  scale_points <- c(
                    scale_points,
                    .mc_candidate_scale_points(
                      call, product, a, boundary_length
                    )
                  )
                }
              }
            }
          }
          additions <- additions[additions > a & additions <= hi]
          if (length(additions)) {
            combined <- sort(unique(c(nodes, additions)))
            if (length(combined) > 50000L) {
              tree_status[tree] <- 412L
              break
            }
            nodes <- combined
          }
          cursor <- cursor + 1L
        }
        base <- c(base, nodes, scale_points, hi)
      }
    }
    all_heights[[tree]] <- sort(unique(base[
      is.finite(base) & base >= 0 & base <= call$ht[tree]
    ]))
    if (cache_allowed) {
      cache_signatures[[length(cache_signatures) + 1L]] <- signature
      cache_heights[[length(cache_heights) + 1L]] <- all_heights[[tree]]
    }
  }
  list(heights = all_heights, status = tree_status)
}

.mc_ob_required <- function(call, chains) {
  active <- unique(unlist(chains, use.names = FALSE))
  product_required <- length(active) && any(
    call$products$diameter_basis[active] == "ob" |
      call$products$scale_bark_basis[active] == "ob"
  )
  extra_required <- !is.null(call$scaling) && nrow(call$scaling) &&
    any(call$scaling$scale_bark_basis == "ob")
  product_required || extra_required
}

.mc_flatten_offsets <- function(values) {
  lengths <- lengths(values)
  list(offsets = as.integer(c(0, cumsum(lengths))), values = unlist(values, use.names = FALSE))
}

.mc_cpp_products <- function(products, curvature) {
  if (any(c("sold_by", "volume_unit") %in% names(products))) {
    units <- attr(products, "units")
    if (is.null(units)) units <- "imperial"
    products <- .validate_products_units(products, units)
  }
  length_ticks <- attr(products, "length_ticks")
  lengths <- .mc_flatten_offsets(length_ticks)
  category <- if (is.null(curvature)) character() else curvature$order
  list(
    priority = products$priority,
    length_mode = as.integer(vapply(products$lengths, is.null, logical(1L))),
    length_offsets = lengths$offsets,
    length_ticks = as.integer(lengths$values),
    min_tick = ifelse(is.na(attr(products, "min_length_tick")), -1L,
                      attr(products, "min_length_tick")),
    max_tick = ifelse(is.na(attr(products, "max_length_tick")), -1L,
                      attr(products, "max_length_tick")),
    step_tick = ifelse(is.na(attr(products, "length_step_tick")), -1L,
                       attr(products, "length_step_tick")),
    parity = match(products$length_parity, c("any", "even", "odd")) - 1L,
    segmentation_policy = ifelse(
      products$segmentation_policy == "generic", 0L,
      as.integer(sub("nvel_opt_", "", products$segmentation_policy))
    ),
    trim = products$trim, min_top = products$min_boundary_length,
    min_sed = products$min_sed, max_sed = products$max_sed,
    min_led = products$min_led, max_led = products$max_led,
    diameter_basis = match(products$diameter_basis, c("ib", "ob")) - 1L,
    max_sweep = match(products$max_sweep, category, nomatch = 0L) - 1L,
    max_crook = match(products$max_crook, category, nomatch = 0L) - 1L,
    max_defect_pct = products$max_defect_pct,
    max_logs_per_segment = ifelse(
      is.na(products$max_logs_per_segment), -1L, products$max_logs_per_segment
    ),
    allow_lower_products = as.integer(products$allow_lower_products),
    pulp_product = as.integer(products$pulp_product),
    product = enc2utf8(products$product),
    scale_rule = match(products$scale_rule, .mc_scale_rules) - 1L,
    measurement_quantity = match(
      products$measurement_quantity,
      c("board_foot", "cubic", "green_weight", "cord")
    ) - 1L,
    scale_unit = match(products$scale_unit, c("ft3", "m3"), nomatch = 1L) - 1L,
    scale_basis = match(products$scale_bark_basis, c("ib", "ob")) - 1L,
    diameter_round = match(products$diameter_round, .mc_rounding_operators) - 1L,
    length_round = match(products$length_round, .mc_rounding_operators) - 1L,
    volume_round = match(products$volume_round, .mc_rounding_operators) - 1L,
    cord_fraction = products$cord_solid_fraction,
    price = ifelse(is.na(products$price), 0, products$price),
    price_quantity = products$price_quantity,
    scribner_factor = .mc_scribner_factor,
    scribner_exception = as.integer(.mc_scribner_exception),
    intl_quadratic = unname(.mc_intl14_constants[["quadratic"]]),
    intl_linear = unname(.mc_intl14_constants[["linear"]]),
    intl_adjustment = unname(.mc_intl14_constants[["adjustment"]])
  )
}

.mc_dp_weight_factors <- function(call, chains, tree_status) {
  n_product <- nrow(call$products)
  factors <- rep(1, call$size * n_product)
  if (call$algorithm != "dp" || call$effective_objective != "value" ||
        !n_product) {
    return(list(factors = factors, status = tree_status))
  }
  green <- which(
    call$products$measurement_quantity == "green_weight" &
      !is.na(call$products$price) & call$products$price > 0
  )
  if (!length(green)) {
    return(list(factors = factors, status = tree_status))
  }
  tree <- rep(seq_len(call$size), lengths(chains))
  product <- unlist(chains, use.names = FALSE)
  keep <- product %in% green & tree_status[tree] == 0L
  tree <- tree[keep]
  product <- product[keep]
  if (!length(tree)) {
    return(list(factors = factors, status = tree_status))
  }
  if (is.null(call$spcd)) {
    tree_status[unique(tree)] <- 411L
    return(list(factors = factors, status = tree_status))
  }
  weight <- mc_provider_green_weight(
    rep(1, length(tree)), as.integer(call$spcd[tree]),
    ifelse(call$products$scale_bark_basis[product] == "ob", 1L, 0L),
    if (call$units == "imperial") 1L else 2L
  )
  failed <- !weight$status %in% c(0L, 52L, 102L)
  if (any(failed)) tree_status[unique(tree[failed])] <- 411L
  denominator <- ifelse(
    call$products$scale_unit[product] == "green_short_ton",
    if (call$units == "imperial") 2000 else 2000 * 0.45359237,
    if (call$units == "metric") 1000 else 1000 / 0.45359237
  )
  index <- (tree - 1L) * n_product + product
  factors[index[!failed]] <- weight$value[!failed] / denominator[!failed]
  list(factors = factors, status = tree_status)
}

.mc_cpp_defects <- function(call, defects, top, curvature) {
  rows <- which(defects$status == 0L)
  x <- defects[rows, , drop = FALSE]
  tree <- match(x$id, call$id)
  fork <- x$effect == "fork"
  x$to[fork] <- top[tree[fork]]
  keep <- is.finite(x$from) & is.finite(x$to) & x$from < x$to
  x <- x[keep, , drop = FALSE]
  tree <- tree[keep]
  ordering <- order(tree, x$from, x$to)
  x <- x[ordering, , drop = FALSE]
  tree <- tree[ordering]
  split <- split(seq_len(nrow(x)), factor(tree, levels = seq_len(call$size)))
  offsets <- as.integer(c(0, cumsum(lengths(split))))
  list(
    offsets = offsets, from = x$from, to = x$to,
    effect = match(x$effect, c("cull", "pulp", "exclude", "rot", "sweep",
                               "crook", "fork", "break")),
    percent = ifelse(is.na(x$percent), 0, x$percent),
    category = if (is.null(curvature)) rep(-1L, nrow(x)) else
      match(x$category, curvature$order, nomatch = 0L) - 1L,
    table = x
  )
}

.mc_profile_lookup <- function(profile, tree, height, column) {
  rows <- .mc_profile_rows(profile, tree)
  if (!length(rows)) return(NA_real_)
  at <- match(height, profile$height[rows])
  if (is.na(at)) return(NA_real_)
  profile[[column]][rows[at]]
}

.mc_profile_lookup_many <- function(profile, tree, height, column) {
  offsets <- attr(profile, "tree_offsets", exact = TRUE)
  if (is.null(offsets)) {
    return(vapply(seq_along(tree), function(row) {
      .mc_profile_lookup(profile, tree[row], height[row], column)
    }, numeric(1L)))
  }
  mc_profile_lookup_cpp(
    offsets, profile$height, as.integer(tree), as.double(height),
    profile[[column]]
  )
}

.mc_profile_rows <- function(profile, tree) {
  offsets <- attr(profile, "tree_offsets", exact = TRUE)
  if (is.null(offsets)) return(which(profile$tree == tree))
  first <- offsets[tree] + 1L
  last <- offsets[tree + 1L]
  if (first > last) integer() else seq.int(first, last)
}

.mc_residuals <- function(call, defects, top, break_height, profile,
                          cpp_residuals, tree_status) {
  rows <- list()
  at <- 0L
  add <- function(tree, from, to, cause, segment = NA_integer_) {
    if (!is.finite(from) || !is.finite(to) || from >= to) return()
    from_volume <- .mc_profile_lookup(profile, tree, from, "cum_ib")
    to_volume <- .mc_profile_lookup(profile, tree, to, "cum_ib")
    from_volume_ob <- .mc_profile_lookup(profile, tree, from, "cum_ob")
    to_volume_ob <- .mc_profile_lookup(profile, tree, to, "cum_ob")
    at <<- at + 1L
    rows[[at]] <<- data.frame(
      tree = tree, segment = segment, from = from, to = to, cause = cause,
      cubic_ib = to_volume - from_volume,
      cubic_ob = to_volume_ob - from_volume_ob,
      stringsAsFactors = FALSE
    )
  }
  simple <- !any(defects$status == 0L & defects$effect == "cull") &&
    !any(is.finite(break_height))
  if (simple) {
    valid <- which(tree_status %in% c(0L, 410L))
    stump_tree <- valid[call$stump[valid] > 0]
    top_tree <- valid[
      is.finite(top[valid]) & is.finite(call$ht[valid]) &
        top[valid] < call$ht[valid]
    ]
    tree <- c(stump_tree, top_tree)
    from <- c(rep(0, length(stump_tree)), top[top_tree])
    to <- c(call$stump[stump_tree], call$ht[top_tree])
    cause <- c(rep("stump", length(stump_tree)), rep("top", length(top_tree)))
    if (length(tree)) {
      lookup <- function(column) {
        .mc_profile_lookup_many(profile, tree, to, column) -
          .mc_profile_lookup_many(profile, tree, from, column)
      }
      at <- 1L
      rows[[at]] <- data.frame(
        tree = tree, segment = NA_integer_, from = from, to = to,
        cause = cause, cubic_ib = lookup("cum_ib"),
        cubic_ob = lookup("cum_ob"), stringsAsFactors = FALSE
      )
    }
  } else {
    for (tree in seq_len(call$size)) {
      if (!tree_status[tree] %in% c(0L, 410L)) next
      add(tree, 0, call$stump[tree], "stump")
      endpoint <- if (is.finite(break_height[tree])) {
        break_height[tree]
      } else {
        call$ht[tree]
      }
      cull <- defects[
        defects$id == call$id[tree] & defects$status == 0L &
          defects$effect == "cull", , drop = FALSE
      ]
      union <- .mc_union_intervals(
        cull$from, cull$to, call$stump[tree], endpoint
      )
      top_segments <- .mc_sound_segments(top[tree], endpoint, union)
      if (nrow(top_segments)) for (i in seq_len(nrow(top_segments))) {
        add(tree, top_segments$lo[i], top_segments$hi[i], "top")
      }
      if (nrow(union)) for (i in seq_len(nrow(union))) {
        add(tree, union$from[i], union$to[i], "cull")
      }
      if (is.finite(break_height[tree])) {
        add(tree, break_height[tree], call$ht[tree], "break")
      }
    }
  }
  if (nrow(cpp_residuals)) {
    cpp_residuals$cause <- c("short_remainder", "diameter_limit", "product_failure")[
      cpp_residuals$cause
    ]
    cpp_residuals$cubic_ob <- .mc_profile_lookup_many(
      profile, cpp_residuals$tree, cpp_residuals$to, "cum_ob"
    ) - .mc_profile_lookup_many(
      profile, cpp_residuals$tree, cpp_residuals$from, "cum_ob"
    )
    cpp_residuals <- cpp_residuals[c(
      "tree", "segment", "from", "to", "cause", "cubic_ib", "cubic_ob"
    )]
    rows[[at + 1L]] <- cpp_residuals
  }
  result <- .mc_bind_rows(rows)
  if (is.null(result)) result <- data.frame(
    tree = integer(), segment = integer(), from = double(), to = double(),
    cause = character(), cubic_ib = double(), cubic_ob = double(),
    stringsAsFactors = FALSE
  )
  result$id <- call$id[result$tree]
  result <- result[c(
    "id", "segment", "from", "to", "cause", "cubic_ib", "cubic_ob"
  )]
  if (nrow(result)) {
    result <- result[order(match(result$id, call$id), result$from, result$to), , drop = FALSE]
    rownames(result) <- NULL
  }
  result
}

.mc_empty_result <- function(call) {
  ids <- .mc_empty_id(call$id)
  logs <- data.frame(
    tree = integer(), log = integer(), segment = integer(), stage = integer(),
    product_index = integer(), length_kind = character(),
    start_height = double(), nominal_end_height = double(),
    end_height = double(), nominal_length = double(), physical_length = double(),
    trim = double(), led_ib = double(), sed_ib = double(), led_ob = double(),
    sed_ob = double(), log_gross_cubic_ib = double(),
    log_gross_cubic_ob = double(), located_deduction_cubic_ib = double(),
    located_deduction_cubic_ob = double(),
    rot_pct = double(), id = ids, product = character(), priority = integer(),
    grade = character(), diameter_basis = character(),
    log_located_net_cubic_ib = double(),
    log_located_net_cubic_ob = double(),
    posthoc_deduction_cubic_ib = double(), log_net_cubic_ib = double(),
    posthoc_deduction_cubic_ob = double(), log_net_cubic_ob = double(),
    trim_cubic_ib = double(), scale_rule = character(),
    measurement_quantity = character(), scale_unit = character(), scale_bark_basis = character(),
    volume_unit = character(), gross_scale = double(),
    located_net_scale = double(), net_scale = double(),
    stringsAsFactors = FALSE
  )
  trees <- data.frame(
    id = ids, dbh = double(), ht = double(), model = character(),
    stump_height = double(), utilization_height = double(),
    gross_stem_cubic_ib = double(), log_net_cubic_ib = double(),
    gross_stem_cubic_ob = double(), log_net_cubic_ob = double(),
    stump_cubic_ib = double(), top_cubic_ib = double(),
    cull_cubic_ib = double(), break_cubic_ib = double(),
    stump_cubic_ob = double(), top_cubic_ob = double(),
    cull_cubic_ob = double(), break_cubic_ob = double(),
    located_deduction_cubic_ib = double(),
    posthoc_deduction_cubic_ib = double(),
    located_deduction_cubic_ob = double(),
    posthoc_deduction_cubic_ob = double(), log_count = integer(),
    stringsAsFactors = FALSE
  )
  if (!is.null(call$spcd)) trees$spcd <- call$spcd[FALSE]
  if (!is.null(call$age)) trees$age <- call$age[FALSE]
  if (!is.null(call$pruned)) trees$pruned <- call$pruned[FALSE]
  trees$reconciled_cubic_ib <- logical()
  trees$reconciled_cubic_ob <- logical()
  if (call$include_status) {
    logs$status <- integer()
    trees$status <- integer()
  }
  priced <- any(!is.na(call$products$price))
  if (priced) {
    for (name in c(
      "gross_value", "located_net_value", "located_deduction_value",
      "posthoc_deduction_value", "net_value"
    )) {
      logs[[name]] <- double()
      trees[[name]] <- double()
    }
  }
  scales <- data.frame(
    id = ids, log = integer(), product = character(),
    scale_rule = character(), unit = character(), bark_basis = character(),
    gross = double(), located_net = double(), located_deduction = double(),
    posthoc_deduction = double(), net = double(), is_product = logical(),
    stringsAsFactors = FALSE
  )
  residuals <- data.frame(
    id = ids, segment = integer(), from = double(), to = double(),
    cause = character(), cubic_ib = double(), cubic_ob = double(),
    stringsAsFactors = FALSE
  )
  defect_accounting <- data.frame(
    id = ids, log = integer(), located_deduction_cubic_ib = double(),
    posthoc_deduction_cubic_ib = double(),
    located_deduction_cubic_ob = double(),
    posthoc_deduction_cubic_ob = double(), stringsAsFactors = FALSE
  )
  values <- data.frame(
    id = ids, log = integer(), product = character(), currency = character(),
    gross_value = double(), located_net_value = double(),
    located_deduction_value = double(), posthoc_deduction_value = double(),
    net_value = double(),
    stringsAsFactors = FALSE
  )
  metadata <- list(
    contract_version = "1.1", units = call$units,
    algorithm = call$algorithm, objective = call$effective_objective,
    unpriced = call$unpriced, model_vector_hash = .mc_model_hash(call$model),
    taper_manifest = taper_manifest(),
    provider_generation = NA_real_,
    nvel_source_revision = nvel_source_revision(),
    products_hash = .mc_products_hash(call$products), posthoc_calls = data.frame(
      call_seq = integer(), kind = character(), percent = double()
    ), assumptions = call$assumptions, call = .mc_replay_call(call)
  )
  result <- list(
    logs = logs, scales = scales, trees = trees, residuals = residuals,
    defect_accounting = defect_accounting, values = values,
    run_metadata = metadata
  )
  if (call$include_status) {
    result$diagnostics <- data.frame(
      id = ids, spcd = integer(), species = character(),
      status = integer(), status_name = character(),
      stringsAsFactors = FALSE
    )
  }
  class(result) <- "merch_result"
  result
}

.mc_replay_call <- function(call) {
  c(list(
    dbh = call$dbh, ht = call$ht, model = call$model,
    products = call$original_products, id = call$id, spcd = call$spcd,
    age = call$age, pruned = call$pruned, defects = call$defects,
    stump_ht = call$stump, utilization_top = call$utilization_top,
    utilization_top_basis = call$utilization_top_basis,
    utilization_height = call$utilization_height,
    curvature_scale = call$curvature_scale, scaling = call$scaling,
    currency = call$currency, units = call$units, status = TRUE
  ), call$aux)
}

.mc_validate_call_defects <- function(call) {
  if (is.null(call$defects) || !nrow(call$defects)) {
    x <- .mc_empty_defects(call$id)
    x$status <- integer()
    return(x)
  }
  validate_defects(call$defects, call$products, call$ht, call$id)
}

.mc_duplicate_stems <- function(call, defects) {
  tree <- match(defects$id, call$id)
  counts <- tabulate(tree, nbins = call$size)
  offsets <- as.integer(c(0, cumsum(counts)))
  columns <- Filter(Negate(is.null), c(list(
    model = call$model, dbh = call$dbh, ht = call$ht,
    spcd = call$spcd, age = call$age, pruned = call$pruned,
    stump = call$stump, utilization_top = call$utilization_top,
    utilization_height = call$utilization_height
  ), call$aux))
  defect_columns <- unclass(defects[setdiff(names(defects), "id")])
  mc_duplicate_map_cpp(columns, offsets, defect_columns)
}

.mc_subset_merch_call <- function(call, rows, defects) {
  result <- call
  result$size <- length(rows)
  for (name in c(
    "dbh", "ht", "model", "id", "spcd", "age", "pruned", "stump",
    "utilization_top", "utilization_height", "model_status"
  )) {
    if (!is.null(result[[name]])) result[[name]] <- result[[name]][rows]
  }
  result$aux <- lapply(result$aux, `[`, rows)
  keep <- defects$id %in% result$id
  result$validated_defects <- defects[keep, , drop = FALSE]
  result$defects <- result$validated_defects
  result
}

.mc_columnar_subset <- function(x, rows) {
  structure(
    lapply(unclass(x)[names(x)], `[`, rows), names = names(x),
    row.names = .set_row_names(length(rows)), class = "data.frame"
  )
}

.mc_expand_tree_table <- function(x, unique_ids, map, ids,
                                  tree_column = FALSE) {
  if (!nrow(x)) return(x)
  unique_tree <- if (tree_column) x$tree else match(x$id, unique_ids)
  index <- mc_expand_index_cpp(as.integer(unique_tree), as.integer(map))
  result <- .mc_columnar_subset(x, index$source)
  if (tree_column) result$tree <- index$tree
  result$id <- ids[index$tree]
  rownames(result) <- NULL
  result
}

.mc_expand_merch_result <- function(x, call, map) {
  unique_ids <- x$trees$id
  scale_status <- attr(x$logs, "scale_status", exact = TRUE)
  x$logs <- .mc_expand_tree_table(
    x$logs, unique_ids, map, call$id, tree_column = TRUE
  )
  x$scales <- .mc_expand_tree_table(x$scales, unique_ids, map, call$id)
  x$residuals <- .mc_expand_tree_table(x$residuals, unique_ids, map, call$id)
  x$defect_accounting <- .mc_expand_tree_table(
    x$defect_accounting, unique_ids, map, call$id
  )
  x$values <- .mc_expand_tree_table(x$values, unique_ids, map, call$id)
  x$trees <- .mc_columnar_subset(x$trees, map)
  x$trees$id <- call$id
  rownames(x$trees) <- NULL
  if (!is.null(x$diagnostics)) {
    x$diagnostics <- .mc_expand_tree_table(
      x$diagnostics, unique_ids, map, call$id
    )
  }
  attr(x$logs, "scales") <- x$scales
  if (!is.null(scale_status)) attr(x$logs, "scale_status") <- scale_status[map]
  x$run_metadata$model_vector_hash <- .mc_model_hash(call$model)
  x$run_metadata$call <- .mc_replay_call(call)
  class(x) <- "merch_result"
  x
}

.mc_can_use_simple_native_plan <- function(call, session, chains, defects) {
  if (nrow(defects) || !is.null(call$scaling) ||
        any(call$products$segmentation_policy != "generic") ||
        any(call$products$scale_rule != "cubic")) {
    return(FALSE)
  }
  active <- unique(unlist(chains, use.names = FALSE))
  if (!length(active)) return(FALSE)
  active_tree <- lengths(chains) > 0L
  compiled <- session$kernel_type[session$model_index[active_tree]] == 1L
  if (!all(compiled)) return(FALSE)
  bounds <- unlist(call$products[active, c(
    "min_sed", "max_sed", "min_led", "max_led"
  )], use.names = FALSE)
  if (any(!is.na(bounds) & bounds > 0)) return(FALSE)
  if (!is.null(call$utilization_top) &&
        any(is.na(call$utilization_top) | call$utilization_top > 0)) {
    return(FALSE)
  }
  TRUE
}

.mc_merchandise <- function(dbh, ht, model, products, id = NULL, spcd = NULL,
                            age = NULL, pruned = NULL, defects = NULL,
                            stump_ht = NULL, utilization_top = NULL,
                            utilization_top_basis = "ib",
                            utilization_height = NULL,
                            curvature_scale = NULL, scaling = NULL,
                            currency = NULL, algorithm = "cascade",
                            objective = "auto", unpriced = "zero", ...,
                            units = "imperial", status = FALSE,
                            model_status = 0L, assumptions = NULL) {
  call <- .mc_prepare_merch_call(
    dbh, ht, model, products, id, spcd, age, pruned, defects, stump_ht,
    utilization_top, utilization_top_basis, utilization_height,
    curvature_scale, scaling, currency, algorithm, objective, unpriced,
    list(...), units, status, model_status, assumptions
  )
  if (!call$size) return(.mc_empty_result(call))
  defects <- .mc_validate_call_defects(call)
  duplicate <- .mc_duplicate_stems(call, defects)
  if (length(duplicate)) {
    unique_call <- .mc_subset_merch_call(call, duplicate$unique, defects)
    include_status <- unique_call$include_status
    if (!include_status) unique_call$include_status <- TRUE
    result <- .mc_merchandise_call(unique_call)
    result <- .mc_expand_merch_result(result, call, duplicate$map)
    if (!include_status) {
      tree_status <- result$trees$status
      result$trees$status <- NULL
      result$logs$status <- NULL
      result$diagnostics <- NULL
      .mc_status_warning("merchandise", tree_status, call$spcd)
    }
    return(result)
  }
  call$validated_defects <- defects
  .mc_merchandise_call(call)
}

.mc_merchandise_call <- function(call) {
  tree_status <- integer(call$size)
  missing_input <- is.na(call$dbh) | is.na(call$ht) |
    (is.na(call$model) & call$model_status == 0L)
  tree_status[missing_input] <- 1L
  tree_status[tree_status == 0L & call$dbh <= 0] <- 2L
  tree_status[tree_status == 0L & call$ht <= 0] <- 3L
  unresolved <- tree_status == 0L & call$model_status != 0L
  tree_status[unresolved] <- call$model_status[unresolved]
  session <- .mc_open_provider(call$model)
  on.exit(mc_provider_close(session$pointer), add = TRUE)
  resolved_model <- !is.na(session$model_index)
  provider_status <- integer(call$size)
  provider_status[resolved_model] <-
    session$status[session$model_index[resolved_model]]
  failed_model <- tree_status == 0L & resolved_model &
    !provider_status %in% c(0L, 52L)
  tree_status[failed_model] <- provider_status[failed_model]
  if (any(resolved_model)) {
    session$model_index[!resolved_model] <-
      session$model_index[which(resolved_model)[[1L]]]
  }
  chains_result <- .mc_build_chains(call, tree_status)
  chains <- chains_result$chains
  tree_status <- chains_result$status
  call$assumptions <- .mc_bind_assumptions(
    call$assumptions, .mc_default_top_assumptions(call, chains)
  )
  call$ob_required <- .mc_ob_required(call, chains)
  defects <- call$validated_defects
  if (nrow(defects)) {
    defect_tree <- match(defects$id, call$id)
    for (row in seq_len(nrow(defects))) {
      tree <- defect_tree[row]
      if (tree_status[tree] == 0L && defects$status[row] != 0L) {
        tree_status[tree] <- defects$status[row]
      }
    }
  }
  curvature <- .mc_validate_curvature(call$curvature_scale, call$products, defects)
  if (!is.null(curvature) && nrow(defects)) {
    unknown <- defects$status == 0L & defects$effect %in% c("sweep", "crook") &
      !defects$category %in% curvature$order
    for (tree in unique(match(defects$id[unknown], call$id))) {
      if (tree_status[tree] == 0L) tree_status[tree] <- 407L
    }
  }
  simple_native <- .mc_can_use_simple_native_plan(
    call, session, chains, defects
  )
  if (simple_native) {
    top <- call$ht
    if (!is.null(call$utilization_height)) {
      top <- pmin(top, call$utilization_height, na.rm = TRUE)
    }
    break_height <- rep(Inf, call$size)
    tree_status[
      tree_status == 0L & (!is.finite(top) | top < call$stump)
    ] <- 409L
  } else {
    crossings <- .mc_plan_crossings(call, session, chains, tree_status)
    top_result <- .mc_resolve_top(
      call, chains, defects, tree_status, crossings
    )
    top <- top_result$top
    break_height <- top_result$break_height
    tree_status <- top_result$status
  }
  if (nrow(defects)) {
    restricted <- unique(match(
      defects$id[defects$status == 0L & defects$effect %in% c("pulp", "fork")],
      call$id
    ))
    for (tree in restricted) {
      if (tree_status[tree] == 0L &&
            sum(call$products$pulp_product[chains[[tree]]]) != 1L) {
        stop(
          "Every active chain subject to a pulp restriction must have ",
          "exactly one pulp product. Mark one applicable row with ",
          "pulp_product = TRUE.", call. = FALSE
        )
      }
    }
  }
  weight_result <- .mc_dp_weight_factors(call, chains, tree_status)
  weight_factors <- weight_result$factors
  tree_status <- weight_result$status
  chain_flat <- .mc_flatten_offsets(chains)
  if (simple_native) {
    plan <- mc_simple_plan_cpp(
      .mc_cpp_products(call$products, curvature),
      attr(call$products, "quantum"), tree_status, chain_flat$offsets,
      as.integer(chain_flat$values - 1L), call$stump, top, call$ht,
      as.integer(threads())
    )
    changed <- tree_status == 0L & plan$status != 0L
    tree_status[changed] <- plan$status[changed]
    segment_offsets <- plan$segment_offsets
    segment_lo <- plan$segment_lo
    segment_hi <- plan$segment_hi
    boundary_offsets <- plan$boundary_offsets
    boundary_values <- plan$boundaries
    profile_offsets <- plan$profile_offsets
    profile_height <- plan$profile_height
  } else {
    segments <- vector("list", call$size)
    for (tree in seq_len(call$size)) {
      cull <- defects[
        defects$id == call$id[tree] & defects$status == 0L &
          defects$effect == "cull", , drop = FALSE
      ]
      segments[[tree]] <- if (tree_status[tree] == 0L) {
        .mc_sound_segments(call$stump[tree], top[tree], cull)
      } else {
        data.frame(lo = double(), hi = double())
      }
    }
    boundary_result <- .mc_boundary_sets(
      call, chains, segments, defects, top, tree_status, crossings
    )
    boundaries <- boundary_result$sets
    tree_status <- boundary_result$status
    height_result <- .mc_query_heights(
      call, chains, segments, boundaries, defects, top, break_height,
      tree_status
    )
    heights <- height_result$heights
    tree_status <- height_result$status
    flat_heights <- .mc_flatten_offsets(heights)
    segment_flat <- .mc_flatten_offsets(lapply(
      segments, function(x) seq_len(nrow(x))
    ))
    segment_offsets <- segment_flat$offsets
    segment_lo <- unlist(lapply(segments, `[[`, "lo"), use.names = FALSE)
    segment_hi <- unlist(lapply(segments, `[[`, "hi"), use.names = FALSE)
    boundary_flat <- .mc_flatten_offsets(boundaries)
    boundary_offsets <- boundary_flat$offsets
    boundary_values <- boundary_flat$values
    profile_offsets <- flat_heights$offsets
    profile_height <- flat_heights$values
  }
  query_tree <- rep(seq_len(call$size), diff(profile_offsets))
  profile <- .mc_query_points(session, call, query_tree, profile_height)
  attr(profile, "tree_offsets") <- profile_offsets
  bad_profile <- !profile$status %in% c(0L, 52L, 102L)
  if (any(bad_profile)) {
    bad_tree <- profile$tree[bad_profile]
    first <- !duplicated(bad_tree)
    bad_tree <- bad_tree[first]
    update <- tree_status[bad_tree] == 0L
    tree_status[bad_tree[update]] <- profile$status[bad_profile][first][update]
  }
  cpp_defects <- .mc_cpp_defects(call, defects, top, curvature)
  core <- mc_buck_cpp(
    .mc_cpp_products(call$products, curvature), attr(call$products, "quantum"),
    if (call$units == "imperial") 1L else 2L,
    match(call$algorithm, c("cascade", "dp")) - 1L,
    match(call$effective_objective, c("net_cubic_ib", "value")) - 1L,
    tree_status, chain_flat$offsets, as.integer(chain_flat$values - 1L),
    segment_offsets, segment_lo, segment_hi, boundary_offsets,
    boundary_values, profile_offsets, profile$height, profile$dib,
    profile$dob, profile$cum_ib, profile$cum_ob, cpp_defects$offsets,
    cpp_defects$from, cpp_defects$to, cpp_defects$effect,
    cpp_defects$percent, cpp_defects$category, weight_factors,
    as.integer(threads())
  )
  core_failed <- tree_status == 0L & core$status != 0L
  tree_status[core_failed] <- core$status[core_failed]
  logs <- core$logs
  logs$native_gross_scale <- core$native_gross_scale
  if (nrow(logs)) {
    logs$id <- call$id[logs$tree]
    logs$product <- call$products$product[logs$product_index]
    logs$priority <- call$products$priority[logs$product_index]
    logs$grade <- call$products$grade[logs$product_index]
    logs$length_kind <- c("ordinary", "boundary")[logs$length_kind + 1L]
    logs$diameter_basis <- call$products$diameter_basis[logs$product_index]
    logs$log_located_net_cubic_ib <-
      logs$log_gross_cubic_ib - logs$located_deduction_cubic_ib
    located_ratio <- logs$located_deduction_cubic_ib /
      logs$log_gross_cubic_ib
    located_ratio[!is.finite(located_ratio)] <- 0
    logs$located_deduction_cubic_ob <-
      logs$log_gross_cubic_ob * located_ratio
    logs$log_located_net_cubic_ob <-
      logs$log_gross_cubic_ob - logs$located_deduction_cubic_ob
    logs$posthoc_deduction_cubic_ib <- 0
    logs$posthoc_deduction_cubic_ob <- 0
    logs$log_net_cubic_ib <- logs$log_located_net_cubic_ib
    logs$log_net_cubic_ob <- logs$log_located_net_cubic_ob
    logs <- .mc_scale_pieces(logs, call, profile)
    scale_rows <- attr(logs, "scales")
    scale_status <- attr(logs, "scale_status")
    logs <- logs[order(logs$tree, logs$log), , drop = FALSE]
    attr(logs, "scales") <- scale_rows
    attr(logs, "scale_status") <- scale_status
    rownames(logs) <- NULL
  } else {
    logs <- .mc_empty_result(call)$logs
    attr(logs, "scale_status") <- integer(call$size)
  }
  scale_status <- attr(logs, "scale_status")
  failed_scale <- tree_status == 0L & scale_status != 0L
  tree_status[failed_scale] <- scale_status[failed_scale]
  log_count <- tabulate(logs$tree, nbins = call$size)
  tree_status[tree_status == 0L & log_count == 0L] <- 410L
  residuals <- .mc_residuals(
    call, defects, top, break_height, profile, core$residuals, tree_status
  )
  gross_stem <- .mc_profile_lookup_many(
    profile, seq_len(call$size), call$ht, "cum_ib"
  )
  gross_stem_ob <- .mc_profile_lookup_many(
    profile, seq_len(call$size), call$ht, "cum_ob"
  )
  log_net <- .mc_sum_by_index(
    logs$log_net_cubic_ib, logs$tree, call$size
  )
  located <- .mc_sum_by_index(
    logs$located_deduction_cubic_ib, logs$tree, call$size
  )
  posthoc <- .mc_sum_by_index(
    logs$posthoc_deduction_cubic_ib, logs$tree, call$size
  )
  log_net_ob <- .mc_sum_by_index(
    logs$log_net_cubic_ob, logs$tree, call$size, require_defined = TRUE
  )
  located_ob <- .mc_sum_by_index(
    logs$located_deduction_cubic_ob, logs$tree, call$size,
    require_defined = TRUE
  )
  posthoc_ob <- .mc_sum_by_index(
    logs$posthoc_deduction_cubic_ob, logs$tree, call$size,
    require_defined = TRUE
  )
  residual_causes <- list(
    stump_cubic_ib = "stump",
    top_cubic_ib = c("top", "short_remainder", "diameter_limit", "product_failure"),
    cull_cubic_ib = "cull",
    break_cubic_ib = "break"
  )
  residual_tree <- match(residuals$id, call$id)
  residual_totals <- lapply(residual_causes, function(cause) {
    selected <- residuals$cause %in% cause
    .mc_sum_by_index(
      residuals$cubic_ib[selected], residual_tree[selected], call$size
    )
  })
  residual_totals_ob <- lapply(residual_causes, function(cause) {
    selected <- residuals$cause %in% cause
    .mc_sum_by_index(
      residuals$cubic_ob[selected], residual_tree[selected], call$size,
      require_defined = TRUE
    )
  })
  trees <- data.frame(
    id = call$id, dbh = call$dbh, ht = call$ht, model = call$model,
    stump_height = call$stump, utilization_height = top,
    gross_stem_cubic_ib = gross_stem, log_net_cubic_ib = log_net,
    gross_stem_cubic_ob = gross_stem_ob, log_net_cubic_ob = log_net_ob,
    stump_cubic_ib = residual_totals$stump_cubic_ib,
    top_cubic_ib = residual_totals$top_cubic_ib,
    cull_cubic_ib = residual_totals$cull_cubic_ib,
    break_cubic_ib = residual_totals$break_cubic_ib,
    stump_cubic_ob = residual_totals_ob$stump_cubic_ib,
    top_cubic_ob = residual_totals_ob$top_cubic_ib,
    cull_cubic_ob = residual_totals_ob$cull_cubic_ib,
    break_cubic_ob = residual_totals_ob$break_cubic_ib,
    located_deduction_cubic_ib = located,
    posthoc_deduction_cubic_ib = posthoc,
    located_deduction_cubic_ob = located_ob,
    posthoc_deduction_cubic_ob = posthoc_ob,
    log_count = log_count, stringsAsFactors = FALSE
  )
  if (!is.null(call$spcd)) trees$spcd <- call$spcd
  if (!is.null(call$age)) trees$age <- call$age
  if (!is.null(call$pruned)) trees$pruned <- call$pruned
  ledger <- with(trees,
    log_net_cubic_ib + stump_cubic_ib + top_cubic_ib + cull_cubic_ib +
      break_cubic_ib + located_deduction_cubic_ib + posthoc_deduction_cubic_ib
  )
  reconcile <- abs(gross_stem - ledger) <= 1e-8 * pmax(1, gross_stem)
  ledger_ob <- with(trees,
    log_net_cubic_ob + stump_cubic_ob + top_cubic_ob + cull_cubic_ob +
      break_cubic_ob + located_deduction_cubic_ob +
      posthoc_deduction_cubic_ob
  )
  reconcile_ob <- abs(gross_stem_ob - ledger_ob) <=
    1e-8 * pmax(1, gross_stem_ob)
  reconcile_ob[!is.finite(gross_stem_ob) | !is.finite(ledger_ob)] <- NA
  trees$reconciled_cubic_ib <- reconcile
  trees$reconciled_cubic_ob <- reconcile_ob
  bad_reconcile <- tree_status %in% c(0L, 410L) &
    (!reconcile | (!is.na(reconcile_ob) & !reconcile_ob))
  tree_status[bad_reconcile] <- 413L
  if (call$include_status) {
    trees$status <- tree_status
    logs$status <- tree_status[logs$tree]
  }
  scales <- attr(logs, "scales")
  if (is.null(scales)) scales <- .mc_empty_result(call)$scales
  values <- .mc_values(logs, call)
  if (any(!is.na(call$products$price))) {
    value_columns <- c(
      "gross_value", "located_net_value", "located_deduction_value",
      "posthoc_deduction_value", "net_value"
    )
    for (name in value_columns) logs[[name]] <- values[[name]]
    value_tree <- match(values$id, call$id)
    for (name in value_columns) {
      trees[[name]] <- .mc_sum_by_index(
        values[[name]], value_tree, call$size, require_defined = TRUE
      )
    }
  }
  defect_accounting <- data.frame(
    id = logs$id, log = logs$log,
    located_deduction_cubic_ib = logs$located_deduction_cubic_ib,
    posthoc_deduction_cubic_ib = logs$posthoc_deduction_cubic_ib,
    located_deduction_cubic_ob = logs$located_deduction_cubic_ob,
    posthoc_deduction_cubic_ob = logs$posthoc_deduction_cubic_ob,
    stringsAsFactors = FALSE
  )
  manifest <- taper_manifest()
  if (!identical(mc_provider_generation(session$pointer), session$generation)) {
    stop("The taper model registry changed during the call.", call. = FALSE)
  }
  metadata <- list(
    contract_version = "1.1", units = call$units,
    algorithm = call$algorithm, objective = call$effective_objective,
    unpriced = call$unpriced,
    model_vector_hash = .mc_model_hash(call$model), taper_manifest = manifest,
    provider_generation = session$generation,
    nvel_source_revision = session$nvel_source_revision,
    products_hash = .mc_products_hash(call$products), posthoc_calls = data.frame(
      call_seq = integer(), kind = character(), percent = double()
    ), assumptions = call$assumptions, call = .mc_replay_call(call)
  )
  failed <- tree_status != 0L
  diagnostic_spcd <- if (is.null(call$spcd)) {
    rep(NA_integer_, sum(failed))
  } else {
    call$spcd[failed]
  }
  diagnostics <- if (call$include_status) data.frame(
    id = call$id[failed], spcd = diagnostic_spcd,
    species = .mc_species_names(diagnostic_spcd),
    status = tree_status[failed],
    status_name = .mc_status_name(tree_status[failed]),
    stringsAsFactors = FALSE
  ) else NULL
  if (!call$include_status) {
    .mc_status_warning("merchandise", tree_status, call$spcd)
  }
  result <- list(
    logs = logs, scales = scales, trees = trees, residuals = residuals,
    defect_accounting = defect_accounting, values = values,
    run_metadata = metadata
  )
  if (call$include_status) result$diagnostics <- diagnostics
  class(result) <- "merch_result"
  result
}

.mc_validate_taper_map <- function(taper_map) {
  if (!is.data.frame(taper_map)) {
    stop(
      "taper_map must be a data frame. Supply species and model columns.",
      call. = FALSE
    )
  }
  if (!"model" %in% names(taper_map) ||
        !any(c("spcd", "species") %in% names(taper_map))) {
    stop(
      "taper_map must contain model and either spcd or species. ",
      "Add the missing column and try again.", call. = FALSE
    )
  }
  if (is.factor(taper_map$model)) taper_map$model <- as.character(taper_map$model)
  if (!is.character(taper_map$model) || anyNA(taper_map$model) ||
        any(!nzchar(taper_map$model))) {
    stop(
      "taper_map$model must contain nonmissing registered model ids. ",
      "Correct the ids and try again.", call. = FALSE
    )
  }
  code <- if ("spcd" %in% names(taper_map)) {
    .mc_resolve_species(taper_map$spcd, "taper_map$spcd", allow_na = FALSE)
  } else {
    .mc_resolve_species(
      taper_map$species, "taper_map$species", allow_na = FALSE
    )
  }
  if (all(c("spcd", "species") %in% names(taper_map))) {
    named_code <- .mc_resolve_species(
      taper_map$species, "taper_map$species", allow_na = FALSE
    )
    if (any(code != named_code)) {
      stop(
        "taper_map$spcd and taper_map$species do not agree. ",
        "Correct the species values and try again.", call. = FALSE
      )
    }
  }
  if (anyDuplicated(code)) {
    stop(
      "Each species may appear in taper_map only once. ",
      "Remove duplicate mappings and try again.", call. = FALSE
    )
  }
  problems <- check_taper_models(unique(taper_map$model))
  if (nrow(problems)) {
    stop(
      "taper_map contains an unregistered taper model id: ",
      problems$id[[1L]], ". Correct the mapping and try again.",
      call. = FALSE
    )
  }
  data.frame(
    spcd = as.integer(code), model = taper_map$model,
    stringsAsFactors = FALSE
  )
}

.mc_model_assumptions <- function(spcd, model, source, settings = NULL) {
  size <- length(model)
  if (!size) return(.mc_empty_assumptions())
  code <- if (is.null(spcd)) rep(NA_integer_, size) else spcd
  from_nvel <- identical(source, "nvel_default")
  result <- .mc_assumption_rows(
    "species_model", size = size, spcd = code,
    species = .mc_species_names(code), model = model, source = source,
    region = if (from_nvel) settings$region else NA_integer_,
    forest = if (from_nvel) settings$forest else NA_integer_,
    district = if (from_nvel) settings$district else NA_integer_
  )
  unique(result)
}

.mc_front_door <- function(dbh, ht, model, products, taper_map, species, spcd,
                           region, forest, district, preset_name, quiet, dots,
                           size) {
  quiet <- .mc_as_logical(quiet, "quiet", allow_na = FALSE)
  if (length(quiet) != 1L) {
    stop("quiet must be one TRUE/FALSE or 0/1 value. Supply one flag and try again.", call. = FALSE)
  }
  species_input <- species
  if (!is.null(model) && is.null(species_input) &&
        is.null(spcd)) {
    candidate <- if (is.factor(model)) as.character(model) else model
    registered <- is.character(candidate) && length(candidate) &&
      all(is.na(candidate) | has_taper_model(candidate))
    species_candidate <- NULL
    if (is.character(candidate) && length(candidate) && !registered) {
      species_candidate <- tryCatch(
        .mc_resolve_species(candidate, "species"),
        error = function(condition) NULL
      )
    }
    interpret_as_species <- .mc_numeric(candidate) ||
      !is.null(species_candidate) ||
      is.character(candidate) && length(candidate) &&
        all(is.na(candidate)) && is.null(products)
    if (interpret_as_species) {
      species_input <- model
      model <- NULL
    }
  }
  model_supplied <- !is.null(model)
  resolved_species <- if (is.null(species_input)) NULL else
    .mc_resolve_species(species_input, "species")
  resolved_spcd <- if (is.null(spcd)) NULL else
    .mc_resolve_species(spcd, "spcd")
  if (!is.null(resolved_species)) {
    resolved_species <- .mc_recycle(resolved_species, size, "species")
  }
  if (!is.null(resolved_spcd)) {
    resolved_spcd <- .mc_recycle(resolved_spcd, size, "spcd")
  }
  if (!is.null(resolved_species) && !is.null(resolved_spcd)) {
    left <- resolved_species
    right <- resolved_spcd
    differ <- !(is.na(left) & is.na(right)) & left != right
    differ[is.na(differ)] <- TRUE
    if (any(differ)) {
      stop(
        "Species and spcd identify different species. ",
        "Supply only one of them or make their values agree.", call. = FALSE
      )
    }
  }
  species_code <- if (is.null(resolved_species)) resolved_spcd else resolved_species
  if (!is.null(species_code)) {
    species_code <- .mc_recycle(species_code, size, "species")
  }
  validated_map <- if (is.null(taper_map) || model_supplied) NULL else
    .mc_validate_taper_map(taper_map)
  need_settings <- is.null(products) ||
    (is.null(model) && is.null(validated_map))
  settings <- if (need_settings) {
    .mc_region_settings(region, preset_name, species_code, forest, district)
  } else {
    NULL
  }
  if (!is.null(model)) {
    if (is.factor(model)) model <- as.character(model)
    model <- .mc_recycle(model, size, "model")
    model_source <- "caller_model"
  } else if (!is.null(validated_map)) {
    if (is.null(species_code)) {
      stop(
        "species is required. Supply an FIA code, symbol, or common name ",
        "so merchandiser can use taper_map.", call. = FALSE
      )
    }
    model <- validated_map$model[match(species_code, validated_map$spcd)]
    model_source <- "caller_taper_map"
  } else {
    if (is.null(species_code)) {
      stop(
        "species is required. Supply an FIA code, symbol, or common name ",
        "so merchandiser can choose a model.", call. = FALSE
      )
    }
    model <- rep(NA_character_, size)
    known <- !is.na(species_code)
    if (any(known)) {
      model[known] <- nvel_default_equation(
        settings$region, settings$forest, settings$district,
        species_code[known]
      )
    }
    model_source <- "nvel_default"
  }
  resolution_status <- integer(size)
  if (!identical(model_source, "caller_model")) {
    resolution_status[is.na(model)] <- 404L
  }
  products_assumed <- is.null(products)
  if (products_assumed) {
    if (is.null(settings$preset)) {
      stop(
        "No product preset is mapped to this region. Supply products or ",
        "choose a name from example_product_names().", call. = FALSE
      )
    }
    products <- .mc_preset_products(settings$preset)
  }
  expanded_products <- .validate_products_units(products, "imperial", check_units = FALSE)
  needs_ob <- any(expanded_products$diameter_basis == "ob" |
                    expanded_products$scale_bark_basis == "ob")
  bark_assumptions <- .mc_empty_assumptions()
  if (needs_ob && !"bark_ratio" %in% names(dots) && !is.null(species_code)) {
    model_ids <- as.character(model)
    known_models <- unique(model_ids[!is.na(model_ids)])
    known_models <- known_models[has_taper_model(known_models)]
    lacks_dob <- rep(FALSE, length(model_ids))
    if (length(known_models)) {
      capabilities <- model_capabilities(known_models)
      lacks_dob <- model_ids %in% capabilities$id[!capabilities$has_dob]
    }
    if (any(lacks_dob)) {
      ratios <- species_lookup(
        species_code, from = "spcd", to = "bark_ratio"
      )
      if (all(!is.na(ratios[lacks_dob]))) {
        dots$bark_ratio <- as.double(ratios)
        bark_assumptions <- .mc_assumption_rows(
          "bark_ratio", size = sum(lacks_dob),
          spcd = species_code[lacks_dob],
          species = .mc_species_names(species_code[lacks_dob]),
          value = ratios[lacks_dob], source = "species_table"
        )
        bark_assumptions <- unique(bark_assumptions)
      }
    }
  }
  model_assumptions <- .mc_model_assumptions(
    species_code, model, model_source, settings
  )
  preset_assumptions <- if (products_assumed) .mc_assumption_rows(
    "product_preset", preset = settings$preset, source = "preset_default",
    region = settings$region, forest = settings$forest,
    district = settings$district
  ) else .mc_empty_assumptions()
  assumptions <- .mc_bind_assumptions(
    model_assumptions, bark_assumptions, preset_assumptions
  )
  if ((need_settings || nrow(bark_assumptions)) && !quiet) {
    model_label <- paste(unique(model[!is.na(model)]), collapse = ", ")
    if (!nzchar(model_label)) model_label <- "none"
    preset_label <- if (products_assumed) {
      paste0("illustrative preset '", settings$preset, "'")
    } else {
      "caller-supplied products"
    }
    model_label <- paste0(
      switch(model_source,
        nvel_default = "assumed model(s) ",
        caller_taper_map = "taper-map model(s) ",
        caller_model = "caller-supplied model(s) "
      ),
      model_label
    )
    region_label <- if (is.null(settings)) "Using " else
      paste0("Using region '", settings$label, "' with ")
    bark_label <- if (nrow(bark_assumptions)) paste0(
      " Filled bark_ratio from the species table for species code(s) ",
      paste(bark_assumptions$spcd, collapse = ", "), "."
    ) else ""
    message(region_label, model_label, " and ", preset_label, ".",
            bark_label, " Set quiet = TRUE to silence this message.")
  }
  list(
    model = model, products = products, spcd = species_code, dots = dots,
    model_status = resolution_status, assumptions = assumptions,
    metadata = if (is.null(settings)) NULL else list(
      region = settings$label, nvel_region = settings$region,
      forest = settings$forest, district = settings$district,
      preset = if (products_assumed) settings$preset else NA_character_,
      model_assumed = identical(model_source, "nvel_default"),
      model_source = model_source, products_assumed = products_assumed,
      taper_map_ignored = model_supplied && !is.null(taper_map)
    )
  )
}

.mc_public_merchandise <- function(dbh, ht, model, products, id, spcd, age,
                                   pruned, defects, stump_ht, utilization_top,
                                   utilization_top_basis, utilization_height,
                                   curvature_scale, scaling, currency,
                                   algorithm, objective, unpriced, dots, units,
                                   status, species, taper_map, region, forest,
                                   district, preset_name, quiet) {
  size_inputs <- Filter(Negate(is.null), c(list(
    dbh = dbh, ht = ht, model = model, id = id, spcd = spcd, age = age,
    pruned = pruned, stump_ht = stump_ht, utilization_top = utilization_top,
    utilization_height = utilization_height, species = species
  ), dots))
  size <- .mc_common_size(size_inputs)
  front <- .mc_front_door(
    dbh, ht, model, products, taper_map, species, spcd, region, forest,
    district, preset_name, quiet, dots, size
  )
  arguments <- c(list(
    dbh = dbh, ht = ht, model = front$model, products = front$products,
    id = id, spcd = front$spcd, age = age, pruned = pruned,
    defects = defects, stump_ht = stump_ht,
    utilization_top = utilization_top,
    utilization_top_basis = utilization_top_basis,
    utilization_height = utilization_height,
    curvature_scale = curvature_scale, scaling = scaling,
    currency = currency, algorithm = algorithm, objective = objective,
    unpriced = unpriced, units = units, status = status,
    model_status = front$model_status, assumptions = front$assumptions
  ), front$dots)
  result <- do.call(.mc_merchandise, arguments)
  result$run_metadata$front_door <- front$metadata
  result
}

#' Select logs from trees using a product table
#'
#' Select the longest eligible logs in product order. Return cuts, tree totals, residual
#' intervals, deductions, scaled quantities, and recorded settings.
#'
#' @param dbh Numeric tree diameters at breast height outside bark. Required. Use inches with
#'   imperial units or centimeters with metric units.
#'
#' Must be positive and finite for a valid tree. Missing or nonfinite measurements produce a failed
#' tree record.
#' @param ht Numeric total tree heights above ground. Required. Use feet
#'   with imperial units or meters with metric units.
#'
#' These are total heights, not merchantable heights. Must be positive
#'   and finite for a valid tree. Missing heights are not fitted here.
#'
#' Complete them first with [complete_heights()].
#' @param model Character or factor vector of registered taper equation
#'   identifiers, for example `'F00FW2W202'`. Default `NULL` uses
#'   `taper_map`, or the geographic defaults if no map is supplied. An explicit model takes
#' precedence over `taper_map`.
#'
#' Unknown or missing
#'   identifiers do not request a replacement equation. Unitless.
#' @param products A product data frame, usually made with [product()]
#'   and [products()]. The fields, units, defaults, and accepted
#' measurement rules are described in [Product specification schema][product_schema]. Default
#'   `NULL` loads the
#'   selected illustrative preset.
#'
#' An explicitly supplied table takes
#'   precedence. Invalid or incomplete specifications stop the call. Example: `products =
#' example_products(name = 'pnw')`.
#' @param id Atomic vector of unique, nonmissing tree identifiers, such as
#'   `c('A-1', 'A-2')`. Default `NULL` assigns sequential row numbers after
#'   input lengths are resolved. Required when a nonempty defect table is
#'   supplied.
#'
#' Use the same identifier type in that table. Unitless.
#' @param spcd Species identifiers, supplied as positive whole-number
#'   inventory species codes, recognized symbols, common names, or
#'   scientific names. Default `NULL`. Supply species
#'   for automatic equation selection, species-restricted products, or
#'   green-weight calculations.
#'
#' Missing required species can leave a tree
#'   unresolved or a scaled quantity unavailable. Unitless.
#' @param age Numeric tree ages, default `NULL`. Use years and the same total-age or
#' breast-height-age convention as
#'   the product table. Required only when a relevant product limits age.
#'
#' Missing required age is a missing product attribute, not age zero.
#' @param pruned Logical or numeric `0`/`1` pruning indicators, default
#'   `NULL`. Example: `pruned = TRUE`. Required only when a relevant
#'   product requires a pruning state.
#'
#' Missing required values are missing
#'   product attributes. Unitless. No pruning height is inferred.
#' @param defects A defect data frame made with [defect()] or
#'   [defects_from_stoppers()]. Default `NULL` means no recorded defects
#'   are applied. An empty table has the same effect.
#'
#' Required fields are
#'   `id`, `from`, `to`, and `effect`, with conditional fields described
#'   in [defect()]. Heights use the call's height units. Missing fields or invalid
#'   record structure may stop the call.
#' @param stump_ht Numeric stump heights above ground, default `NULL`. Omission uses 1 foot for
#'   imperial calls or 0.3 meter for metric calls. These are the package's two defaults, not exact
#'   conversions of each
#'   other.
#'
#' Supplied values must satisfy
#'   `0 <= stump_ht < ht`. An explicit `NA` does not request the default. Supply a finite height
#' for each tree being calculated.
#' @param utilization_top Numeric minimum utilization diameters, default
#'   `NULL`. Units are inches or
#'   centimeters. When omitted, each tree uses the minimum small-end
#'   diameter and bark basis of its last reachable product before defects
#'   are applied.
#'
#' A supplied zero requests the tree tip. A missing value
#'   or a diameter without a usable crossing gives `utilization_top_invalid`.
#' @param utilization_top_basis One character value, `'ib'` for inside
#'   bark or `'ob'` for outside bark. Default `'ib'`. Used for an
#'   explicitly supplied `utilization_top`.
#'
#' The omitted-top calculation
#'   instead uses the selected product's bark basis. Missing is invalid. Example:
#' `utilization_top_basis = 'ib'`.
#'
#' Unitless.
#' @param utilization_height Numeric upper height limits above ground,
#'   default `NULL` for no additional height cap. Values use feet or
#'   meters and must be nonnegative and finite, or `NA` for no cap on that
#'   tree.  The effective top is the
#'   lowest of this cap, the diameter limit, total height, and the earliest
#'   recorded break.
#' @param curvature_scale A list with names `order` and `ratio_upper`, in
#'   that order. Default `NULL`. Required when a defect supplies a sweep
#'   or crook category or a product limits one.
#'
#' `order` is a unique,
#'   nonmissing character vector from least to greatest severity. `ratio_upper` is a numeric vector
#' of equal length, strictly increasing
#'   from zero to a final `Inf`, with finite intervening values. Both components are unitless.
#'
#' Current
#'   eligibility uses category order. The bounds are validated metadata,
#'   not an equation that converts measured sweep into a scale deduction. Invalid lists or unknown
#' product maximum categories stop the call.
#'
#' A defect category absent from the scale gives tree `unknown_curvature_category`.
#' @param report_also Character procedure names or a data frame of named
#'   measurements, default `NULL` for none. Missing definitions are invalid. Units follow each
#' definition.
#'
#' See [Scaling rules][scaling_rules] for fields, rounding, and examples. These
#'   measurements
#'   use the selected cuts without changing prices or selecting logs again.
#' @param scaling Deprecated alternative to `report_also`, with the same default
#'   `NULL`. Supply only one. Use `report_also = 'huber_ft3_ib'` in new code.
#' @param currency One nonempty character label for the currency of all
#'   supplied prices, default `NULL`. Required if any product has a price. Example: `currency =
#' 'USD'` for United States dollars.
#'
#' Missing is
#'   invalid when prices are present. This labels values without converting
#'   currencies or establishing whether prices are delivered or stumpage.
#' @param ... Uniquely named auxiliary vectors needed by the chosen taper
#'   equations, listed in the corresponding section. Omitted auxiliaries use model defaults
#'   where provided.
#'
#' A missing required auxiliary produces a failed tree. Unknown names, invalid types, and invalid
#' supplied domains are errors. These are equation inputs, not additional
#'   product fields or green-weight moisture overrides.
#' @param units One character value, `'imperial'` or `'metric'`. Default `'imperial'`. Imperial
#'   inputs use inches for diameters and
#'   feet for lengths and heights, with physical volume in cubic feet.
#'
#' Metric inputs use centimeters and meters, with physical volume in
#'   cubic meters. Measurement units are specified separately in `products`. Missing or other
#' spellings are invalid.
#'
#' Example: `units = 'metric'`.
#' @param status One logical or `0`/`1` value, default `FALSE`. `TRUE` adds status columns to tree
#'   and log tables and a `diagnostics`
#'   table, while suppressing tree-status warnings. `FALSE` reports
#'   nonzero tree statuses in warnings.
#'
#' Structural errors still stop either
#'   call. Missing is invalid. Example: `status = TRUE`.
#'
#' Unitless.
#' @param species An alternative to `spcd`, with the same accepted inputs
#'   and default `NULL`. Example: `species = 'Douglas-fir'`. If both
#'   arguments are supplied, they must resolve to the same species.
#'
#' A recognized species is also accepted in the third positional
#'   argument, but using the named argument avoids confusion with `model`. Missing behavior is the
#' same as for `spcd`. Unitless.
#' @param taper_map A data frame containing `model` and either `spcd` or
#'   `species`, with one equation assignment per species. Default `NULL`.
#'
#'   Species spellings are resolved and equation identifiers and species
#'   scopes are checked. Missing or duplicate assignments are invalid. A tree species absent from a
#' supplied map gets `model_unresolved` rather than
#'   an automatic replacement equation.
#'
#' Explicit `model` takes precedence. All fields are unitless.
#' @param preset One name returned by [example_product_names()], default `NULL`. Example: `preset =
#'   'pnw'`. Selects an illustrative product
#'   table if `products` is omitted, and provides geographic defaults if
#'   needed.
#'
#' Other names include `'us_south'` and `'douglas_fir'`. Missing or unknown
#'   names are invalid. These are examples to review, not verified product
#'   specifications.
#'
#' Unitless.
#' @param quiet One logical or `0`/`1` value, default `FALSE`. `TRUE` suppresses the informational
#'   defaults message. It does not
#'   suppress tree-status warnings or errors.
#'
#' Missing is invalid. Example: `quiet = TRUE`. Unitless.
#' @param region One numeric Forest Service region code or a preset name,
#'   default `NULL`. Used for source-library
#'   equation defaults when explicit equations are absent. Valid library
#'   region codes are 1 through 10.
#'
#' Omission uses the selected preset,
#'   otherwise the southern example for an entirely loblolly pine species
#'   vector and the Pacific Northwest example for other inputs. This is not an
#'   inference of geographic location. Missing is invalid.
#'
#' Unitless.
#' @param forest One numeric whole-number Forest Service forest code from
#'   zero through 99, default `NULL`. Omission uses the selected geographic settings. The Pacific
#' Northwest example
#'   uses 12 and the southern example uses 8.
#'
#' This is an agency code, not
#'   the name or identifier of the user's stand. Missing is invalid. Unitless.
#' @param district One numeric whole-number Forest Service district code
#'   from zero through 99, default `NULL`. Omission uses the selected settings, currently zero in
#' both example
#'   presets. Other numeric regions begin with forest and district zero
#'   unless overridden.
#'
#' Missing is invalid. Unitless.
#'
#' @section Input length and equation selection:
#' Tree measurements, identifiers, species, tree attributes, height limits, and equation
#' auxiliaries must have one common length. A length-one value is repeated for all trees.
#' Identifiers must still be unique after this step.
#'
#' Product, defect, mapping, and extra-scale tables have their own row counts. They are not
#' repeated to the inventory length.
#'
#' Explicit `model` and `products` bypass geographic defaults. `assumptions()` records
#' automatically selected settings.
#'
#' The species-to-equation map is checked against registered species scopes. Supplying a model
#' directly is not a substitute for that review.
#'
#' Products requiring outside-bark dimensions can trigger a species-reference bark ratio lookup
#' when the model lacks an outside-bark equation. The result records this assumption. Otherwise
#' supply `bark_ratio`
#' explicitly or choose a model that provides the needed dimensions.
#'
#' @section Equation auxiliaries:
#' Each supplied auxiliary is numeric unless stated otherwise, with one
#' value or one per tree. It is used only by models declaring that input.
#' Model-specific defaults and requirements can be inspected with
#' [get_taper_model()] and [check_taper_models()].
#' \describe{
#'   \item{`bark_ratio`}{Inside-bark diameter divided by outside-bark
#'     diameter, greater than zero and at most one. Omission uses the model's ratio or the
#' automatic species lookup
#'     listed in the corresponding section. If none is available, outside-bark calculations
#'     may be unavailable.
#'
#' Unitless.}
#'   \item{`upper_ht1`, `upper_d1`}{A paired upper-stem measurement height
#'     and diameter. Heights use feet or meters and diameters use inches
#'     or centimeters. Both must be positive.
#'
#'  Three-point equations requiring the pair fail
#'     when it is absent or incomplete. Some regional models use an upper
#'     height alone under their own declared input rules.}
#'   \item{`upper_ht2`, `upper_d2`}{Optional second measurement pair with
#'     the same types, units, and positive domains.
#'
#' Supply both where paired. Omission means no second measurement.}
#'   \item{`upper_bark`}{Character bark basis for upper measurements,
#'     `'ib'` or `'ob'`, defaulting to the model's convention. Flewelling upper measurements
#' default to inside bark.
#'
#' Example:
#'     `upper_bark = 'ib'`. A model may lack the conversion needed for
#'     an outside-bark measurement. Unitless.}
#'   \item{`form_class`}{Positive numeric form-class input for models
#'     that declare it.
#'
#' Unitless. Its definition
#'     and any default belong to the selected equation. Missing a required
#'     value prevents that model's calculation.}
#'   \item{`site_index`}{Positive numeric site-index height, in feet or
#'     meters, for models declaring it.
#'
#' The model determines species and reference-age convention. Do not
#'     assume that accepting this input means a particular call uses it.}
#'   \item{`basal_area`}{Positive numeric stand basal area, in square feet
#'     per acre or square meters per hectare. Available only where declared by the model.
#'
#' Omitted optional inputs
#'     use its defaults.}
#'   \item{`decay_class`, `cull`}{Where declared by a taper model,
#'     `decay_class` is a whole-number class from 1 through 5, and `cull`
#'     is a numeric percentage from zero through 100. These are model auxiliaries, with
#'     model-specific requirements. They do not replace `defects` or
#'     [apply_defect_pct()].
#'
#' The separate [biomass()] function
#'     has its own live-tree and deduction conventions.}
#' }
#'
#' @section How logs are selected and measured:
#' A tree enters the eligible product with the smallest priority number. Selection starts at
#' the stump, considers that product's feasible logs, and takes the longest nominal length. It
#' repeats until the product has no feasible log or reaches its count limit.
#'
#' If permitted, selection then proceeds to the next reachable lower-priority product. It
#' cannot skip sound wood to find a more favorable log farther up the stem. A cull interval
#' creates a new usable stem section and restarts the product sequence and counts.
#'
#' Product row order does not change this behavior.
#'
#' A supplied `min_boundary_length` permits additional logs ending at exact eligible
#' boundaries, including defect boundaries and crossings of log diameter limits. These are
#' reported as `length_kind = 'boundary'`. They may have fractional nominal lengths.
#'
#' Ordinary lengths are reported as `'ordinary'`.
#'
#' Physical length is nominal length plus trim. Product diameter limits apply at the start and
#' physical cut. Scaling rules use the nominal body and the chosen scaling rule's measurement
#' and rounding procedure.
#'
#' Doyle scaling uses diameter at the nominal end. Acceptance uses the physical cut, including
#' trim. Log-table end diameters retain their unrounded values.
#'
#' Located rot reduces physical volume and applies that same fractional
#' deduction to gross scaled quantity. Additional percentage deductions made
#' later reduce current net quantities without changing cuts. Gross and
#' net board-foot values may therefore have different rounding properties.
#' The package does not infer a regional defect-scaling method from a
#' product or grade label.
#'
#' Prices do not change the longest-log selection made by this function.
#' When any product has a price, unpriced logs receive zero value. A priced
#' log with unavailable scaled quantity has missing value. Use
#' [optimize_bucking()] to select cuts for value or physical net cubic
#' volume within the same product priorities.
#'
#' @return An object of class `merch_result`. See [Result tables][result_tables] for every column.
#' `logs` contains cuts and quantities, and `trees` retains all input trees.
#'
#' `scales` holds measurements, `residuals` holds unused sections, and `defect_accounting` holds
#' deductions. `values` records priced quantities, and `run_metadata` retains calculation choices.
#'
#' Tree rows retain input order. Logs are numbered within each tree in
#'   cutting order. Join related tables using `id` and, for logs, `log`.
#'
#' Measurements and quantities are numeric, counts and indices are
#'   integers, labels are character, and reconciliation flags are logical. Identifiers retain the
#' caller's atomic type.
#'
#' @section Status and missing values:
#' With `status = TRUE`, `trees$status` and `logs$status` are integer columns. `ok` means
#' the calculation completed. `no_feasible_log` means a valid tree produced no feasible log.
#'
#' Other failures require review before summarizing. A failed tree can retain partial output,
#' so its presence in a log table does not establish a complete result.
#'
#' The merchandising codes are:
#' \describe{
#'   \item{`no_entry_product`}{No product passed tree eligibility. Review species, diameter
#' classes, age, and pruning requirements.}
#'   \item{`defect_height_out_of_range`}{A defect height is outside
#'     the tree. Check its units and reference to ground.}
#'   \item{`defect_interval_invalid`}{Defect bounds are invalid.
#'
#' Check the interval and point-record conventions.}
#'   \item{`defect_percent_out_of_range`}{A percentage is outside
#'     zero through 100. Correct the recorded percentage.}
#'   \item{`model_unresolved`}{No equation was resolved for the
#'     species. Check the species and equation map.}
#'   \item{`unknown_defect_effect`}{The effect is unsupported.
#'
#' Use one of the documented operational treatments.}
#'   \item{`unknown_curvature_category`}{A category is absent from
#'     the ordered scale. Reconcile category labels.}
#'   \item{`missing_product_attribute`}{A product needs a missing
#'     tree attribute. Supply it or revise the product requirement.}
#'   \item{`utilization_top_invalid`}{The requested top cannot be
#'     resolved.
#'
#' Check the diameter, bark basis, and equation.}
#'   \item{`no_feasible_log`}{Valid zero volume under these
#'     specifications. Retain the tree in inventory counts.}
#'   \item{`scale_input_unavailable`}{A scaling measurement cannot be
#'     calculated. Check species, bark information, and measurement inputs.
#'
#' Missing scaled quantity and dependent value remain `NA`.
#' }
#'   \item{`profile_adapter_failure`}{Profile evaluation or the
#'     calculation plan failed. Inspect diagnostics and reproduce the tree
#'     separately before treating the result as usable.}
#'   \item{`reconciliation_failure`}{The volume accounting did not
#'     close. Investigate the tree and model before reporting totals.}
#' }
#'
#'
#' Some invalid defect and
#' product inputs are caught earlier and stop the call instead of becoming
#' tree codes. Equation failures retain the input, model, capability, and evaluation diagnoses
#' returned by [status_codes()].
#'
#' Before calculating totals, count statuses across the complete tree table. Do not silently drop
#' failed trees or interpret missing quantities as
#' zero. [product_summary()] defaults to excluding missing quantities from
#' sums.
#'
#' Use `na_action = 'propagate'` when an incomplete sum should remain
#' missing. A log-table summary alone cannot count trees having no logs.
#'
#' @usage
#'
#' ## Call signatures
#' merchandise(dbh, ht, model = NULL, products = NULL, id = NULL, spcd = NULL, age = NULL, pruned
#'   = NULL, defects = NULL, stump_ht = NULL, utilization_top = NULL, utilization_top_basis =
#'   'ib', utilization_height = NULL, curvature_scale = NULL, report_also = NULL, currency =
#'   NULL, ..., units = 'imperial', status = FALSE, species = NULL, taper_map = NULL, preset =
#'   NULL, quiet = FALSE, region = NULL, forest = NULL, district = NULL, scaling = NULL)
#' @examples
#' ## Calculate logs for the shipped tree list
#' result <- merchandise(dbh = example_trees$dbh,
#'                       ht = example_trees$ht,
#'                       model = example_trees$model,
#'                       species = example_trees$species,
#'                       products = example_products(name = 'pnw'),
#'                       status = TRUE)
#'
#' ## Inspect the result
#' plot(result, tree = 5)
#' @seealso [product_schema], [result_tables], [defect()], [optimize_bucking()],
#' [product_summary()], [stand_table()], [assumptions()], [status_codes()].
#' @export
merchandise <- function(dbh, ht, model = NULL, products = NULL, id = NULL,
                        spcd = NULL,
                        age = NULL, pruned = NULL, defects = NULL,
                        stump_ht = NULL, utilization_top = NULL,
                        utilization_top_basis = "ib",
                        utilization_height = NULL, curvature_scale = NULL,
                        report_also = NULL, currency = NULL, ...,
                        units = "imperial", status = FALSE, species = NULL,
                        taper_map = NULL, preset = NULL, quiet = FALSE,
                        region = NULL, forest = NULL, district = NULL, scaling = NULL) {
  scaling <- .mc_report_also(report_also, scaling, units)
  .mc_public_merchandise(
    dbh, ht, model, products, id, spcd, age, pruned, defects, stump_ht,
    utilization_top, utilization_top_basis, utilization_height,
    curvature_scale, scaling, currency, "cascade", "auto", "zero",
    list(...), units, status, species, taper_map, region, forest, district,
    preset, quiet
  )
}

#' Select log cuts for the greatest value or volume
#'
#' Select cuts that maximize value or net inside-bark cubic volume under the supplied product
#' specifications. Return the same result structure as [merchandise()].
#'
#' @inheritParams merchandise
#' @param objective One character value, `'auto'` (default), `'value'`, or
#'   `'net_cubic_ib'`. Automatic selection uses value if any product is priced,
#'   otherwise net inside-bark physical cubic volume. Unitless.
#'
#' Missing or
#'   unknown choices are errors. Example: `objective = 'value'`.
#' @param unpriced One character value, `'zero'` (default), `'exclude'`, or
#'   `'error'`. With a value objective these respectively assign zero value,
#'   remove unpriced products, or reject an incomplete price table. Removing
#'   every product is an error.
#'
#' Unitless. Missing choices are errors. Example: `unpriced = 'error'`.
#'
#' @section Product priority and cut selection:
#' The current product must be used while it has a feasible log and has not reached its per-
#' section count limit. A higher-paying lower-priority product cannot displace a feasible
#' current product. Progression to lower products requires `allow_lower_products = TRUE`.
#'
#' A cull section restarts product selection and counts. Sound wood cannot be skipped to reach
#' a better log. These constraints define the alternatives being compared.
#'
#' Physical cubic volume includes trim. Scaled quantity follows the product's nominal body and
#' scaling procedure. Located rot reduces those quantities before value is compared.
#'
#' Later percentage deductions do not trigger new cuts. Source-library length policies retain
#' their specified sequence.
#'
#' @section Numerical methods:
#' The optimization compares feasible continuations from each cutting height,
#' product, and count.
#' The procedure does not fit taper equations or estimate uncertainty in
#' prices or tree dimensions.
#' @return A `merch_result` with cuts, tree totals, scales, residuals, deductions,
#'   values, and recorded settings described in [Result tables][result_tables]. The saved
#'   `algorithm` label is `'dp'`, meaning cuts were compared for the selected
#'   objective. Physical cubic units follow `units`, and measurement units and
#'   currency follow the product table and call.
#' @seealso [merchandise()] for longest-log selection, [compare_bucking_prices()]
#' for price scenarios, [Product specification schema][product_schema] for product fields,
#'   [defect()] for
#'   restrictions, and [Result tables][result_tables] for returned columns.
#' @export
#' @usage
#'
#' ## Call signatures
#' optimize_bucking(dbh, ht, model = NULL, products = NULL, id = NULL, spcd = NULL, age = NULL,
#'   pruned = NULL, defects = NULL, stump_ht = NULL, utilization_top = NULL,
#'   utilization_top_basis = 'ib', utilization_height = NULL, curvature_scale = NULL, report_also
#'   = NULL, currency = NULL, objective = 'auto', unpriced = 'zero', ..., units = 'imperial',
#'   status = FALSE, species = NULL, taper_map = NULL, preset = NULL, quiet = FALSE, region =
#'   NULL, forest = NULL, district = NULL, scaling = NULL)
#' @examples
#' ## Optimize cuts under the example prices
#' result <- optimize_bucking(dbh = example_trees$dbh,
#'                            ht = example_trees$ht,
#'                            model = example_trees$model,
#'                            species = example_trees$species,
#'                            products = example_products(name = 'douglas_fir'),
#'                            currency = 'USD',
#'                            objective = 'value',
#'                            status = TRUE)
#'
#' ## Draw the selected cuts
#' plot(result, tree = 5)
#' @inheritSection merchandise Input length and equation selection
#' @inheritSection merchandise Equation auxiliaries
#' @inheritSection merchandise Status and missing values
optimize_bucking <- function(dbh, ht, model = NULL, products = NULL, id = NULL,
                             spcd = NULL,
                             age = NULL, pruned = NULL, defects = NULL,
                             stump_ht = NULL, utilization_top = NULL,
                             utilization_top_basis = "ib",
                             utilization_height = NULL,
                             curvature_scale = NULL, report_also = NULL,
                             currency = NULL, objective = "auto",
                             unpriced = "zero", ..., units = "imperial",
                             status = FALSE, species = NULL, taper_map = NULL,
                             preset = NULL, quiet = FALSE, region = NULL,
                             forest = NULL, district = NULL, scaling = NULL) {
  scaling <- .mc_report_also(report_also, scaling, units)
  .mc_public_merchandise(
    dbh, ht, model, products, id, spcd, age, pruned, defects, stump_ht,
    utilization_top, utilization_top_basis, utilization_height,
    curvature_scale, scaling, currency, "dp", objective, unpriced,
    list(...), units, status, species, taper_map, region, forest, district,
    preset, quiet
  )
}
