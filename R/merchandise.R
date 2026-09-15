#' Cut and scale logs from trees
#'
#' Select logs by product order or maximize their total value within each usable stem segment.
#' Cull and restriction boundaries restart the product order and log counts.
#' @param tree_id Identify the tree every result row came from. Required atomic vector, unitless.
#'   This identifier links every result row to its input tree. Values must be unique and nonmissing.
#' @param dbh Supply diameter at breast height outside bark. Numeric, inches, required.
#'   Must be greater than zero and at most 400.
#' @param ht Supply total tree height above ground. Numeric, feet, required.
#'   Must be greater than zero and at most 500.
#' @param spcd Identify each tree's species. Numeric inventory species code,
#'   unitless, required.
#' @param products Supply mill specifications. Their order sets cascade priority only. Made with
#'   [products()], required. Its measurement units are documented in [product()].
#' @param model Choose the taper equation for each tree. Character model identifier, unitless,
#'   default `NULL` selects by species. One model may be repeated for all trees.
#' @param taper_map Assign equations to species. Data frame with numeric `spcd` and character
#'   `model`, unitless, default `NULL` uses the package defaults. Explicit `model` takes precedence.
#' @param age Supply tree age for products with age limits. Numeric, years, default `NULL`.
#'   Products without age limits do not use this input.
#' @param pruned_ht Record clear wood from ground to this height. Numeric, feet, default `NULL`
#'   records no pruned section. A product requiring pruning must fit entirely below this height.
#' @param defects Supply sections that change cutting. Data frame from [defect()], with heights
#'   in feet and sweep in percent, default `NULL` records no defects.
#' @param stump_ht Leave this height below the first log. Numeric, feet, default `1`.
#' @param strategy Choose how to select cuts. Character, unitless, default `'cascade'` takes
#'   the longest feasible log in supplied product order at each step.
#'   `'optimize'` maximizes total value with any eligible product at each cut position.
#'   Product order has no effect on optimization, and every product must have a positive price.
#'   Equal values prefer fewer logs, then longer logs lower on the stem.
#'   Remaining ties prefer lower physical ends, then product names in byte order.
#'   Value differences within eight machine epsilons of the larger total are treated as ties.
#' @param quiet Silence the message about default equations. Logical, unitless, default `FALSE`.
#' @param ... Supply named auxiliary measurements required by the taper model. Every value is
#'   numeric, inches for diameters and feet for heights, except `upper_bark`. Default no
#'   auxiliary inputs.
#'   Supported inputs depend on the model:
#'   * `bark_ratio`: inside to outside diameter ratio.
#'   * `upper_ht1`, `upper_d1`, `upper_ht2`, `upper_d2`: paired upper measurements.
#'   * `upper_bark`: the one character input, `'ib'` or `'ob'`.
#'   * `form_class`: unitless.
#'   * `site_index`: feet.
#'   * `basal_area`: square feet per acre.
#'   * `decay_class`: whole-number class.
#'   * `cull`: percent.
#'
#'   Inspect [get_taper_model()] for defaults.
#' @details
#' Invalid stump heights return status 411 on their tree. Invalid pruning heights return
#'   status 408. Negative, nonfinite, and above-tree values are invalid, and stumps must
#'   be strictly below the tree height. Orphan defects are dropped before validation.
#' @return A `merch_result` list with four data frames and `call`, the inputs needed to replay it.
#'   Every table starts with `tree_id`, retaining the input identifier type.
#'
#'   `logs` contains:
#'   * `log`: number within the tree.
#'   * `product`: product name.
#'   * `start_height`, `end_height`: physical cut heights in feet.
#'   * `length`: nominal feet.
#'   * `scaling_length`: rounded feet.
#'   * `sed`, `led`: physical small and large end diameters in inches.
#'   * `scaling_diameter`: inches used by the scale rule.
#'   * `inside_bark`: logical diameter basis.
#'   * `scale`: quantity in `volume_unit`.
#'   * `volume_unit`: measurement name.
#'   * `value`: present when a product is priced, in the price's unit.
#'     Unpriced log values are missing.
#'
#'   `scaling_length` is nominal length rounded down to the product's `length_round`, in feet.
#'   Source board foot rules scale that length in whole or even feet according to `length_round`.
#'   Cubic, green weight, and cord scales measure the nominal body from the start
#'   to the nominal end.
#'   Their `scaling_length` is reported for the mill's records only.
#'
#'   `sed` and `led` are measured at the physical ends on the product's bark basis, recorded by
#'   `inside_bark`. Product diameter limits are tested against those measurements.
#'   `scaling_diameter` is the inside bark diameter used by the source board foot rule, rounded
#'   as that rule rounds it. It is `NA` for cubic, green weight, and cord products.
#'
#'   `residuals` has `start_height`, `end_height` (feet above ground), and `cause` (text).
#'
#'   `status` has only nonzero tree codes in `status`, with `name` and `description` (text).
#'   An empty status table means all trees completed cleanly.
#'
#'   `assumptions` has `assumption`, `spcd`, `model`, `value`, `unit`, `basis`,
#'   `product`, and `source`.
#'   Codes and labels are unitless, and `unit` describes each numeric value.
#' @importFrom utils head tail
#' @export
#' @usage
#' merchandise(
#'   tree_id,
#'   dbh,
#'   ht,
#'   spcd,
#'   products,
#'   model = NULL,
#'   taper_map = NULL,
#'   age = NULL,
#'   pruned_ht = NULL,
#'   defects = NULL,
#'   stump_ht = 1,
#'   strategy = 'cascade',
#'   quiet = FALSE,
#'   ...
#' )
#' @examples
#' ## Cut and scale the shipped trees with an explicit product
#' merchandise(tree_id = example_trees$tree_id,
#'             dbh = example_trees$dbh,
#'             ht = example_trees$ht,
#'             spcd = example_trees$spcd,
#'             products = product(product = 'domestic_saw',  ## name, unitless
#'                                min_length = 16,  ## feet
#'                                max_length = 40,  ## feet
#'                                trim = 1,  ## feet
#'                                min_sed = 6,  ## inches
#'                                volume_unit = 'scribner'))  ## board feet
merchandise <- function(
  tree_id, dbh, ht, spcd, products, model = NULL, taper_map = NULL, age = NULL,
  pruned_ht = NULL, defects = NULL, stump_ht = 1, strategy = "cascade", quiet = FALSE, ...
) {
  strategy <- .mc_scalar_choice(strategy, "strategy", c("cascade", "optimize"))
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    stop("quiet must be TRUE or FALSE.", call. = FALSE)
  }
  products <- .mc_validate_products(products)
  if (strategy == "optimize" && any(is.na(products$price) | products$price == 0)) {
    stop("Product '", products$product[which(is.na(products$price) | products$price == 0)[1]],
      "' needs a positive price for optimize.",
      call. = FALSE
    )
  }
  if (strategy == "optimize") {
    products <- products[order(products$product, method = "radix"), ]
    rownames(products) <- NULL
  }
  dots <- .capture_aux(list(...))
  not_numeric <- names(dots)[!vapply(dots, is.numeric, logical(1)) & names(dots) != "upper_bark"]
  if (length(not_numeric)) {
    stop("auxiliary input must be numeric: ", not_numeric[[1L]], call. = FALSE)
  }
  if (is.null(stump_ht))
    stop("stump_ht must contain numeric heights.", call. = FALSE)
  model_defaulted <- is.null(model)
  model <- .mc_default_models(spcd, model, taper_map)
  inputs <- Filter(Negate(is.null), c(list(
    tree_id = tree_id, dbh = dbh, ht = ht, spcd = spcd,
    model = model, age = age, pruned_ht = pruned_ht, stump_ht = stump_ht
  ), dots))
  size <- .common_size(inputs)
  inputs <- Map(function(x, name) .mc_recycle(x, size, name), inputs, names(inputs))
  .mc_require_atomic_id(inputs$tree_id, "tree_id")
  if (anyDuplicated(inputs$tree_id))
    stop("tree_id must be unique.", call. = FALSE)
  for (name in intersect(
    c("dbh", "ht", "spcd", "age", "pruned_ht", "stump_ht"),
    names(inputs)
  )) {
    inputs[[name]] <- .mc_as_double(inputs[[name]], name)
  }
  if (!is.character(inputs$model) && !is.factor(inputs$model)) {
    stop("model must contain taper model identifiers.", call. = FALSE)
  }
  inputs$model <- as.character(inputs$model)
  if (is.null(defects))
    defects <- .mc_empty_defects(inputs$tree_id)
  if (is.data.frame(defects) && "tree_id" %in% names(defects)) {
    unknown_tree <- !defects$tree_id %in% inputs$tree_id
    if (any(unknown_tree)) {
      warning("Dropped defects with unknown tree_id: ",
        paste(unique(defects$tree_id[unknown_tree]), collapse = ", "), ".", call. = FALSE
      )
      defects <- defects[!unknown_tree, , drop = FALSE]
      if (!nrow(defects))
        defects <- .mc_empty_defects(inputs$tree_id)
    }
  }
  checked <- validate_defects(defects, inputs$tree_id, inputs$ht, products)
  replay <- c(
    inputs[intersect(c(
      "tree_id", "dbh", "ht", "spcd", "model", "age", "pruned_ht",
      "stump_ht"
    ), names(inputs))], list(products = products, defects = defects, strategy = strategy),
    inputs[names(dots)]
  )
  if (!"age" %in% names(replay))
    replay["age"] <- list(NULL)
  if (!"pruned_ht" %in% names(replay))
    replay["pruned_ht"] <- list(NULL)
  aux <- inputs[names(dots)]
  if (any(!products$inside_bark) && !"bark_ratio" %in% names(aux)) {
    available <- unique(inputs$model[has_taper_model(inputs$model)])
    if (any(!.model_capabilities(available)$has_dob)) {
      aux$bark_ratio <- species_reference$bark_ratio[match(
        inputs$spcd,
        species_reference$spcd
      )]
    }
  }
  stem <- .mc_stem_inputs(inputs$spcd, inputs$model, aux, any(!products$inside_bark))
  aux <- stem$aux[setdiff(names(stem$aux), "spcd")]
  call <- list(
    size = size, id = inputs$tree_id, dbh = inputs$dbh, ht = inputs$ht, spcd = inputs$spcd,
    model = inputs$model, stump = inputs$stump_ht, aux = aux, measurement_system = "imperial",
    ob_required = any(!products$inside_bark)
  )
  recorded <- data.frame(
    tree_id = inputs$tree_id, assumption = rep("species_model", size),
    spcd = inputs$spcd, model = inputs$model, value = rep(NA_real_, size), unit = rep(
      NA_character_,
      size
    ), basis = rep(NA_character_, size), product = rep(NA_character_, size), source = rep(
      if (model_defaulted) "species_default" else "caller_model",
      size
    )
  )
  stump_assumption <- recorded
  stump_assumption$assumption <- rep("stump_height", size)
  stump_assumption$value <- inputs$stump_ht
  stump_assumption$unit <- rep("feet", size)
  stump_assumption$source <- rep("stump_ht", size)
  recorded <- rbind(recorded, stump_assumption)
  if (!is.null(aux$bark_ratio)) {
    bark_assumption <- stump_assumption
    bark_assumption$assumption <- rep("bark_ratio", size)
    bark_assumption$value <- rep_len(aux$bark_ratio, size)
    bark_assumption$unit <- rep("diameter_ratio", size)
    bark_assumption$basis <- rep("inside_over_outside", size)
    bark_assumption$source <- rep(if ("bark_ratio" %in% names(dots)) "caller" else
                                    "species_reference", size)
    recorded <- rbind(recorded, bark_assumption)
  }
  if (model_defaulted && !quiet && size) {
    message("Selected taper equations by species. Use assumptions() to review them.")
  }
  empty_logs <- data.frame(
    tree_id = inputs$tree_id[FALSE], log = integer(), product = character(),
    start_height = double(), end_height = double(), length = double(),
    scaling_length = double(),
    sed = double(), led = double(), scaling_diameter = double(), inside_bark = logical(),
    scale = double(), volume_unit = character()
  )
  if (any(!is.na(products$price)))
    empty_logs$value <- double()
  residuals <- data.frame(
    tree_id = inputs$tree_id[FALSE], start_height = double(), end_height = double(),
    cause = character()
  )
  residual_rows <- list()
  result_status <- integer(size)
  result_status[!is.finite(inputs$dbh) | !is.finite(inputs$ht) | !is.finite(inputs$spcd)] <- 1L
  result_status[result_status == 0 & (inputs$dbh <= 0 | inputs$dbh > 400)] <- 2L
  result_status[result_status == 0 & (inputs$ht <= 0 | inputs$ht > 500)] <- 3L
  bad_stump <- !is.finite(inputs$stump_ht) | inputs$stump_ht < 0 |
    inputs$stump_ht >= inputs$ht
  result_status[result_status == 0 & bad_stump] <- 411L
  invalid_species <- !inputs$spcd %in% species_reference$spcd
  result_status[result_status == 0 & invalid_species] <- 7L
  result_status[result_status == 0 & (is.na(inputs$model) | !nzchar(inputs$model))] <- 404L
  result_status[result_status == 0 & !has_taper_model(inputs$model)] <- 50L
  if (!is.null(inputs$pruned_ht)) {
    bad_pruning <- !is.finite(inputs$pruned_ht) | inputs$pruned_ht < 0 |
      inputs$pruned_ht > inputs$ht
    result_status[result_status == 0 & bad_pruning] <- 408L
  }
  resolved <- .resolve_models(inputs$model, c(list(spcd = inputs$spcd), aux), result_status)
  result_status <- resolved$status
  scope_status <- result_status == 52L
  for (row in seq_len(nrow(checked))) {
    i <- match(checked$tree_id[row], inputs$tree_id)
    if (result_status[i] %in% c(0L, 52L) && checked$status[row] != 0)
      result_status[i] <- checked$status[row]
  }
  cached_cuts <- new.env(parent = emptyenv())
  prepared <- list()
  task <- integer(size)
  add_residual <- function(i, start_height, end_height, cause) {
    if (is.finite(start_height) && is.finite(end_height) && start_height < end_height) {
      residual_rows[[length(residual_rows) + 1L]] <<- list(
        tree = i, start_height = start_height, end_height = end_height, cause = cause
      )
    }
  }
  for (i in seq_len(size)) {
    if (!result_status[i] %in% c(0L, 52L))
      next
    allowed <- .mc_eligible_products(products, inputs, i)
    tree_ends <- checked$start_height[checked$tree_id == inputs$tree_id[i] &
                                        checked$effect == "end"]
    endpoint <- min(c(inputs$ht[i], tree_ends))
    bottom <- min(inputs$stump_ht[i], endpoint)
    add_residual(i, 0, bottom, "stump")
    if (!length(allowed)) {
      result_status[i] <- 400L
      add_residual(i, bottom, endpoint, "no_entry_product")
      add_residual(i, endpoint, inputs$ht[i], "end")
      next
    }
    one <- call
    one$size <- 1
    for (name in c("id", "dbh", "ht", "spcd", "model", "stump")) one[[name]] <-
      call[[name]][i]
    one$aux <- lapply(call$aux, function(x) {
      if (length(x) == 1) x else x[i]
    })
    selected_model <- resolved$models[[match(one$model, resolved$dictionary)]]
    declared <- c(selected_model$inputs$required, selected_model$inputs$optional)
    one$aux <- one$aux[intersect(names(one$aux), declared)]
    tree_defects <- checked[checked$tree_id == one$id, , drop = FALSE]
    pruning <- if (is.null(inputs$pruned_ht)) 0 else inputs$pruned_ht[i]
    key_defects <- tree_defects[setdiff(names(tree_defects), "tree_id")]
    rownames(key_defects) <- NULL
    key <- paste(as.character(serialize(list(
      one[setdiff(names(one), "id")], allowed, key_defects, pruning
    ), NULL)), collapse = "")
    if (!exists(key, cached_cuts, inherits = FALSE)) {
      prepared[[length(prepared) + 1L]] <- list(
        call = one, allowed = allowed, defects = tree_defects, pruned_ht = pruning
      )
      cached_cuts[[key]] <- length(prepared)
    }
    task[i] <- cached_cuts[[key]]
  }
  prepared <- .mc_prepare_batch(prepared, products, strategy)
  answers <- .mc_cut_batch(prepared, products, strategy)
  active <- which(task > 0L)
  answer_status <- vapply(answers, `[[`, integer(1), "status")[task[active]]
  result_status[active] <- ifelse(answer_status == 0L & scope_status[active], 52L, answer_status)
  expand <- function(field) {
    counts <- vapply(answers, function(x) nrow(x[[field]]), integer(1))
    source <- mc_expand_index_cpp(rep(seq_along(answers), counts), task[active])
    values <- do.call(rbind, lapply(answers, `[[`, field))
    list(values = values[source$source, , drop = FALSE], tree = active[source$tree])
  }
  logs <- empty_logs
  if (length(active)) {
    combined <- expand("logs")
    if (nrow(combined$values)) {
      logs <- combined$values
      logs$tree_id <- unname(inputs$tree_id[combined$tree])
    }
    combined <- expand("residuals")
    if (nrow(combined$values)) {
      residual_rows[[length(residual_rows) + 1L]] <- c(
        list(tree = combined$tree), as.list(combined$values)
      )
    }
  }
  rownames(logs) <- NULL
  if (length(residual_rows)) {
    column <- function(name) unlist(lapply(residual_rows, `[[`, name), use.names = FALSE)
    residuals <- data.frame(
      tree_id = inputs$tree_id[column("tree")], start_height = column("start_height"),
      end_height = column("end_height"), cause = column("cause")
    )
  }
  if (nrow(residuals))
    residuals <- residuals[order(
      match(residuals$tree_id, inputs$tree_id),
      residuals$start_height
    ), ]
  rownames(residuals) <- NULL
  codes <- status_codes()
  result_status[!result_status %in% codes$status] <- 54L
  bad <- which(result_status != 0)
  at <- match(result_status[bad], codes$status)
  status <- data.frame(
    tree_id = inputs$tree_id[bad], status = result_status[bad], name = codes$name[at],
    description = codes$description[at]
  )
  structure(list(
    logs = logs, residuals = residuals, status = status, assumptions = recorded,
    call = replay
  ), class = "merch_result")
}

.mc_eligible_products <- function(products, inputs, i) {
  good <- inputs$dbh[i] >= products$min_dbh & (is.na(products$max_dbh) |
                                                 inputs$dbh[i] < products$max_dbh)
  for (p in seq_len(nrow(products))) {
    if (length(products$spcd[[p]]))
      good[p] <- good[p] && inputs$spcd[i] %in% products$spcd[[p]]
    if (!is.na(products$min_age[p]) || !is.na(products$max_age[p])) {
      age <- if (is.null(inputs$age))
        NA_real_ else inputs$age[i]
      good[p] <- good[p] && is.finite(age) && (is.na(products$min_age[p]) ||
                                                 age >= products$min_age[p]) &&
        (is.na(products$max_age[p]) || age < products$max_age[p])
    }
    if (products$requires_pruned[p]) {
      pruned <- if (is.null(inputs$pruned_ht))
        0 else inputs$pruned_ht[i]
      good[p] <- good[p] && is.finite(pruned) && pruned > inputs$stump_ht[i]
    }
  }
  which(good)
}

.mc_cpp_products <- function(products, pruned_ht = 0) {
  rules <- c(
    scribner = "scribner_decimal_c_whole_40", international = "international_1_4_4ft",
    doyle = "doyle_formula", cubic = "cubic", green_ton = "cubic", cord = "cubic"
  )
  rule <- unname(rules[products$volume_unit])
  rule[products$volume_unit == "scribner" & products$split_scale] <-
    "scribner_decimal_c_split_20"
  list(
    product = products$product, min_tick = as.integer(ceiling(products$min_length * 2)),
    max_tick = as.integer(floor(products$max_length * 2)), trim = products$trim,
    min_sed = products$min_sed,
    max_sed = products$max_sed, min_led = products$min_led,
    max_led = products$max_led, diameter_basis = as.integer(!products$inside_bark),
    max_sweep = products$max_sweep, max_logs = as.integer(ifelse(is.na(products$max_logs),
      -1, products$max_logs
    )), requires_pruned = as.integer(products$requires_pruned),
    pruned_ht = pruned_ht, scale_rule = match(rule, .mc_scale_rules) - 1L,
    measurement_quantity = match(products$volume_unit,
      c("cubic", "green_ton", "cord"),
      nomatch = 0L
    ), scale_basis = as.integer(!products$inside_bark &
      products$volume_unit %in% c(
        "cubic", "cord"
      )), diameter_round = unname(c(
      default = 0L,
      down = 2L, nearest = 3L, none = 1L
    )[products$round]), length_round = products$length_round,
    cord_fraction = products$cord_solid_fraction, price = ifelse(is.na(products$price), 0,
      products$price
    ), price_quantity = products$price_per, scribner_factor = .mc_scribner_factor,
    scribner_exception = as.integer(.mc_scribner_exception),
    intl_quadratic = unname(.mc_intl14_constants[["quadratic"]]),
    intl_linear = unname(.mc_intl14_constants[["linear"]]),
    intl_adjustment = unname(.mc_intl14_constants[["adjustment"]])
  )
}

.mc_prepare_batch <- function(prepared, products, strategy) {
  sessions <- list()
  on.exit({
    for (session in sessions) mc_provider_close(session$pointer)
  }, add = TRUE)
  models <- vapply(prepared, function(x) x$call$model, character(1))
  for (model in unique(models)) {
    rows <- which(models == model)
    work <- prepared[rows]
    call <- work[[1L]]$call
    call$size <- length(work)
    for (name in c("id", "dbh", "ht", "spcd", "model", "stump")) {
      call[[name]] <- unlist(lapply(work, function(x) x$call[[name]]), use.names = FALSE)
    }
    call$aux <- lapply(names(call$aux), function(name) {
      unlist(lapply(work, function(x) x$call$aux[[name]]), use.names = FALSE)
    })
    names(call$aux) <- names(work[[1L]]$call$aux)
    sessions[[model]] <- .mc_open_provider(call$model)
    session <- sessions[[model]]
    roots <- vector("list", length(work))
    if (strategy == "cascade") {
      top_product <- vapply(work, function(x) {
        x$allowed[which.min(products$min_sed[x$allowed])]
      }, integer(1))
      target <- products$min_sed[top_product]
      crossing <- which(target > 0)
      roots[crossing] <- .mc_all_crossings(
        session, call, crossing, target[crossing],
        ifelse(products$inside_bark[top_product[crossing]], "ib", "ob"),
        call$stump[crossing], call$ht[crossing]
      )
    }
    geometry <- .mc_prepare_segments(work, roots)
    for (i in seq_along(work)) {
      one <- work[[i]]
      work[[i]] <- .mc_prepare_tree(
        one$call, products, one$allowed, one$pruned_ht, geometry[[i]]
      )
    }
    points <- .mc_grid_points(work, products)
    profile <- .mc_query_points(session, call, points$tree, points$height)
    offsets <- c(0L, cumsum(tabulate(points$tree, nbins = length(work))))
    for (i in seq_along(work)) {
      one <- work[[i]]
      one$profile <- lapply(profile, `[`, seq.int(offsets[i] + 1L, offsets[i + 1L]))
      failed <- one$profile$status[!one$profile$status %in% c(0, 52, 102)]
      if (length(failed)) {
        one <- list(
          logs = data.frame(), residuals = as.data.frame(one$residuals), status = failed[1]
        )
      }
      prepared[[rows[i]]] <- one
    }
  }
  active <- which(vapply(prepared, function(x) is.null(x$status), logical(1)))
  species <- vapply(prepared[active], function(x) x$call$spcd, numeric(1))
  green <- products$volume_unit == "green_ton"
  for (spcd in unique(species)) {
    weights <- rep(1, nrow(products))
    status <- 0L
    if (any(green)) {
      weight <- mc_provider_green_weight(1, as.integer(spcd), 0L, 1L)
      status <- weight$status
      weights[green] <- weight$value / 2000
    }
    for (i in active[species == spcd]) {
      if (status != 0L) {
        prepared[[i]] <- list(
          logs = data.frame(), residuals = as.data.frame(prepared[[i]]$residuals), status = 7L
        )
      } else {
        prepared[[i]]$weights <- weights
      }
    }
  }
  prepared
}

.mc_prepare_segments <- function(work, roots) {
  size <- length(work)
  stump <- vapply(work, function(x) x$call$stump, numeric(1))
  ht <- vapply(work, function(x) x$call$ht, numeric(1))
  endpoint <- top <- numeric(size)
  defects <- lapply(work, `[[`, "defects")
  for (i in seq_along(work)) {
    endpoint[i] <- ht[i]
    ends <- defects[[i]]$start_height[defects[[i]]$effect == "end"]
    if (length(ends)) endpoint[i] <- min(endpoint[i], ends)
    top[i] <- endpoint[i]
    if (!is.null(roots[[i]])) {
      if (length(roots[[i]])) {
        top[i] <- min(top[i], tail(roots[[i]], 1))
      } else {
        top[i] <- stump[i]
      }
    }
    top[i] <- max(stump[i], top[i])
    defects[[i]]$end_height[is.na(defects[[i]]$end_height)] <- ht[i]
  }
  column <- function(name) unlist(lapply(defects, `[[`, name), use.names = FALSE)
  defect_tree <- rep(seq_len(size), vapply(defects, nrow, integer(1)))
  effect <- column("effect")
  from <- column("start_height")
  to <- column("end_height")
  cuts <- which(effect %in% c("cull", "restrict"))
  tree <- c(seq_len(size), seq_len(size), defect_tree[cuts], defect_tree[cuts])
  height <- c(stump, top, from[cuts], to[cuts])
  keep <- height >= stump[tree] & height <= top[tree]
  tree <- tree[keep]
  height <- height[keep]
  ordering <- order(tree, height)
  tree <- tree[ordering]
  height <- height[ordering]
  keep <- c(TRUE, diff(tree) != 0L | diff(height) != 0)
  tree <- tree[keep]
  height <- height[keep]
  starts <- which(diff(tree) == 0L)
  lo <- height[starts]
  hi <- height[starts + 1L]
  segment_tree <- tree[starts]
  groups <- split(seq_along(lo), factor(segment_tree, levels = seq_len(size)))
  cull <- logical(length(lo))
  for (r in which(effect == "cull")) {
    rows <- groups[[defect_tree[r]]]
    cull[rows] <- cull[rows] | (from[r] < hi[rows] & to[r] > lo[rows])
  }
  top_rows <- which(top < endpoint)
  end_rows <- which(endpoint < ht)
  residual_tree <- c(segment_tree[cull], top_rows, end_rows)
  residuals <- list(
    start_height = c(lo[cull], top[top_rows], endpoint[end_rows]),
    end_height = c(hi[cull], endpoint[top_rows], ht[end_rows]),
    cause = c(rep("cull", sum(cull)), rep("top", length(top_rows)), rep("end", length(end_rows)))
  )
  residual_groups <- split(
    seq_along(residual_tree), factor(residual_tree, levels = seq_len(size))
  )
  lapply(seq_len(size), function(i) {
    rows <- groups[[i]]
    rows <- rows[!cull[rows]]
    list(endpoint = endpoint[i], top = top[i], defects = defects[[i]],
         segments = list(lo = lo[rows], hi = hi[rows]),
         residuals = lapply(residuals, `[`, residual_groups[[i]]))
  })
}

.mc_grid_points <- function(work, products) {
  column <- function(field, name) {
    unlist(lapply(work, function(x) x[[field]][[name]]), use.names = FALSE)
  }
  sizes <- vapply(work, function(x) length(x$segments$lo), integer(1))
  tree <- rep(seq_along(work), sizes)
  lo <- column("segments", "lo")
  hi <- column("segments", "hi")
  aligned_trim <- vapply(work, function(x) {
    all(products$trim[x$allowed] * 2 == floor(products$trim[x$allowed] * 2))
  }, logical(1))
  aligned <- which(lo * 2 == floor(lo * 2) & aligned_trim[tree])
  sizes <- floor((hi[aligned] - lo[aligned]) * 2) + 1L
  segment <- rep(aligned, sizes)
  heights <- lo[segment] + (sequence(sizes) - 1L) / 2
  sizes <- vapply(work, function(x) length(x$points), integer(1))
  point_tree <- c(rep(seq_along(work), sizes), tree[segment])
  heights <- c(unlist(lapply(work, `[[`, "points"), use.names = FALSE), heights)
  ordering <- order(point_tree, heights)
  point_tree <- point_tree[ordering]
  heights <- heights[ordering]
  keep <- c(TRUE, diff(point_tree) != 0L | diff(heights) != 0)
  list(tree = point_tree[keep], height = heights[keep])
}

.mc_prepare_tree <- function(call, products, allowed, pruned_ht, geometry) {
  endpoint <- geometry$endpoint
  top <- geometry$top
  defects <- geometry$defects
  segments <- geometry$segments
  residuals <- geometry$residuals
  points <- c(0, call$stump, call$ht, top, endpoint, defects$start_height, defects$end_height)
  for (r in seq_along(segments$lo)) {
    grid_aligned <- segments$lo[r] * 2 == floor(segments$lo[r] * 2) &&
      all(products$trim[allowed] * 2 == floor(products$trim[allowed] * 2))
    nodes <- numeric()
    if (!grid_aligned) {
      nodes <- segments$lo[r]
      cursor <- 1L
      while (cursor <= length(nodes)) {
        a <- nodes[cursor]
        added <- numeric()
        for (p in allowed) {
          low <- ceiling(products$min_length[p] * 2)
          high <- min(floor(products$max_length[p] * 2), floor((segments$hi[r] - a -
                                                                  products$trim[p] + 1e-9) *
                                                                 2))
          if (high < low)
            next
          nominal <- seq.int(low, high) / 2
          added <- c(added, a + nominal + products$trim[p])
          points <- c(points, a + nominal, a + nominal + products$trim[p])
          if (products$volume_unit[p] == "scribner") {
            convention <- if (products$split_scale[p])
              "split_20" else "whole_40"
            for (len in unique(floor(nominal / products$length_round[p]) *
                                 products$length_round[p])) {
              points <- c(points, a + cumsum(.mc_nvel_segments(
                len, convention, products$length_round[p]
              )))
            }
          }
        }
        nodes <- sort(unique(c(nodes, added)))
        if (length(nodes) > 50000)
          stop("The cut grid is too large for this tree.", call. = FALSE)
        cursor <- cursor + 1L
      }
    }
    points <- c(points, nodes, segments$hi[r])
  }
  points <- sort(unique(points[is.finite(points) & points >= 0 & points <= call$ht]))
  active_defects <- defects[defects$effect != "end", ]
  list(call = call, allowed = allowed, defects = defects, pruned_ht = pruned_ht,
       segments = segments, residuals = residuals, points = points,
       active_defects = active_defects)
}

.mc_cut_batch <- function(prepared, products, strategy) {
  answers <- prepared
  active <- which(vapply(prepared, function(x) is.null(x$status), logical(1)))
  if (!length(active)) return(answers)
  work <- prepared[active]
  column <- function(field, name) {
    unlist(lapply(work, function(x) x[[field]][[name]]), use.names = FALSE)
  }
  offsets <- function(field) {
    as.integer(c(0, cumsum(vapply(work, function(x) {
      if (is.list(x[[field]])) length(x[[field]][[1L]]) else length(x[[field]])
    }, integer(1)))))
  }
  core <- mc_buck_cpp(
    .mc_cpp_products(products, vapply(work, `[[`, numeric(1), "pruned_ht")),
    0.5, 1L, as.integer(strategy == "optimize"), 1L, integer(length(work)),
    offsets("allowed"), as.integer(unlist(lapply(work, `[[`, "allowed")) - 1L),
    offsets("segments"), column("segments", "lo"), column("segments", "hi"),
    integer(length(work) + 1L), numeric(), offsets("profile"),
    column("profile", "height"), column("profile", "dib"), column("profile", "dob"),
    column("profile", "cum_ib"), column("profile", "cum_ob"),
    offsets("active_defects"), column("active_defects", "start_height"),
    column("active_defects", "end_height"),
    match(column("active_defects", "effect"), c("cull", "restrict", "end", "sweep")),
    column("active_defects", "percent"),
    match(column("active_defects", "product"), products$product, nomatch = 0L) - 1L,
    unlist(lapply(work, `[[`, "weights")), as.integer(threads())
  )
  names(core$residuals)[names(core$residuals) == "from"] <- "start_height"
  names(core$residuals)[names(core$residuals) == "to"] <- "end_height"
  green <- which(products$volume_unit[core$logs$product_index] == "green_ton")
  species <- vapply(work, function(x) x$call$spcd, numeric(1))[core$logs$tree[green]]
  for (spcd in unique(species)) {
    rows <- green[species == spcd]
    weight <- mc_provider_green_weight(
      core$logs$log_gross_cubic_ib[rows], rep(as.integer(spcd), length(rows)),
      integer(length(rows)), 1L
    )
    core$native_gross_scale[rows] <- weight$value / 2000
  }
  for (i in seq_along(work)) {
    rows <- which(core$logs$tree == i)
    one <- list(logs = core$logs[rows, ], native_gross_scale = core$native_gross_scale[rows],
                residuals = core$residuals[core$residuals$tree == i, ], status = core$status[i])
    answers[[active[i]]] <- .mc_finish_tree(work[[i]], products, one)
  }
  answers
}

.mc_finish_tree <- function(prepared, products, core) {
  call <- prepared$call
  profile <- prepared$profile
  defects <- prepared$defects
  residuals <- as.data.frame(prepared$residuals)
  raw <- core$logs
  p <- raw$product_index
  diameter_height <- raw$nominal_end_height
  scribner_rows <- products$volume_unit[p] == "scribner"
  for (r in which(scribner_rows)) {
    scaling_length <- floor(raw$nominal_length[r] / products$length_round[p[r]]) *
      products$length_round[p[r]]
    convention <- if (products$split_scale[p[r]]) "split_20" else "whole_40"
    diameter_height[r] <- raw$start_height[r] + sum(.mc_nvel_segments(
      scaling_length, convention, products$length_round[p[r]]
    ))
  }
  scaling_diameter <- profile$dib[match(diameter_height, profile$height)]
  board <- products$volume_unit[p] %in% c("scribner", "international", "doyle")
  for (r in seq_len(nrow(raw))) {
    if (!board[r]) {
      scaling_diameter[r] <- NA_real_
      next
    }
    if (products$round[p[r]] == "down")
      scaling_diameter[r] <- floor(scaling_diameter[r])
    if (products$round[p[r]] == "nearest")
      scaling_diameter[r] <- floor(scaling_diameter[r] + 0.5)
    if (products$volume_unit[p[r]] != "doyle")
      scaling_diameter[r] <- floor(scaling_diameter[r] + 0.5)
  }
  logs <- data.frame(
    tree_id = rep(call$id, nrow(raw)), log = raw$log, product = products$product[p],
    start_height = raw$start_height, end_height = raw$end_height,
    length = raw$nominal_length,
    scaling_length = floor(raw$nominal_length / products$length_round[p]) *
      products$length_round[p],
    sed = ifelse(products$inside_bark[p], raw$sed_ib, raw$sed_ob), led = ifelse(
      products$inside_bark[p],
      raw$led_ib, raw$led_ob
    ), scaling_diameter = scaling_diameter, inside_bark = products$inside_bark[p],
    scale = core$native_gross_scale, volume_unit = products$volume_unit[p]
  )
  doyle_logs <- which(logs$volume_unit == "doyle")
  logs$scale[doyle_logs] <- pmax(logs$scaling_diameter[doyle_logs] - 4, 0)^2 *
    logs$scaling_length[doyle_logs] / 16
  if (any(!is.na(products$price)))
    logs$value <- logs$scale * products$price[p] / products$price_per[p]
  trim <- raw$end_height > raw$nominal_end_height
  if (any(trim))
    residuals <- rbind(residuals, data.frame(
      start_height = raw$nominal_end_height[trim], end_height = raw$end_height[trim],
      cause = "trim"
    ))
  unused <- core$residuals
  unused$cause <- c("short_remainder", "diameter_limit", "short_remainder")[unused$cause]
  for (r in seq_len(nrow(unused))) {
    restricted <- any(defects$effect == "restrict" & defects$start_height < unused$end_height[r] &
                        defects$end_height >
                          unused$start_height[r])
    if (restricted)
      unused$cause[r] <- "restricted"
  }
  if (nrow(unused))
    residuals <- rbind(residuals, unused[c("start_height", "end_height", "cause")])
  final_status <- if (core$status != 0) {
    core$status
  } else if (!nrow(logs)) {
    410L
  } else {
    0L
  }
  list(logs = logs, residuals = residuals, status = final_status)
}
