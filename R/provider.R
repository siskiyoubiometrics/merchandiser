.mc_open_provider <- function(model) {
  model <- as.character(model)
  dictionary <- unique(model[!is.na(model)])
  session <- mc_provider_open(dictionary)
  session$dictionary <- dictionary
  session$model_index <- match(model, dictionary)
  session
}

.mc_provider_aux <- function(call) {
  aux <- call$aux
  if (!is.null(call$spcd)) aux <- c(list(spcd = call$spcd), aux)
  aux
}

.mc_subset_aux <- function(aux, rows) {
  lapply(aux, `[`, rows)
}

.mc_first_bad_status <- function(status) {
  accepted <- status %in% c(0L, 52L, 102L)
  bad <- status[!accepted]
  if (length(bad)) bad[[1L]] else 0L
}

.mc_r_crossing_basis <- function(arguments, target, lower, upper, basis) {
  count <- length(target)
  step <- if (identical(arguments$units, "imperial")) 1 / 192 else 0.0254 / 16
  intervals <- pmax(1L, as.integer(ceiling(pmax(upper - lower, 0) / step)))
  grid_count <- intervals + 1L
  request <- rep(seq_len(count), grid_count)
  offset <- sequence(grid_count) - 1L
  height <- lower[request] + offset * step
  ends <- cumsum(grid_count)
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  height[ends] <- upper
  args <- arguments
  for (name in setdiff(names(args), c("units", "status"))) {
    args[[name]] <- args[[name]][request]
  }
  args$h <- height
  diameter <- if (identical(basis, "ib")) {
    do.call(dib, args)
  } else {
    do.call(dob, args)
  }
  roots <- rep(list(numeric()), count)
  status <- integer(count)
  bracket_request <- bracket_lower <- bracket_upper <- bracket_difference <-
    vector("list", count)
  for (i in seq_len(count)) {
    positions <- seq.int(starts[i], ends[i])
    current_status <- diameter$status[positions]
    bad <- which(!current_status %in% c(0L, 52L))
    if (length(bad)) {
      status[i] <- current_status[bad[1L]]
      next
    }
    difference <- diameter$value[positions] - target[i]
    exact <- difference == 0
    if (any(exact)) {
      runs <- rle(exact)
      run_end <- cumsum(runs$lengths)
      run_start <- run_end - runs$lengths + 1L
      for (run in which(runs$values)) {
        exact_positions <- positions[run_start[run]:run_end[run]]
        roots[[i]] <- c(roots[[i]], height[exact_positions[1L]])
        if (length(exact_positions) > 1L) {
          roots[[i]] <- c(
            roots[[i]], height[utils::tail(exact_positions, 1L)]
          )
        }
      }
    }
    if (length(difference) > 1L) {
      left <- utils::head(difference, -1L)
      right <- utils::tail(difference, -1L)
      changes <- which((left < 0 & right > 0) | (left > 0 & right < 0))
      if (length(changes)) {
        bracket_request[[i]] <- rep(i, length(changes))
        bracket_lower[[i]] <- height[positions[changes]]
        bracket_upper[[i]] <- height[positions[changes + 1L]]
        bracket_difference[[i]] <- difference[changes]
      }
    }
    if (!length(roots[[i]]) && !length(bracket_request[[i]])) {
      status[i] <- if (utils::tail(difference, 1L) > 0) {
        100L
      } else if (difference[1L] < 0) {
        101L
      } else {
        103L
      }
    }
  }
  active_request <- unlist(bracket_request, use.names = FALSE)
  if (length(active_request)) {
    bracket_lower <- unlist(bracket_lower, use.names = FALSE)
    bracket_upper <- unlist(bracket_upper, use.names = FALSE)
    lower_difference <- unlist(bracket_difference, use.names = FALSE)
    active <- rep(TRUE, length(active_request))
    for (iteration in seq_len(200L)) {
      work <- which(active)
      if (!length(work)) break
      midpoint <- (bracket_lower[work] + bracket_upper[work]) / 2
      mapped <- active_request[work]
      args <- arguments
      for (name in setdiff(names(args), c("units", "status"))) {
        args[[name]] <- args[[name]][mapped]
      }
      args$h <- midpoint
      evaluated <- if (identical(basis, "ib")) {
        do.call(dib, args)
      } else {
        do.call(dob, args)
      }
      bad <- !evaluated$status %in% c(0L, 52L)
      if (any(bad)) {
        failed <- work[bad]
        status[active_request[failed]] <- evaluated$status[bad]
        active[failed] <- FALSE
      }
      good <- work[!bad]
      if (length(good)) {
        value <- evaluated$value[!bad] - target[active_request[good]]
        exact <- value == 0
        bracket_lower[good[exact]] <- midpoint[!bad][exact]
        bracket_upper[good[exact]] <- midpoint[!bad][exact]
        nonexact <- good[!exact]
        if (length(nonexact)) {
          nonexact_value <- value[!exact]
          left <- lower_difference[nonexact] * nonexact_value <= 0
          bracket_upper[nonexact[left]] <- midpoint[!bad][!exact][left]
          bracket_lower[nonexact[!left]] <- midpoint[!bad][!exact][!left]
          lower_difference[nonexact[!left]] <- nonexact_value[!left]
        }
        converged <- bracket_upper[good] - bracket_lower[good] <= 1e-4
        active[good[converged]] <- FALSE
      }
    }
    status[unique(active_request[active])] <- 103L
    for (at in which(!active)) {
      request_at <- active_request[at]
      if (status[request_at] %in% c(0L, 52L)) {
        roots[[request_at]] <- c(
          roots[[request_at]],
          (bracket_lower[at] + bracket_upper[at]) / 2
        )
      }
    }
  }
  for (i in seq_len(count)) {
    roots[[i]] <- structure(sort(unique(roots[[i]])), status = status[i])
  }
  roots
}

.mc_all_crossings <- function(session, call, tree, target, basis, lower, upper) {
  n <- length(tree)
  if (!n) return(list())
  result <- vector("list", n)
  compiled <- session$kernel_type[session$model_index[tree]] == 1L
  if (any(compiled)) {
    rows <- which(compiled)
    original <- tree[rows]
    aux <- .mc_subset_aux(.mc_provider_aux(call), original)
    answer <- mc_provider_crossings(
      session$pointer, call$dbh[original], call$ht[original],
      session$model_index[original], aux, target[rows],
      ifelse(basis[rows] == "ob", 1L, 0L), lower[rows], upper[rows],
      if (call$units == "imperial") 1L else 2L
    )
    for (j in seq_along(rows)) {
      begin <- answer$offsets[j] + 1
      end <- answer$offsets[j + 1L]
      roots <- if (end >= begin) answer$roots[begin:end] else numeric()
      result[[rows[j]]] <- structure(roots, status = answer$status[j])
    }
  }
  if (any(!compiled)) {
    rows <- which(!compiled)
    original <- tree[rows]
    aux <- .mc_subset_aux(.mc_provider_aux(call), original)
    arguments <- c(list(
      dbh = call$dbh[original], ht = call$ht[original],
      model = call$model[original], units = call$units, status = TRUE
    ), aux)
    for (current_basis in c("ib", "ob")) {
      selected_rows <- rows[basis[rows] == current_basis]
      if (!length(selected_rows)) next
      selected <- match(selected_rows, rows)
      args <- arguments
      args$dbh <- args$dbh[selected]
      args$ht <- args$ht[selected]
      args$model <- args$model[selected]
      for (name in names(aux)) args[[name]] <- args[[name]][selected]
      answer <- .mc_r_crossing_basis(
        args, target[selected_rows], lower[selected_rows],
        upper[selected_rows], current_basis
      )
      for (j in seq_along(selected_rows)) {
        root_status <- attr(answer[[j]], "status")
        if (root_status %in% c(100L, 101L)) {
          attr(answer[[j]], "status") <- 0L
        }
        result[[selected_rows[j]]] <- answer[[j]]
      }
    }
  }
  result
}

.mc_query_points <- function(session, call, tree, height) {
  n_query <- length(tree)
  output <- data.frame(
    tree = as.integer(tree), height = as.double(height), dib = rep(NA_real_, n_query),
    dob = rep(NA_real_, n_query), cum_ib = rep(NA_real_, n_query),
    cum_ob = rep(NA_real_, n_query), status = rep(0L, n_query)
  )
  if (!n_query) return(output)
  behavior_needs_ob <- isTRUE(call$ob_required)
  query_ob <- behavior_needs_ob |
    session$has_dob[session$model_index[tree]] != 0L
  compiled <- session$kernel_type[session$model_index[tree]] == 1L
  for (need_ob in c(FALSE, TRUE)) {
    rows <- which(compiled & query_ob == need_ob)
    if (!length(rows)) next
    answer <- mc_provider_query(
      session$pointer, call$dbh, call$ht, session$model_index,
      .mc_provider_aux(call), tree[rows], height[rows],
      if (call$units == "imperial") 1L else 2L, need_ob, need_ob
    )
    output$dib[rows] <- answer$dib
    output$dob[rows] <- answer$dob
    output$cum_ib[rows] <- answer$cum_ib
    output$cum_ob[rows] <- answer$cum_ob
    output$status[rows] <- answer$status
  }
  if (any(!compiled)) {
    rows <- which(!compiled)
    original <- tree[rows]
    aux <- .mc_subset_aux(.mc_provider_aux(call), original)
    common <- c(list(
      dbh = call$dbh[original], ht = call$ht[original], h = height[rows],
      model = call$model[original], units = call$units, status = TRUE
    ), aux)
    inside <- do.call(dib, common)
    output$dib[rows] <- inside$value
    output$status[rows] <- inside$status
    outside_rows <- which(query_ob[rows])
    if (length(outside_rows)) {
      outside_original <- original[outside_rows]
      outside_common <- c(list(
        dbh = call$dbh[outside_original], ht = call$ht[outside_original],
        h = height[rows[outside_rows]], model = call$model[outside_original],
        units = call$units, status = TRUE
      ), .mc_subset_aux(.mc_provider_aux(call), outside_original))
      outside <- do.call(dob, outside_common)
      output$dob[rows[outside_rows]] <- outside$value
      output$status[rows[outside_rows]] <- vapply(
        seq_along(outside_rows), function(j) {
          i <- outside_rows[j]
          .mc_first_bad_status(c(inside$status[i], outside$status[j]))
        }, integer(1L)
      )
    }
    below_stump <- height[rows] < call$stump[original]
    output$status[rows[below_stump]] <- 0L
    positive <- height[rows] > 0
    output$cum_ib[rows[!positive]] <- 0
    output$cum_ob[rows[!positive]] <- 0
    if (any(positive)) {
      selected <- which(positive)
      volume_common <- c(list(
        dbh = call$dbh[original[selected]], ht = call$ht[original[selected]],
        model = call$model[original[selected]], lower = 0,
        lower_type = "height", upper = height[rows[selected]],
        upper_type = "height", units = call$units, status = TRUE
      ), .mc_subset_aux(.mc_provider_aux(call), original[selected]))
      ib <- do.call(stem_volume, c(volume_common, list(bark = "inside")))
      output$cum_ib[rows[selected]] <- ib$value
      ob_rows <- which(query_ob[rows[selected]])
      ob_status <- rep(0L, length(selected))
      if (length(ob_rows)) {
        ob_original <- original[selected[ob_rows]]
        ob_common <- c(list(
          dbh = call$dbh[ob_original], ht = call$ht[ob_original],
          model = call$model[ob_original], lower = 0,
          lower_type = "height", upper = height[rows[selected[ob_rows]]],
          upper_type = "height", units = call$units, status = TRUE
        ), .mc_subset_aux(.mc_provider_aux(call), ob_original))
        ob <- do.call(
          stem_volume,
          c(ob_common, list(bark = "outside"))
        )
        output$cum_ob[rows[selected[ob_rows]]] <- ob$value
        ob_status[ob_rows] <- ob$status
      }
      for (j in seq_along(selected)) {
        at <- rows[selected[j]]
        current <- output$status[at]
        statuses <- ib$status[j]
        if (query_ob[at]) statuses <- c(statuses, ob_status[j])
        volume_status <- .mc_first_bad_status(statuses)
        if (current == 0L && volume_status != 0L) output$status[at] <- volume_status
      }
    }
  }
  output
}
