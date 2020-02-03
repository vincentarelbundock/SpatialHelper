#' Internal sanity check function
#'
#' @inheritParams dyadic_wy
sanity = function(dat, 
                  origin = NULL,
                  destination = NULL,
                  w = NULL,
                  y = NULL,
                  wy = NULL,
                  time = NULL,
                  weights = NULL,
                  type = NULL) {

    # argument type
    checkmate::assert_string(origin, null.ok = TRUE)
    checkmate::assert_string(destination, null.ok = TRUE)
    checkmate::assert_string(w, null.ok = TRUE)
    checkmate::assert_string(y, null.ok = TRUE)
    checkmate::assert_string(wy, null.ok = TRUE)
    checkmate::assert_string(time, null.ok = TRUE)
    checkmate::assert_string(weights, null.ok = TRUE)
    checkmate::assert_string(type, null.ok = TRUE)
    checkmate::assert_data_frame(dat, null.ok = TRUE)

    if (!is.null(weights)) {
        checkmate::assert_true(weights %in% c('ik', 'im', 'jk', 'jm'))
    }

    if (!is.null(type)) {
        checkmate::assert_true(type %in% c("aggregate_origin", "aggregate_destination", 
                                           "specific_origin", "specific_destination"))
    }

    # variables present in data.frame
    if (!is.null(origin)) checkmate::assert_true(origin %in% colnames(dat))
    if (!is.null(destination)) checkmate::assert_true(destination %in% colnames(dat))
    if (!is.null(time)) checkmate::assert_true(time %in% colnames(dat))
    if (!is.null(w)) checkmate::assert_true(w %in% colnames(dat))
    if (!is.null(y)) checkmate::assert_true(y %in% colnames(dat))

    # variable types
    checkmate::assert(
        checkmate::check_numeric(dat[[origin]], any.missing = FALSE),
        checkmate::check_character(dat[[origin]], any.missing = FALSE),
        null.ok = TRUE
    )

    checkmate::assert(
        checkmate::check_numeric(dat[[destination]], any.missing = FALSE),
        checkmate::check_character(dat[[destination]], any.missing = FALSE),
        null.ok = TRUE
    )

    if (!is.null(time)) {
        checkmate::assert(
            checkmate::check_date(dat[[time]], any.missing = FALSE),
            checkmate::check_numeric(dat[[time]], any.missing = FALSE),
            checkmate::check_character(dat[[time]], any.missing = FALSE),
            null.ok = TRUE
        )
    }

    # duplicate indices
    if (!is.null(origin) &
        !is.null(destination) &
        is.null(time)) {
        variables = c(origin, destination)
        origin_destination_index = dat[, variables]
        origin_destination_index = apply(origin_destination_index, 1, paste, collapse = '|')
        checkmate::assert_true(anyDuplicated(origin_destination_index) == 0)
    } else if (!is.null(origin) &
               !is.null(origin) &
               !is.null(time)) {
        variables = c(origin, destination, time)
        origin_destination_time_index = dat[, variables]
        origin_destination_time_index = apply(origin_destination_time_index, 1, paste, collapse = '|')
        checkmate::assert_true(anyDuplicated(origin_destination_time_index) == 0)
    }
    
    # rectangular data: assumes directed dyads. assumes self-weights exist (not NA but could be zero)
    units = c(dat[[origin]], dat[[ destination]])
    units = unique(units)
    if (is.null(time)) {
        n = length(unique(units))**2
        msg = 'dat must be a "rectangular" data.frame, which includes rows for all possible combinations of origin and destination values. dat must be in directed dyad format. If the weights data are in undirected format, they should be converted using the `undirected_to_directed` function. Self-weights (diagonal of the W matrix) must be supplied explicitly: they cannot be NA, but could be equal to zero. Again, the `undirected_to_directed` function can help with this process. The `expand.grid` function can also be useful when preparing a dataset with all directed dyad observations.'
    } else {
        msg = 'dat must be a "rectangular" data.frame, which includes rows for all possible combinations of origin, destination, and time values. dat must be in directed dyad format. If the weights data are in undirected format, they should be converted using the `undirected_to_directed` function. Self-weights (diagonal of the W matrix) must be supplied explicitly: they cannot be NA, but could be equal to zero. Again, the `undirected_to_directed` function can help with this process. The `expand.grid` function can also be useful when preparing a dataset with all directed dyad-year observations.'
        times = unique(dat[[time]])
        n = length(units)**2 * length(times)
    }
    if (n != nrow(dat)) {
        stop(msg)
    }
}

#' duplicating rows while inverting origin and destination identifiers.
#' 
#' @param dat directed dyadic dataset (data.frame)
#' @param origin name of origin id column (character)
#' @param destination name of destination id column (character)
#' @inheritParams dyadic_wy
#' 
#' @export
undirected_to_directed = function(dat, origin = 'unit1', destination = 'unit2', time = NULL) {

    # sanity
    if (is.null(time)) {
        idx = ifelse(dat[[origin]] <= dat[[destination]], 
                     paste(dat[[origin]], dat[[destination]], sep = ' | '),
                     paste(dat[[destination]], dat[[origin]], sep = ' | '))
        if (anyDuplicated(idx)) {
            stop('There are duplicate origin-destination/destination-origin obervations.')
        }
    } else {
        idx = ifelse(dat[[origin]] <= dat[[destination]], 
                     paste(dat[[origin]], dat[[destination]], dat[[time]], sep = ' | '),
                     paste(dat[[destination]], dat[[origin]], dat[[time]], sep = ' | '))
        if (anyDuplicated(idx)) {
            stop('There are duplicate origin-destination-time/destination-origin-time obervations.')
        }
    }
    # duplicate + switch index + rbind
    a = b = dat
    a[[origin]] = ifelse(dat[[origin]] <= dat[[destination]], dat[[origin]], dat[[destination]])
    a[[destination]] = ifelse(dat[[origin]] <= dat[[destination]], dat[[destination]], dat[[origin]])
    b[[origin]] = ifelse(dat[[origin]] >= dat[[destination]], dat[[origin]], dat[[destination]])
    b[[destination]] = ifelse(dat[[origin]] >= dat[[destination]], dat[[destination]], dat[[origin]])
    out = rbind(a, b)
    return(out)
}

#' Internal function to apply to a cross-section
#' 
#' @inheritParams dyadic_wy
monadic_w_cs = function(dat, origin = 'unit1', destination = 'unit2', w = 'w') {
    tmp = dat[, c(origin, destination, w)]
    W = stats::reshape(tmp, idvar = origin, timevar = destination, direction = 'wide')
    idx = W[[1]]
    W = W[, 2:ncol(W)]
    colnames(W) = row.names(W) = idx
    W = as.matrix(W)
    return(W)
}

#' Create a W matrix. With panel data, this will produce a block diagonal matrix.
#'
#' @param dat directed dyadic dataset (data.frame)
#' @param origin name of origin id column (character)
#' @param destination name of destination id column (character)
#' @param w name of distance/weights column (character)
#' @param time name of the (optional) time variable column (character)
#' @param row_normalize should each value of the W matrix be divided by the
#' row-wise sum? (boolean)
#' @inheritParams dyadic_wy
#'
#' @export
monadic_w = function(dat, origin = 'unit1', destination = 'unit2', w = 'w', 
                     time = NULL, row_normalize = TRUE) {
    # sanity checks
    sanity(dat, origin = origin, destination = destination, w = w, time = time)
    # cross-section
    if (is.null(time)) {
        dat = dat[order(dat[[origin]], dat[[destination]]),]
        out = monadic_w_cs(dat, origin = origin, destination = destination, w = w)
    # panel
    } else {
        dat = dat[order(dat[[origin]], dat[[destination]], dat[[time]]),]
        out = split(dat, dat[[time]])
        out = lapply(out, function(x) 
                     monadic_w_cs(x, origin = origin, destination = destination, w = w))
        idx = lapply(names(out), function(x) 
                     paste(colnames(out[[x]]), x, sep = '|'))
        idx = unlist(idx)
        out = Matrix::bdiag(out)
        colnames(out) = row.names(out) = idx
    }
    # row normalize
    if (row_normalize) {
        out = out / rowSums(out)
    }
    # output
    out = as.matrix(out)
    return(out)
}

#' Adds a new Wy column which measures specific/aggregate origin/destination contagion
#' (directed dyadic cross-sectional or panel data)
#'
#' @param dat directed dyadic dataset (data.frame)
#' @param origin name of origin id column (character)
#' @param destination name of destination id column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param wy name of the output variable (character)
#' @param time name of the time variable (optional) (character)
#' @param type of W matrix (character) 
#' \itemize{
#'   \item specific origin: y_ij = f(sum_k!=i w * y_kj)
#'   \item specific destination: y_ij = f(sum_m!=j w * y_im)
#'   \item aggregate origin: y_ij = f(sum_k!=i sum_m w * y_km)
#'   \item aggregate destination: y_ij = f(sum_k sum_m!=j w * y_km)
#' }
#' @param weights weight specification: ik, jk, im, jm (character)
#' @param row_normalize should each value of the W matrix be divided by the
#' row-wise sum? (boolean)
#' @param zero_loop should wy be set to 0 when origin == destination (boolean)
#' @param progress boolean show progress bar (boolean). Only works with
#' `plan(multiprocess)` and panel data (`time != NULL`).
#'
#' @export
dyadic_wy = function(dat, 
                     origin = 'unit1', 
                     destination = 'unit2', 
                     y = 'y', 
                     w = 'w', 
                     wy = 'wy', 
                     time = NULL,
                     type = 'specific_origin', 
                     weights = 'ik', 
                     row_normalize = TRUE, 
                     zero_loop = TRUE,
                     progress = FALSE) {

    # sanity checks
    sanity(dat, origin = origin, destination = destination, w = w, y = y, time = time, weights = weights, type = type)


    # inner loop function
    f = function(x) 
        dyadic_wy_cs(x,
                     origin = origin, destination = destination, w = w, y = y, wy = wy,
                     type = type, weights = weights, row_normalize = row_normalize, 
                     zero_loop = zero_loop)

    # cross-section
    if (is.null(time)) {
        dat = dat[order(dat[[origin]], dat[[destination]]),]
        out = f(dat)
        if (progress) {
            warning('progress argument only works with `plan(multiprocess)` and panel data (`time != NULL`).')
        }

    # panel
    } else {
        dat = dat[order(dat[[origin]], dat[[destination]], dat[[time]]),]
        out = split(dat, dat[[time]])
        out = furrr::future_map(out, f, .progress = progress)
        out = do.call('rbind', out)
    }

    return(out)
}

#' Internal function
#' @inheritParams dyadic_wy
dyadic_wy_cs = function(dat, 
                        origin = 'unit1', 
                        destination = 'unit2', 
                        y = 'y', 
                        w = 'w', 
                        wy = 'wy', 
                        type = 'specific_origin', 
                        weights = 'ik', 
                        row_normalize = TRUE, 
                        zero_loop = TRUE) {

    # weights
    W = dat[, c(origin, destination, w)]
    if (zero_loop) {
        W = W[W[[origin]] != W[[destination]],]
    }
    if (weights == 'ik') {
        colnames(W) = c('i', 'k', 'w')
    } else if (weights == 'im') {
        colnames(W) = c('i', 'm', 'w')
    } else if (weights == 'jk') {
        colnames(W) = c('i', 'k', 'w')
    } else if (weights == 'jm') {
        colnames(W) = c('j', 'm', 'w')
    }
    # edges
    Y = dat[, c(origin, destination, y)]
    colnames(Y) = c('k', 'm', 'y')
    # main loop
    out = expand.grid('i' = unique(dat[[origin]]),
                      'j' = unique(dat[[destination]]),
                      'wy' = NA,
                      stringsAsFactors = FALSE)
    for (idx in 1:nrow(out)) {
        i = out$i[idx]
        j = out$j[idx]
        Z = Y
        Z$i = i
        Z$j = j
        # aggregate origin: y_ij = sum_k!=i sum_m w * y_km
        if (type == 'aggregate_origin') {
            Z = Z[Z$k != i,] 
        # aggregate destination: y_ij = sum_k sum_m!=j w * y_km
        } else if (type == 'aggregate_destination') {
            Z = Z[Z$m != j,] 
        # specific origin: y_ij = sum_k!=i w * y_kj
        } else if (type == 'specific_origin') {
            Z = Z[(Z$k != i) & (Z$m == j),]
        # specific destination: y_ij = sum_m!=j w * y_im
        } else if (type == 'specific_destination') {
            Z = Z[(Z$k == i) & (Z$m != j),] 
        } 
        # wy 
        Z = merge(Z, W)
        if (row_normalize) {
            Z$w = Z$w / sum(Z$w)
        }
        out$wy[idx] = sum(Z$w * Z$y)
    }
    colnames(out) = c(origin, destination, wy)
    # merge back into dataset
    out = merge(dat, out)
    return(out)
}
