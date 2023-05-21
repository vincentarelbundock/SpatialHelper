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

    if (!is.null(type)) {
        checkmate::assert_true(type %in% c("aggregate_origin", "aggregate_destination", 
                                           "specific_origin", "specific_destination"))
        if (!is.null(weights)) {
            # Neumayer rules from Stata documentation
            # TODO: Write more tests if you want access to the others
            if (type == 'specific_origin') {
                checkmate::assert_true(weights %in% c("ik", "ki")) 
                # c("ik", "ki", "im", "mi", "jm", "mj", "jk", "kj"))
            } else if (type == 'specific_destination') {
                checkmate::assert_true(weights %in% c("jm", "mj")) 
                # c("ik", "ki", "im", "mi", "jm", "mj", "jk", "kj")
            } else if (type == 'aggregate_origin') {
                checkmate::assert_true(weights %in% c("ik", "ki")) 
                # c("ik", "ki", "im", "mi"))
            } else if (type == 'aggregate_destination') {
                checkmate::assert_true(weights %in% c("jm", "mj")) 
                # c("jm", "mj", "jk", "kj"))
            }
        }
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

    # weights are non-negative
    checkmate::assert_numeric(dat[[w]], lower = 0, upper = Inf, any.missing = FALSE)

    # duplicate indices
    if (!is.null(origin) &&
        !is.null(destination) &&
        is.null(time)) {
        variables = c(origin, destination)
        origin_destination_index = dat[, variables]
        origin_destination_index = do.call(paste, c(origin_destination_index, sep = '|'))
        checkmate::assert_true(anyDuplicated(origin_destination_index) == 0)
    } else if (!is.null(origin) &&
               !is.null(origin) &&
               !is.null(time)) {
        variables = c(origin, destination, time)
        origin_destination_time_index = dat[, variables]
        origin_destination_time_index = do.call(paste, c(origin_destination_time_index, sep = '|'))
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
        out = rbindlist(out)
    }

    return(out)
}

#' Internal function
#' @inheritParams dyadic_wy
#' @import data.table
dyadic_wy_cs = function(datdf,
                        origin = "unit1",
                        destination = "unit2",
                        y = "y",
                        w = "w",
                        wy = "wy",
                        type = "specific_origin",
                        weights = "ik",
                        row_normalize = TRUE,
                        zero_loop = TRUE) {
    # weights
    dat = as.data.table(datdf)
    W = dat[, .SD, .SDcols = c(origin, destination, w)]
    if (zero_loop) {
        W = W[W[[origin]] != W[[destination]]]
    }
    colnames(W) <- c(strsplit(weights, "")[[1]], "w")
    # edges
    Y = dat[, .SD, .SDcols = c(origin, destination, y)]
    colnames(Y) = c("k", "m", "y")
    # main loop
    out = data.table::CJ(
        "i" = unique(dat[[origin]]),
        "j" = unique(dat[[destination]]),
        "wy" = NA)
    byidx <- intersect(colnames(Y), colnames(W))
    for (idx in seq_len(nrow(out))) {
        ival = out$i[idx]
        jval = out$j[idx]
        Y[, i := ival]
        Y[, j := jval]
        # aggregate origin: y_ij = sum_k!=i sum_m w * y_km
        if (type == "aggregate_origin") {
            Z = merge(Y[k != ival], W)
        # aggregate destination: y_ij = sum_k sum_m!=j w * y_km
        } else if (type == "aggregate_destination") {
            Z = merge(Y[m != jval], W)
        # specific origin: y_ij = sum_k!=i w * y_kj
        } else if (type == "specific_origin") {
            Z = merge(Y[(k != ival) & (m == jval)], W)
        # specific destination: y_ij = sum_m!=j w * y_im
        } else if (type == "specific_destination") {
            Z = merge(Y[(k == ival) & (m != jval)], W)
        }
        if (row_normalize) {
            Z[, w := w / sum(w)]
        }
        out$wy[idx] = sum(Z$w * Z$y)
    }
    colnames(out) = c(origin, destination, wy)
    # merge back into dataset
    data.table::setDT(dat)
    out = merge(dat, out)
    data.table::setDF(out)
    return(out)
}
