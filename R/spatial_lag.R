#TODO: does not rename with the wy argument. calls everything "wy"
library(Matrix)
library(parallel)
library(assertthat)

#' Internal sanity check function
sanity = function(dat, source = 'source', target = 'target', w = 'w', y = NULL,
                  time = NULL) {
    # argument type
    assert_that(is.string(source),
                is.string(target),
                is.string(w),
                is.data.frame(dat))
    if (!is.null(y)) {
        assert_that(is.string(y))
    }
    if (!is.null(time)) {
        assert_that(is.string(time))
    }
    # columns
    assert_that(dat %has_name% source, 
                dat %has_name% target, 
                dat %has_name% w)
    if (!is.null(y)) {
        assert_that(dat %has_name% y)
    }
    if (!is.null(time)) {
        assert_that(dat %has_name% time)
    }
    # data types
    assert_that(is.numeric(dat[[source]]) | is.integer(dat[[source]]) | is.character(dat[[source]]),
                is.numeric(dat[[target]]) | is.integer(dat[[target]]) | is.character(dat[[target]]),
                is.numeric(dat[[w]]) | is.integer(dat[[w]]) | is.character(dat[[w]]))
    if (!is.null(y)) {
        assert_that(is.numeric(dat[[y]]) | is.integer(dat[[y]]) | is.date(dat[[y]]))
    }
    if (!is.null(time)) {
        assert_that(is.numeric(dat[[time]]) | is.integer(dat[[time]]) | is.date(dat[[time]]))
    }
    # missing values
    assert_that(noNA(dat[[source]]),
                noNA(dat[[target]]),
                noNA(dat[[w]]))
    if (!is.null(y)) {
        assert_that(noNA(dat[[y]]))
    }
    if (!is.null(time)) {
        assert_that(noNA(dat[[time]]))
    }
    # duplicate indices
    if (is.null(time)) {
        variables = c(source, target)
    } else {
        variables = c(source, target, time)
    }
    idx = dat[, variables]
    idx = apply(idx, 1, paste, collapse = '|')
    if (length(idx) != length(unique(idx))) {
        msg = paste(variables, collapse = '-')
        msg = paste('There are duplicate', msg, 'observations in dat.')
        stop(msg)
    }
    # rectangular data: assumes directed dyads. assumes self-weights exist (not NA but could be zero)
    units = c(dat[[source]], dat[[ target]])
    units = unique(units)
    if (is.null(time)) {
        n = length(unique(units))**2
        msg = 'dat must be a "rectangular" data.frame, which includes rows for all possible combinations of source and target values. dat must be in directed dyad format. If the weights data are in undirected format, they should be converted using the `undirected_to_directed` function. Self-weights (diagonal of the W matrix) must be supplied explicitly: they cannot be NA, but could be equal to zero. Again, the `undirected_to_directed` function can help with this process. The `expand.grid` function can also be useful when preparing a dataset with all directed dyad observations.'
    } else {
        msg = 'dat must be a "rectangular" data.frame, which includes rows for all possible combinations of source, target, and time values. dat must be in directed dyad format. If the weights data are in undirected format, they should be converted using the `undirected_to_directed` function. Self-weights (diagonal of the W matrix) must be supplied explicitly: they cannot be NA, but could be equal to zero. Again, the `undirected_to_directed` function can help with this process. The `expand.grid` function can also be useful when preparing a dataset with all directed dyad-year observations.'
        times = unique(dat[, time])
        n = length(units)**2 * length(times)
    }
    if (n != nrow(dat)) {
        stop(msg)
    }
}

#' duplicating rows while inverting source and target identifiers.
#' 
#' @param dat directed dyadic dataset (data.frame)
#' @param source name of source id column (character)
#' @param target name of target id column (character)
#' 
#' @export
undirected_to_directed = function(dat, source = 'unit1', target = 'unit2', time = NULL) {
    # sanity
    if (is.null(time)) {
        idx = ifelse(dat[[source]] <= dat[[target]], 
                     paste(dat[[source]], dat[[target]], sep = ' | '),
                     paste(dat[[target]], dat[[source]], sep = ' | '))
        if (anyDuplicated(idx)) {
            stop('There are duplicate source-target/target-source obervations.')
        }
    } else {
        idx = ifelse(dat[[source]] <= dat[[target]], 
                     paste(dat[[source]], dat[[target]], dat[[time]], sep = ' | '),
                     paste(dat[[target]], dat[[source]], dat[[time]], sep = ' | '))
        if (anyDuplicated(idx)) {
            stop('There are duplicate source-target-time/target-source-time obervations.')
        }
    }
    # duplicate + switch index + rbind
    a = b = dat
    a[[source]] = ifelse(dat[[source]] <= dat[[target]], dat[[source]], dat[[target]])
    a[[target]] = ifelse(dat[[source]] <= dat[[target]], dat[[target]], dat[[source]])
    b[[source]] = ifelse(dat[[source]] >= dat[[target]], dat[[source]], dat[[target]])
    b[[target]] = ifelse(dat[[source]] >= dat[[target]], dat[[target]], dat[[source]])
    out = rbind(a, b)
    return(out)
}

#' Internal function to apply to a cross-section
#' 
monadic_w_cs = function(dat, source = 'unit1', target = 'unit2', w = 'w') {
    tmp = dat[, c(source, target, w)]
    W = reshape(tmp, idvar = source, timevar = target, direction = 'wide')
    idx = W[, 1]
    W = W[, 2:ncol(W)]
    colnames(W) = row.names(W) = idx
    W = as.matrix(W)
    return(W)
}

#' Create a W matrix. With panel data, this will produce a block diagonal matrix.
#'
#' @param dat directed dyadic dataset (data.frame)
#' @param source name of source id column (character)
#' @param target name of target id column (character)
#' @param w name of distance/weights column (character)
#' @param time name of the (optional) time variable column (character)
#' @param row_normalize should each value of the W matrix be divided by the
#' row-wise sum? (boolean)
#'
#' @export
monadic_w = function(dat, source = 'unit1', target = 'unit2', w = 'w', 
                     time = NULL, row_normalize = TRUE) {
    # sanity checks
    sanity(dat, source = source, target = target, w = w, time = time)
    # cross-section
    if (is.null(time)) {
        dat = dat[order(dat[, source], dat[, target]),]
        out = monadic_w_cs(dat, source = source, target = target, w = w)
    # panel
    } else {
        dat = dat[order(dat[, source], dat[, target], dat[, time]),]
        out = split(dat, dat[, time])
        out = lapply(out, function(x) 
                     monadic_w_cs(x, source = source, target = target, w = w))
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

#' Adds a new Wy column which measures specific/aggregate source/target contagion
#' (directed dyadic cross-sectional or panel data)
#'
#' @param dat directed dyadic dataset (data.frame)
#' @param source name of source id column (character)
#' @param target name of target id column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param wy name of the output variable (character)
#' @param time name of the time variable (optional) (character)
#' @param type of W matrix (character) 
#' \itemize{
#' \item specific source: y_ij = f(sum_k!=i w * y_kj)
#' \item specific target: y_ij = f(sum_m!=j w * y_im)
#' \item aggregate source: y_ij = f(sum_k!=i sum_m w * y_km)
#' \item aggregate target: y_ij = f(sum_k sum_m!=j w * y_km)
#' }
#' @param weights weight specification: ik, jk, im, jm (character)
#' @param row_normalize should each value of the W matrix be divided by the
#' row-wise sum? (boolean)
#' @param zero_loop should wy be set to 0 when source == target (boolean)
#' @param ncpus number of cpus to use for parallel computation (integer)
#' @param progress show progress bar (boolean) (only works for cross-sectional
#' data.)
#' @param return_data function returns the full dataset (TRUE) or just the
#' spatial lag (FALSE)
#'
#' @export
dyadic_wy = function(dat, source = 'unit1', target = 'unit2', y = 'y', w = 'w', wy = 'wy', time = NULL,
                     type = 'specific_source', weights = 'ik', row_normalize = TRUE, zero_loop = TRUE,
                     ncpus = 1, progress = TRUE, return_data = FALSE) {
    # sanity checks
    sanity(dat, source = source, target = target, w = w, y = y, time = time)
    if (!weights %in% c('ik', 'im', 'jk', 'jm')) {
        stop('weight must be "ik", "im", "jk", or "jm"')
    }
    if (!type %in% c("aggregate_source", "aggregate_target", "specific_source", "specific_target")) {
        stop('"type" must be "aggregate_source", "aggregate_target", "specific_source", "specific_target".')
    }
    # cross-section
    if (is.null(time)) {
        dat = dat[order(dat[[source]], dat[[target]]),]
        out = dyadic_wy_cs(dat, source = source, target = target, w = w, y = y,
                           type = type, weights = weights, row_normalize = row_normalize,
                           zero_loop = zero_loop, ncpus = ncpus, progress = progress)
    # panel
    } else {
        if (progress) {
            warning('progress argument does not work for panel data.')
        }
        dat = dat[order(dat[[source]], dat[[target]], dat[[time]]),]
        out = split(dat, dat[[time]])
        f = function(x) dyadic_wy_cs(x,
                                     source = source, target = target, w = w, y = y,
                                     type = type, weights = weights, row_normalize = row_normalize,
                                     zero_loop = zero_loop, ncpus = 1, progress = FALSE, 
                                     return_data = return_data)
        out = mclapply(out, f, mc.cores = ncpus, mc.preschedule = FALSE)
        out = do.call('rbind', out)
    }
    return(out)
}

dyadic_wy_cs = function(dat, source = 'unit1', target = 'unit2', y = 'y', w = 'w', wy = 'wy',
                        type = 'specific_source', weights = 'ik', row_normalize = TRUE, zero_loop = TRUE,
                        ncpus = 1, progress = TRUE, return_data = FALSE) {
    # weights
    W = dat[, c(source, target, w)]
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
    Y = dat[, c(source, target, y)]
    colnames(Y) = c('k', 'm', 'y')
    # main loop
    out = expand.grid('i' = unique(dat[[source]]),
                      'j' = unique(dat[[target]]),
                      'wy' = NA,
                      stringsAsFactors = FALSE)
    if (progress) pb = txtProgressBar(min = 0, max = nrow(out), style = 3)
    for (idx in 1:nrow(out)) {
        i = out$i[idx]
        j = out$j[idx]
        Z = Y
        Z$i = i
        Z$j = j
        # aggregate source: y_ij = sum_k!=i sum_m w * y_km
        if (type == 'aggregate_source') {
            Z = Z[Z$k != i,] 
        # aggregate target: y_ij = sum_k sum_m!=j w * y_km
        } else if (type == 'aggregate_target') {
            Z = Z[Z$m != j,] 
        # specific source: y_ij = sum_k!=i w * y_kj
        } else if (type == 'specific_source') {
            Z = Z[(Z$k != i) & (Z$m == j),]
        # specific target: y_ij = sum_m!=j w * y_im
        } else if (type == 'specific_target') {
            Z = Z[(Z$k == i) & (Z$m != j),] 
        } 
        # wy 
        Z = merge(Z, W)
        if (row_normalize) {
            Z$w = Z$w / sum(Z$w)
        }
        out$wy[idx] = sum(Z$w * Z$y)
        if (progress) setTxtProgressBar(pb, idx)
    }
    colnames(out) = c(source, target, wy)
    # merge back into dataset
    if (return_data) {
        out = merge(dat, out)
    }
    return(out)
}
