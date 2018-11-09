library(parallel)

#' Create a new Wy column to measure specific/aggregate source/target contagion
#' (dyadic cross-sectional data)
#'
#' @param dat directed dyadic dataset (data.frame)
#' @param source name of source id column (character)
#' @param target name of target id column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param wy name of the output variable (character)
#' @param type (character) aggregate source (different source); aggregate
#' target (different target); specific source (same target different source);
#' specific target (same source different target)
#' @param weights weight specification: ik, jk, im, jm (character)
#' @param row_normalize should the W matrix be row-normalized? (boolean)
#' @param zero_loop should wy be set to 0 when source == target (boolean)
#' @param ncpus number of cpus to use for parallel computation (integer)
#' @param progress show progress bar (boolean)
#'
#' @export
dyadic_w = function(dat, source = 'unit1', target = 'unit2', y = 'y', w = 'w', wy = 'wy',
                    type = 'specific_source', weights = 'ik', row_normalize = TRUE, zero_loop = TRUE,
                    ncpus = 1, progress = TRUE) {
    # sanity checks
    if (!all(c(source, target, y, w) %in% colnames(dat))) {
        stop('source, target, y, and w must be columns in dat')
    }
    tmp = paste(dat[, source], dat[, target])
    if (anyDuplicated(tmp) != 0) {
        stop('Duplicate source-target-time indices in dat.')
    }
    tmp = length(unique(dat[, source])) *
          length(unique(dat[, target]))
    if (tmp != nrow(dat)) {
    	stop('dat is not rectangular')
    }
    if (any(is.na(dat[, source]))) {
    	stop('There are missing values in source.')
    }
    if (any(is.na(dat[, target]))) {
    	stop('There are missing values in target.')
    }
    if (any(is.na(dat[, y]))) {
    	stop('There are missing values in y.')
    }
    if (any(is.na(dat[, w]))) {
    	stop('There are missing values in w.')
    }
    if (!weights %in% c('ik', 'im', 'jk', 'jm')) {
        stop('weight must be "ik", "im", "jk", or "jm"')
    }
    if (!type %in% c("aggregate_source", "aggregate_target", "specific_source", "specific_target")) {
            stop('"type" must be "aggregate_source", "aggregate_target", "specific_source", "specific_target".')
    }
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
    # loops
    out = list()
    j_loop = function(j) {
        Z = Y
        Z$i = i
        Z$j = j
        # specific source: y_ij = sum_k!=i w * y_kj
        # specific target: y_ij = sum_m!=j w * y_im
        # aggregate source: y_ij = sum_k!=i sum_m w * y_km
        # aggregate target: y_ij = sum_k sum_m!=j w * y_km
        if (type == 'aggregate_source') {
            Z = Z[Z$k != i,] # different source
        } else if (type == 'aggregate_target') {
            Z = Z[Z$m != j,] # different target
        } else if (type == 'specific_source') {
            Z = Z[(Z$k != i) & (Z$m == j),] # same target different source
        } else if (type == 'specific_target') {
            Z = Z[(Z$k == i) & (Z$m != j),] # same source different target
        } 
        # wy 
        Z = merge(Z, W)
        if (row_normalize) {
            Z$w = Z$w / sum(Z$w)
        }
        Z = data.frame('i' = i, 'j' = j, 'wy' = sum(Z$w * Z$y))
        return(Z)
    }
    is = unique(dat[, source])
    if (progress) pb = txtProgressBar(min = 0, max = length(is), style = 3)
    for (idx in seq_along(is)) {
        i = is[idx]
        if (progress) setTxtProgressBar(pb, idx)
        tmp = mclapply(unique(dat[, target]), j_loop, mc.cores = ncpus)
        out = c(out, tmp)
    }
    out = do.call('rbind', out)
    colnames(out) = c(source, target, wy)
    if (zero_loop) {
        out$wy[out[, source] == out[, target]] = 0
    }
    return(out)
}
