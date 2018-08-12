# TODO: are network loops non-zero? does that matter?
sanity_check_dyad_time = function(dyad_time, 
                                  unit_src,
                                  unit_tar,
                                  time, 
                                  w, 
                                  y) {
    if (!w %in% colnames(dyad_time)) {
        stop('w must be a column in dyad_time')
    }
    if (!y %in% colnames(dyad_time)) {
        stop('y must be a column in dyad_time')
    }
    if (class(dyad_time)[1] != 'data.frame') {
    	stop('dyad_time must be a data.frame')
    }
    tmp = paste(dyad_time[, unit_src], dyad_time[, unit_tar], dyad_time[, time])
    if (anyDuplicated(tmp) != 0) {
        stop('Duplicate unit_src-unit_tar-time indices in dyad_time.')
    }
    tmp = length(unique(dyad_time[, unit_src])) *
          length(unique(dyad_time[, unit_tar])) *
          length(unique(dyad_time[, time])) 
    if (tmp != nrow(dyad_time)) {
    	stop('dyad_time is not rectangular')
    }
    if (any(is.na(dyad_time))) {
    	stop('dyad_time contains missing values')
    }
}

#' Add a new Wy column to measure specific source/target contagion
#' (cross-sectional data)
#'
#' @param dat dyad-year dataset (data.frame)
#' @param unit_src name of source id column (character)
#' @param unit_target name of target id column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param wy name of the output variable (character)
#' @param row_normalize should the W matrix be row-normalized? (boolean)
#'
#' @export
specific_source = function(dat,
                   unit_src = 'unit1',
                   unit_tar = 'unit2',
                   w = 'w',
                   y = 'y',
                   wy = 'wy',
                   row_normalize = TRUE) {
    dat = dat[order(dat[, unit_src], dat[, unit_tar]),]
	w_ik = Matrix::Matrix(dat[, w], nrow=sqrt(nrow(dat)), byrow=TRUE)
    y_ik = Matrix::Matrix(dat[, y], nrow=sqrt(nrow(dat)), byrow=TRUE)
	diag(w_ik) = 0
	if (row_normalize) {
		w_ik = w_ik / Matrix::rowSums(w_ik)
	}
    result = w_ik %*% y_ik
    result = as.vector(Matrix::t(result))
    dat[, wy] = result 
    return(dat)
}

#' A wrapper around `specific_source`. Split-Apply-Combine by time period.
#'
#' @param dat dyad-year dataset (data.frame)
#' @param unit_src name of source id column (character)
#' @param unit_target name of target id column (character)
#' @param time name of time column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param wy name of the output variable (character)
#' @param row_normalize should the W matrix be row-normalized? (boolean)
#'
#' @export
specific_source_panel = function(dat,
                                 unit_src = 'unit1',
                                 unit_tar = 'unit2',
                                 time = 'time',
                                 w = 'w',
                                 y = 'y',
                                 wy = 'wy',
                                 row_normalize = TRUE) {
    out = dat[order(dat[, unit_src], dat[, unit_tar], dat[, time]), ]
    out = split(dat, dat[, time])
    out = lapply(out, function(x) specific_source(dat = x,
                                                  unit_src = unit_src,
                                                  unit_tar = unit_tar,
                                                  w = w,
                                                  y = y,
                                                  wy = wy,
                                                  row_normalize = row_normalize))
    out = do.call('rbind', out)
    return(out)
}
