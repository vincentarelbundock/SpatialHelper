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

#' Produce a new Wy column to measure specific source/target contagion
#' (cross-sectional data)
#'
#' @param dat dyad-year dataset (data.frame)
#' @param unit_src name of source id column (character)
#' @param unit_target name of target id column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param type "source" or "target"-specific contagion (character)
#' @param zero_diag should the W matrix diagonal be zeroed? (boolean)
#' @param row_normalize should the W matrix be row-normalized? (boolean)
#'
#' @export
sl_dyad_specific = function(dat,
                   unit_src = 'unit1',
                   unit_tar = 'unit2',
                   w = 'w',
                   y = 'y',
                   type = 'source',
                   zero_diag = TRUE,
                   row_normalize = TRUE) {
    tmp = dat[, c(unit_src, unit_tar, w, y)] 
    if (type == 'source') {
        tmp = tmp[order(tmp[, unit_tar], tmp[, unit_src]),]
    } else if (type == 'target') {
        tmp = tmp[order(tmp[, unit_src], tmp[, unit_tar]),]
    } else {
        stop('type must be "source" or "target"')
    }
    w = tmp %>%
        dplyr::select_(unit_src, unit_tar, w) %>%
        tidyr::spread(2, 3) %>%
        tibble::column_to_rownames(unit_src) %>%
        as.matrix 
    w = rep(list(w), nrow(tmp) / nrow(w)) %>%
        Matrix::bdiag()
    if (zero_diag) {
        Matrix::diag(w) = 0
    }
    if (row_normalize) {
        w = w / Matrix::rowSums(w)
    }
    tmp$wy = w %*% tmp[, y] %>% as.vector
    out = tmp[, c(unit_src, unit_tar, 'wy')]
    return(out)
}

#' Produce a new Wy column to measure specific source/target contagion (panel
#' data)
#'
#' @param dat dyad-year dataset (data.frame)
#' @param unit_src name of source id column (character)
#' @param unit_target name of target id column (character)
#' @param time name of time id column (character)
#' @param w name of distance/weights column (character)
#' @param y name of outcome to lag (character)
#' @param type "source" or "target"-specific contagion (character)
#' @param zero_diag should the W matrix diagonal be zeroed? (boolean)
#' @param row_normalize should the W matrix be row-normalized? (boolean)
#'
#' @export
sl_dyad_specific_panel = function(dat, unit_src, unit_tar, time, w, y, type,
                                  zero_diag, row_normalize) {
    sanity_check_dyad_time(dat, unit_src, unit_tar, time, w, y)
    tmp = dat[order(dat[, time]),]
    idx = tmp[, time]
    tmp = split(tmp, tmp[, time])
    out = lapply(tmp, sl_dyad_specific, 
                 unit_src = unit_src,
                 unit_tar = unit_tar,
                 w = w,
                 y = y,
                 type = type,
                 zero_diag = zero_diag,
                 row_normalize = row_normalize
                 )
    out = do.call('rbind', out)
    out[, time] = idx
    out = out[, c(unit_src, unit_tar, time, 'wy')]
    return(out)
}

#w_monadic = function(unit_time, dyad_time, y = 'x', w = 'w', row_normalize = TRUE) {
    ## sanity checks
    #sanity_check(unit_time, dyad_time, w, y)
    ## hardcoded variable names
    #unit_time = unit_time[, c('unit', 'time', y)] %>%
                 #setNames(c('unit', 'time', 'y'))
    #dyad_time = dyad_time[, c('unit1', 'unit2', 'time', w)] %>%
                #setNames(c('unit1', 'unit2', 'time', 'w'))
	## sorting
    #unit_time = unit_time %>% 
                 #dplyr::arrange(time, unit)
	#dyad_time = dyad_time %>% 
				#dplyr::arrange(time, unit1, unit2)
	## W matrix
    #W = dyad_time %>% 
        #split(.$time) %>%
        #purrr::map(~ dplyr::select(., -time)) %>%
        #purrr::map(~ tidyr::spread(., unit1, w)) %>%
        #purrr::map(~ tibble::column_to_rownames(., 'unit2')) %>%
        #purrr::map(as.matrix) %>%
        #Matrix::bdiag(.)
    #diag(W) = 0
	## row normalization
    #if (row_normalize) {
        #W = W / rowSums(W)
    #}
	## spatial lag
    #out = W %*% unit_time$y %>%
          #as.vector
    #return(out)
#}

#sanity_check = function(unit_time, dyad_time, w, y) {
    ## is nrow(dyad_time) a multiple of w
    #if (!w %in% colnames(dyad_time)) {
        #stop('w must be a column in dyad_time')
    #}
    #if (!y %in% colnames(unit_time)) {
        #stop('y must be a column in unit_time')
    #}
    #if (class(unit_time)[1] != 'data.frame') {
        #stop('unit_time must be a data.frame')
    #}
    #if (class(dyad_time)[1] != 'data.frame') {
        #stop('dyad_time must be a data.frame')
    #}
    #tmp = paste(dyad_time$unit1, dyad_time$unit2, dyad_time$time)
    #if (anyDuplicated(tmp) != 0) {
        #stop('Duplicate unit1-unit2-time indices in dyad_time.')
    #}
    #tmp = paste(unit_time$unit, unit_time$time)
    #if (anyDuplicated(tmp) != 0) {
        #stop('Duplicate unit-time indices in unit_time.')
    #}
    #tmp = length(unique(unit_time$unit)) *
          #length(unique(unit_time$time)) 
    #if (tmp != nrow(unit_time)) {
        #stop('unit_time is not rectangular')
    #}
    #tmp = length(unique(dyad_time$unit1)) *
          #length(unique(dyad_time$unit2)) *
          #length(unique(dyad_time$time)) 
    #if (tmp != nrow(dyad_time)) {
        #stop('dyad_time is not rectangular')
    #}
    #if (any(is.na(unit_time))) {
        #stpo('unit_time must not contain missing values')
    #}
    #if (any(is.na(dyad_time))) {
        #stop('dyad_time must not contain missing values')
    #}
    #if (length(setdiff(unit_time$unit, dyad_time$unit1)) > 0) {
        #stop('There are elements in unit_time$unit that do not appear in dyad_time$unit1')
    #}
    #if (length(setdiff(unit_time$unit, dyad_time$unit2) > 0)) {
        #stop('There are elements in unit_time$unit that do not appear in dyad_time$unit2')
    #}
    #if (length(setdiff(dyad_time$unit1, unit_time$unit) > 0)) {
        #stop('There are elements in dyad_time$unit1 that do not appear in unit_time$unit')
    #}
    #if (length(setdiff(dyad_time$unit2, unit_time$unit) > 0)) {
        #stop('There are elements in dyad_time$unit2 that do not appear in unit_time$unit')
    #}
#}
