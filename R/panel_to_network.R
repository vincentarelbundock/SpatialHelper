sanity_check_pre = function(unit_time, dyad_time) {
    if ((!class(unit_time)[1] == 'data.frame') |
        (!class(dyad_time)[1] == 'data.frame')) {
        stop("Input data are in data.frames (not tibble, matrix, etc.)")
    }
    if (any(is.na(unit_time))) {
        warning('unit_time contains missing values.')
    }
    if (any(is.na(dyad_time))) {
        warning('dyad_time contains missing values.')
    }
    if ((!'unit' %in% colnames(unit_time)) |
        (!'time' %in% colnames(unit_time))) {
        stop("unit_time must include columns named 'unit' and 'time'.")
    }
    if ((!'unit1' %in% colnames(dyad_time)) |
        (!'unit2' %in% colnames(dyad_time)) |
        (!'time' %in% colnames(dyad_time))) {
        stop("dyad_time must include columns named 'unit1', 'unit2', and 'time'.")
    }
    if ((class(unit_time$unit)[1] == 'factor') |
        (class(unit_time$time)[1] == 'factor') |
        (class(dyad_time$unit1)[1] == 'factor') |
        (class(dyad_time$unit2)[1] == 'factor') |
        (class(dyad_time$time)[1] == 'factor')) {
        stop("'unit', 'unit1', 'unit2', and 'time' columns cannot be factors.")
    }
    if ((class(dyad_time$unit1)[1] != class(unit_time$unit)[1]) |
        (class(dyad_time$unit2)[1] != class(unit_time$unit)[1]) |
        (class(dyad_time$time)[1] != class(unit_time$time)[1])) {
        stop("Indices must be of the same type in dyad_time and unit_time.")
    }
    idx = unit_time[, c('unit', 'time')]
    idx = unique(idx)
    if (nrow(idx) < nrow(unit_time)) {
        stop('There should not be duplicate Unit-Time indices in unit_time')
    }
    idx = dyad_time[, c('unit1', 'unit2', 'time')]
    idx = unique(idx)
    if (nrow(idx) < nrow(dyad_time)) {
        stop('There should not be duplicate Unit1-Unit2-Time indices in dyad_time')
    }
    idx = length(unique(unit_time$unit)) * length(unique(unit_time$time))
    if (nrow(unit_time) < idx) {
        stop('unit_time is missing some Unit-Time combinations. Panel data must be "rectangular".')
    }
    a = length(unique(dyad_time$unit1))
    b = length(unique(dyad_time$unit2))
    d = length(unique(dyad_time$time))
    idx = a * b * d
    if (nrow(dyad_time) < idx) {
        stop('dyad_time is missing some Unit1-Unit2-Time combinations. Dyadic panel data must be "rectangular".')
    }
    # common units
    units_dyad = c(dyad_time$unit1, dyad_time$unit2)
    units_unit = unit_time$unit
    units_setdiff = sort(unique(c(setdiff(units_dyad, units_unit),
                                  setdiff(units_unit, units_dyad))))
    if (length(units_setdiff) > 0) {
        msg = paste(units_setdiff, collapse = ', ')
        warning(paste('The following units will be excluded because they do not appear in both the monadic and the dyadic data:', msg))
    }
    units = intersect(units_dyad, units_unit)
    units = unique(units)
    # common time
    times_dyad = dyad_time$time
    times_unit = unit_time$time
    times = intersect(times_dyad, times_unit)
    times = unique(times)
    times_setdiff = sort(unique(c(setdiff(times_dyad, times_unit),
                                  setdiff(times_unit, times_dyad))))
    if (length(times_setdiff) > 0) {
        msg = paste(times_setdiff, collapse = ', ')
        warning(paste('The following time periods will be excluded because they do not appear in both the monadic and the dyadic data:', msg))
    }
    # output
    out = list('common_units' = units, 'common_times' = times)
    return(out)
}

sanity_check_post = function(network_environment) {
    # alignment
    for (n in names(network_environment)) {
        if (n != 'vertex.attributes') {
            for (i in seq_along(network_environment[[n]])) {
                fail1 = any(colnames(network_environment[[n]][[i]]) != names(network_environment[['vertex.attributes']][[i]]$unit))
                fail2 = any(row.names(network_environment[[n]][[i]]) != names(network_environment[['vertex.attributes']][[i]]$unit))
                if (fail1 | fail2) {
                    stop('Panel and Dyadic data are not properly aligned.')
                }
            }
        }
    }
}

prep_attributes = function(unit_time) {
    out = split(unit_time, unit_time$time) 
    out = lapply(out, as.list)
    return(out)
}

prep_dyads = function(dyad_time, cores = 1) {
    out = split(dyad_time, dyad_time$time) 
    if (cores > 1) {
        out = parallel::mclapply(out, prep_dyad_crosssection, mc.cores = cores)
    } else {
        out = lapply(out, prep_dyad_crosssection)
    }
    return(out)
}

prep_dyad_crosssection = function(dyad_crosssection) {
    vars = names(dyad_crosssection)[!names(dyad_crosssection) %in% c('unit1', 'unit2', 'time')]
    out = lapply(vars, function(x) prep_variable(dyad_crosssection, x))
    names(out) = vars
    return(out)
}

prep_variable = function(dyad_crosssection, v) {
    out = dyad_crosssection[, c('unit1', 'unit2', v)]
    out = stats::reshape(out, timevar = "unit2", idvar = "unit1", direction = 'wide')
    row.names(out) = out[, 1]
    out = out[, -1]
    colnames(out) = gsub(paste0(v, '\\.'), '', colnames(out))
    out = as.matrix(out)
    return(out)
}

#' Converts data.frames to objects amenable to network analysis using `btergm`
#' @param dyad_time data.frame dyadic dataset with columns named `unit1`,
#' `unit2`, `time`. Additional columns are edge attributes. 
#' @param unit_time data.frame unit/time dataset with columns named `unit`,
#' `time`. Additional columns are vertex attributes
#' @param cores integer number of cores to use for computation with mclapply
#' @param verbose print progress report if TRUE
#' @examples
#' \dontrun{
#'   # Examples are in the README file at
#'   http://github.com/vincentarelbundock/SpatialHelper
#; }
#' @export
panel_to_network = function(unit_time, dyad_time, cores = 1, verbose = TRUE) {
    # sanity checks pre
    if (verbose) cat('Sanity checks...\n')
    idx = sanity_check_pre(unit_time, dyad_time)
    units = idx$common_units
    times = idx$common_times

    # subset and sort
    if (verbose) cat('Subsetting and ordering...\n')
    unit_time = unit_time[unit_time$unit %in% units,]
    unit_time = unit_time[unit_time$time %in% times,]
    unit_time = unit_time[order(unit_time$unit, unit_time$time),]
    dyad_time = dyad_time[dyad_time$unit1 %in% units,]
    dyad_time = dyad_time[dyad_time$unit2 %in% units,]
    dyad_time = dyad_time[dyad_time$time %in% times,]
    dyad_time = dyad_time[order(dyad_time$unit1, dyad_time$unit2, dyad_time$time),]

    # df to matrices
    if (verbose) cat('Edge attributes...\n')
    dyad_time = prep_dyads(dyad_time, cores = cores)
    if (verbose) cat('Vertex attributes...\n')
	unit_time = prep_attributes(unit_time)

	# prepare environment
    if (verbose) cat('Environment...\n')
	env = new.env()
    vars = names(dyad_time[[1]])
	for (v in vars) {
		env[[v]] = lapply(seq_along(dyad_time), function(i) dyad_time[[i]][[v]])
	}
    env[['vertex.attributes']] = unit_time
    class(env) = c(class(env), 'network_environment')

    # sanity checks post
    if (verbose) cat('More sanity checks...\n')
    sanity_check_post(env)

    # output
    return(env)
}

#' Identifies the network object that will serve as dependent (endogenous)
#' network in the `btergm` analysis.
#' @param network_dependent name of the dependent network (character)
#' @param network_environment an environment produced by the panel_to_network
#' function.
#' @param ... arguments will be passed to the network::network function.
#' @note unnamed arguments (e.g., `directed`, `loops`) will be passed to the
#' `network::network` function that is used under the hood to create network
#' objects.
#' @export
dependent_network = function(network_dependent, network_environment, ...) {
    if (!'network_environment' %in% class(network_environment)) {
        stop("The network_environment must be produced by the `panel_to_network` function.")
    }
    if ((class(network_dependent)[1] != 'character') |
        (length(network_dependent) != 1)) {
        stop("network_dependent must be a string of length 1")
    }
    if (!network_dependent %in% names(network_environment)) {
        stop(paste(network_dependent, 'is not available in the network_environment.'))
    }
    tmp = lapply(seq_along(network_environment$vertex.attributes), function(i)
                 network::network(x = network_environment[[network_dependent]][[i]],
                                  vertex.attr = network_environment$vertex.attributes[[i]],
                                  vertex.attrnames = names(network_environment$vertex.attributes[[i]]), ...))
    out = network_environment
    out[[network_dependent]] = tmp 
    return(out)
}
