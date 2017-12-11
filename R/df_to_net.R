sanity_unit_time = function(unit_time) {
    test_that("Data.frame includes columns: 'unit', 'time'", {
        expect_true('unit' %in% colnames(unit_time))
        expect_true('time' %in% colnames(unit_time))
    })
    test_that("'unit' and 'time' columns cannot be a factors", {
        expect_false(class(unit_time$unit)[1] == 'factor')
        expect_false(class(unit_time$time)[1] == 'factor')
    })
    test_that("Unique Unit-Year indices", {
        idx = unit_time[, c('unit', 'time')]
        idx = unique(idx)
        expect_equal(nrow(idx), nrow(unit_time))
    })
    test_that("Panel data is rectangular", {
        expect_equal(nrow(unit_time), 
                     length(unique(unit_time$unit)) * length(unique(unit_time$time)))
    })
}

sanity_dyad_time = function(dyad_time) {
    test_that("Data.frame includes the following columns: 'unit1', 'unit2', 'time'", {
        expect_true('unit1' %in% colnames(dyad_time))
        expect_true('unit2' %in% colnames(dyad_time))
        expect_true('time' %in% colnames(dyad_time))
    })
    test_that("'unit1', 'unit2', and 'time' columns cannot be factors", {
        expect_false(class(dyad_time$unit1)[1] == 'factor')
        expect_false(class(dyad_time$unit2)[1] == 'factor')
        expect_false(class(dyad_time$time)[1] == 'factor')
    })
    test_that("'unit1' and 'unit2' are of the same type", {
        expect_true(class(dyad_time$unit1)[1] == class(dyad_time$unit1)[1])
    })
    test_that("Unique Unit1-Unit2-Year indices", {
        idx = dyad_time[, c('unit1', 'unit2', 'time')]
        idx = unique(idx)
        expect_equal(nrow(idx), nrow(dyad_time))
    })
    test_that("Dyadic panel data is rectangular", {
        a = length(unique(dyad_time$unit1))
        b = length(unique(dyad_time$unit2))
        d = length(unique(dyad_time$time))
        x = a * b * d
        expect_equal(nrow(dyad_time), x)
    })
}

prep_attributes = function(unit_time) {
    out = split(unit_time, unit_time$time) 
    out = lapply(out, as.list)
    return(out)
}

prep_variable = function(dyad, v) {
    out = dyad[, c('unit1', 'unit2', v)]
    out = reshape(out, timevar = "unit2", idvar = "unit1", direction = 'wide')
    row.names(out) = out[, 1]
    out = out[, -1]
    colnames(out) = gsub(paste0(v, '\\.'), '', colnames(out))
    out = as.matrix(out)
    return(out)
}

prep_dyad = function(dyad_cs) {
    vars = names(dyad_cs)[!names(dyad_cs) %in% c('unit1', 'unit2', 'time')]
    out = lapply(vars, function(x) prep_variable(dyad_cs, x))
    names(out) = vars
    return(out)
}

prep_dyads = function(dyad_time) {
    out = split(dyad_time, dyad_time$time) 
    out = lapply(out, prep_dyad)
    return(out)
}

prep_network = function(dv, iv, ...) {
	out = lapply(seq_along(dv), function(i)
				 network::network(x = dv[[i]],
								  vertex.attr = iv[[i]],
								  vertex.attrnames = names(iv[[i]]), ...))
    return(out)
}

#' Align and subset matrices using a vector of unit names
#' @param endo_net character name of the dependent variable (from `dyad_time`)
#' @param dyad_time data.frame dyadic dataset with columns named `unit1`,
#' `unit2`, `time`. Additional columns are used to create the endogenous and
#' exogenous networks
#' @param unit_time data.frame unit/time dataset with columns named `unit`,
#' `time`. Additional columns are vertex attributes
#' @note unnamed arguments (e.g., `directed`, `loops`) will be passed to the
#' `network::network` function
#' @examples
#' # Examples are in the README file at
#' http://github.com/vincentarelbundock/btergmHelper
#' @export
df_to_net = function(endo_net, unit_time, dyad_time, ...) {
    # sanity checks
    sanity_unit_time(unit_time)
    sanity_dyad_time(dyad_time)
    test_that("Indices in the two datasets are of compatible types", {
        expect_true(class(dyad_time$unit1)[1] == class(unit_time$unit)[1])
        expect_true(class(dyad_time$time)[1] == class(unit_time$time)[1])
    })
    # common units
    units_dyad = c(dyad_time$unit1, dyad_time$unit2)
    units_unit = unit_time$unit
    units = intersect(units_dyad, units_unit)
    units = unique(units)
    # common time
    times_dyad = dyad_time$time
    times_unit = unit_time$time
    times = intersect(times_dyad, times_unit)
    times = unique(times)
    # subset and sort
    dyad_time = dyad_time %>%
                dplyr::filter(unit1 %in% units,
                              unit2 %in% units,
                              time %in% times) %>%
                dplyr::arrange(unit1, unit2, time) %>%
                dplyr::select(unit1, unit2, time, order(names(.)))
    unit_time = unit_time %>%
                dplyr::filter(unit %in% units,
                              time %in% times) %>%
                dplyr::arrange(unit, time) %>%
                dplyr::select(unit, time, order(names(.)))
    # df to matrices
    dyad_time = prep_dyads(dyad_time)
	unit_time = prep_attributes(unit_time)
	# sanity checks
    test_that("IV and DV are aligned.", {
		for (i in seq_along(dyad_time)) {
	        expect_true(all(colnames(dyad_time[[i]][[1]]) == unit_time[[i]]$unit))
		}
    })
    test_that("IV and DV have the same number of time periods", {
	    expect_true(length(dyad_time) == length(unit_time))
    })
	# output
	env = new.env()
    vars = names(dyad_time[[1]])
	for (v in vars) {
		env[[v]] = lapply(seq_along(dyad_time), function(i) dyad_time[[i]][[v]])
	}
	env[['net']] = prep_network(env[[endo_net]], unit_time, ...)
    return(env)
}
