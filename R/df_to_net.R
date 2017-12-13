sanity_unit_time = function(unit_time) {
    testthat::test_that("Data.frame includes columns: 'unit', 'time'", {
        testthat::expect_true('unit' %in% colnames(unit_time))
        testthat::expect_true('time' %in% colnames(unit_time))
    })
    testthat::test_that("'unit' and 'time' columns cannot be a factors", {
        testthat::expect_false(class(unit_time$unit)[1] == 'factor')
        testthat::expect_false(class(unit_time$time)[1] == 'factor')
    })
    testthat::test_that("Unique Unit-Year indices", {
        idx = unit_time[, c('unit', 'time')]
        idx = unique(idx)
        testthat::expect_equal(nrow(idx), nrow(unit_time))
    })
    testthat::test_that("Panel data is rectangular", {
        testthat::expect_equal(nrow(unit_time), 
                     length(unique(unit_time$unit)) * length(unique(unit_time$time)))
    })
}

sanity_dyad_time = function(dyad_time) {
    testthat::test_that("Data.frame includes the following columns: 'unit1', 'unit2', 'time'", {
        testthat::expect_true('unit1' %in% colnames(dyad_time))
        testthat::expect_true('unit2' %in% colnames(dyad_time))
        testthat::expect_true('time' %in% colnames(dyad_time))
    })
    testthat::test_that("'unit1', 'unit2', and 'time' columns cannot be factors", {
        testthat::expect_false(class(dyad_time$unit1)[1] == 'factor')
        testthat::expect_false(class(dyad_time$unit2)[1] == 'factor')
        testthat::expect_false(class(dyad_time$time)[1] == 'factor')
    })
    testthat::test_that("'unit1' and 'unit2' are of the same type", {
        testthat::expect_true(class(dyad_time$unit1)[1] == class(dyad_time$unit1)[1])
    })
    testthat::test_that("Unique Unit1-Unit2-Year indices", {
        idx = dyad_time[, c('unit1', 'unit2', 'time')]
        idx = unique(idx)
        testthat::expect_equal(nrow(idx), nrow(dyad_time))
    })
    testthat::test_that("Dyadic panel data is rectangular and directed", {
        a = length(unique(dyad_time$unit1))
        b = length(unique(dyad_time$unit2))
        d = length(unique(dyad_time$time))
        x = a * b * d
        testthat::expect_equal(nrow(dyad_time), x)
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

prep_dyads = function(dyad_time, cores = 1) {
    out = split(dyad_time, dyad_time$time) 
    if (cores > 1) {
        out = mclapply(out, prep_dyad, mc.cores = cores)
    } else {
        out = lapply(out, prep_dyad)
    }
    return(out)
}

#' Converts a list of matrices and vertex attributes to a dependent network for
#' btergm
#' @param dv list of matrix stored in the environment produced by `df_to_mat`
#' @param iv list of vertex attributes stored in the `vertex.attributes` object
#' in the environment produced by `df_to_mat`
#' @note unnamed arguments (e.g., `directed`, `loops`) will be passed to the
#' `network::network` function that is used under the hood to create network
#' objects.
#' @export
mat_to_net = function(dv, iv, ...) {
    out = lapply(seq_along(dv), function(i)
                 network::network(x = dv[[i]],
                                  vertex.attr = iv[[i]],
                                  vertex.attrnames = names(iv[[i]]), ...))
    return(out)
}

#' Converts data.frames to matrices amenable to network analysis with btergm
#' @param dyad_time data.frame dyadic dataset with columns named `unit1`,
#' `unit2`, `time`. Additional columns are used to create the endogenous and
#' exogenous networks
#' @param unit_time data.frame unit/time dataset with columns named `unit`,
#' `time`. Additional columns are vertex attributes
#' @param cores integer number of cores to use for computation with mclapply
#' @examples
#' # Examples are in the README file at
#' http://github.com/vincentarelbundock/btergmHelper
#' @export
df_to_mat = function(unit_time, dyad_time, cores = 1) {
    # sanity checks
    sanity_unit_time(unit_time)
    sanity_dyad_time(dyad_time)
    testthat::test_that("Indices in the two datasets are of compatible types", {
        testthat::expect_true(class(dyad_time$unit1)[1] == class(unit_time$unit)[1])
        testthat::expect_true(class(dyad_time$time)[1] == class(unit_time$time)[1])
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
    dyad_time = prep_dyads(dyad_time, cores = cores)
	unit_time = prep_attributes(unit_time)
	# sanity checks
    testthat::test_that("IV and DV are aligned.", {
		for (i in seq_along(dyad_time)) {
            testthat::expect_true(all(colnames(dyad_time[[i]][[1]]) == unit_time[[i]]$unit))
		}
    })
    testthat::test_that("IV and DV have the same number of time periods", {
	    testthat::expect_true(length(dyad_time) == length(unit_time))
    })
	# output
	env = new.env()
    vars = names(dyad_time[[1]])
	for (v in vars) {
		env[[v]] = lapply(seq_along(dyad_time), function(i) dyad_time[[i]][[v]])
	}
    env[['vertex.attributes']] = unit_time
    return(env)
}
