context('Dyadic spatial lags')

unit = letters[1:3]
time = 1:2
dyad_time = expand.grid('unit1' = unit, 'unit2' = unit, 'time' = time, stringsAsFactors = FALSE) 
dyad_time$w = rnorm(nrow(dyad_time))
dyad_time$y = rnorm(nrow(dyad_time))

test_that('specific source', {
    #y_ij = rho * sum w_ik y_kj
    #k != i
	tmp = sl_dyad_specific_panel(dyad_time, 'unit1', 'unit2', 'time', 'w', 'y',
		                         type = 'source', row_normalize = FALSE, zero_diag = TRUE)
	tmp = dyad_time %>% 
          dplyr::left_join(tmp, by = c('unit1', 'unit2', 'time'))
	tmp = tmp[tmp$time == 2, ]
	a = dplyr::filter(tmp, unit1 == 'a', unit2 == 'b')$wy
	b = dplyr::filter(tmp, unit1 == 'a', unit2 == 'b')$w * 
        dplyr::filter(tmp, unit1 == 'b', unit2 == 'b')$y +
		dplyr::filter(tmp, unit1 == 'a', unit2 == 'c')$w * 
        dplyr::filter(tmp, unit1 == 'c', unit2 == 'b')$y 
	testthat::expect_equal(a, b)
})

test_that('specific target', {
    #y_ij = rho * sum w_jk y_ik
    #k != j
	tmp = sl_dyad_specific_panel(dyad_time, 'unit1', 'unit2', 'time', 'w', 'y', 
						         type = 'target', row_normalize = FALSE, zero_diag = TRUE)
	tmp = dyad_time %>% 
          dplyr::left_join(tmp, by = c('unit1', 'unit2', 'time'))
	tmp = tmp[tmp$time == 2, ]
	a = dplyr::filter(tmp, unit1 == 'a', unit2 == 'b')$wy
	b = dplyr::filter(tmp, unit1 == 'b', unit2 == 'a')$w * 
        dplyr::filter(tmp, unit1 == 'a', unit2 == 'a')$y +
		dplyr::filter(tmp, unit1 == 'b', unit2 == 'c')$w * 
        dplyr::filter(tmp, unit1 == 'a', unit2 == 'c')$y 
	testthat::expect_equal(a, b)
})
