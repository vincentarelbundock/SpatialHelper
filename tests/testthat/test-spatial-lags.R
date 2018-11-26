library(parallel)
library(testthat)

# Test data
unit = 1:10
dat = expand.grid('unit1' = unit, 'unit2' = unit, stringsAsFactors = FALSE)
dat = dat[sample(1:nrow(dat), nrow(dat)),] # shuffle
dat$w = rlnorm(nrow(dat))
dat$y = sample(as.numeric(dat$w > 2), nrow(dat), replace = TRUE)

test_that('aggregate source / row_normalize = FALSE', {
    #y_ij = sum_k!=i sum_m w * y_km
    k = dyadic_wy(dat, type = 'aggregate_source', weights = 'ik', progress = FALSE, row_normalize = FALSE, zero_loop = FALSE)
    tmp = merge(dat, k)
    for (a in 1:nrow(tmp)) {
        src = tmp$unit1[a]
        tar = tmp$unit2[a]
        w = NULL
        y = NULL
        for (b in 1:nrow(tmp)) {
            if (tmp$unit1[b] != src) {
                y = c(y, tmp$y[b])
                w = c(w, tmp$w[(tmp$unit1 == src) & (tmp$unit2 == tmp$unit1[b])])
            }
        }
        result = sum(w * y)
        testthat::expect_equal(result, tmp$wy[a])
    }
})

test_that('aggregate target / weigths = "jm" / row_normalize = FALSE', {
    #y_ij = sum_k sum_m!=j w * y_km
    k = dyadic_wy(dat, type = 'aggregate_target', weights = 'jm', progress = FALSE, row_normalize = FALSE, zero_loop = FALSE)
    tmp = merge(dat, k)
    for (a in 1:nrow(tmp)) {
        src = tmp$unit1[a]
        tar = tmp$unit2[a]
        w = NULL
        y = NULL
        for (b in 1:nrow(tmp)) {
            if (tmp$unit2[b] != tar) {
                y = c(y, tmp$y[b])
                w = c(w, tmp$w[(tmp$unit1 == tar) & (tmp$unit2 == tmp$unit2[b])])
            }
        }
        result = sum(w * y)
        testthat::expect_equal(result, tmp$wy[a])
    }
})

test_that('specific source / row_normalize = FALSE', {
    #y_ij = sum_k!=i w * y_kj
    k = dyadic_wy(dat, type = 'specific_source', weights = 'ik', progress = FALSE, row_normalize = FALSE, zero_loop = FALSE)
    tmp = merge(dat, k)
    for (a in 1:nrow(tmp)) {
        src = tmp$unit1[a]
        tar = tmp$unit2[a]
        w = NULL
        y = NULL
        for (b in 1:nrow(tmp)) {
            if (tmp$unit1[b] != src) {
                if (tmp$unit2[b] == tar) {
                    y = c(y, tmp$y[b])
                    w = c(w, tmp$w[(tmp$unit1 == src) & (tmp$unit2 == tmp$unit1[b])])
                }
            }
        }
        result = sum(w * y)
        testthat::expect_equal(result, tmp$wy[a])
    }
})

test_that('specific source / row_normalize = TRUE', {
    #y_ij = sum_k!=i w * y_kj
    k = dyadic_wy(dat, type = 'specific_source', weights = 'ik', progress = FALSE, row_normalize = TRUE, zero_loop = FALSE)
    tmp = merge(dat, k)
    for (a in 1:nrow(tmp)) {
        src = tmp$unit1[a]
        tar = tmp$unit2[a]
        w = NULL
        y = NULL
        for (b in 1:nrow(tmp)) {
            if (tmp$unit1[b] != src) {
                if (tmp$unit2[b] == tar) {
                    y = c(y, tmp$y[b])
                    w = c(w, tmp$w[(tmp$unit1 == src) & (tmp$unit2 == tmp$unit1[b])])
                }
            }
        }
        w = w / sum(w)
        result = sum(w * y)
        testthat::expect_equal(result, tmp$wy[a])
    }
})
    
test_that('specific target / row_normalize = FALSE / weights ik', {
    #y_ij = sum_m!=j w * y_im
    k = dyadic_wy(dat, type = 'specific_target', weights = 'ik', progress = FALSE, row_normalize = FALSE, zero_loop = FALSE)
    tmp = merge(dat, k)
    for (a in 1:nrow(tmp)) {
        src = tmp$unit1[a]
        tar = tmp$unit2[a]
        w = NULL
        y = NULL
        for (b in 1:nrow(tmp)) {
            if (tmp$unit2[b] != tar) {
                if (tmp$unit1[b] == src) {
                    y = c(y, tmp$y[b])
                    w = c(w, tmp$w[(tmp$unit1 == src) & (tmp$unit2 == tmp$unit1[b])])
                }
            }
        }
        result = sum(w * y)
        testthat::expect_equal(result, tmp$wy[a])
    }
})

test_that('specific target / row_normalize = FALSE / weights im', {
    #y_ij = sum_m!=j w * y_im
    k = dyadic_wy(dat, type = 'specific_target', weights = 'im', progress = FALSE, row_normalize = FALSE, zero_loop = FALSE)
    tmp = merge(dat, k)
    for (a in 1:nrow(tmp)) {
        src = tmp$unit1[a]
        tar = tmp$unit2[a]
        w = NULL
        y = NULL
        for (b in 1:nrow(tmp)) {
            if (tmp$unit2[b] != tar) {
                if (tmp$unit1[b] == src) {
                    y = c(y, tmp$y[b])
                    w = c(w, tmp$w[(tmp$unit1 == src) & (tmp$unit2 == tmp$unit2[b])])
                }
            }
        }
        result = sum(w * y)
        testthat::expect_equal(result, tmp$wy[a])
    }
})
