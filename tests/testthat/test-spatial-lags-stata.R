###############################
#  Command line instructions  #
###############################
# git clone https://github.com/vincentarelbundock/SpatialHelper ~/Downloads/SpatialHelper
# cd ~/Downloads/SpatialHelper/tests/testthat
# R CMD batch spatial-lags-simulations.R
# stata -b spatial-lags-stata.do
# R
# library(devtools)
# test() 

skip("no stata test data")

library(here)
library(testthat)
library(tidyverse)
library(SpatialHelper)
setwd(here::here('tests/testthat'))

## simulate data
#source('spatial-lags-simulations.R')

# stata batch run
#system('/usr/local/stata14/stata-se -b do spatial-lags-stata.do')

# delete stata cruft
unlink(Sys.glob('SE_file*dta'))
unlink('spatial-lags-stata.log')

# List of parameters that we will be testing. Parameters are guessed based on
# the filenames given by Stata. Each row of the `parameters` dataset represents
# a different combination of parameters (i.e., type, weights, row_normalize)
parameters <- tibble(# list of csv files produced by Stata.
                     filename = Sys.glob('data/*_*_*.csv'),
                     # panel or cross-sectional?
                     dataset = ifelse(str_detect(filename, 'cs_'), 'cs', 'pan'),
                     # spatial lag type
                     type = case_when(str_detect(filename, 'aggregate_destination') ~ 'aggregate_destination',
                                      str_detect(filename, 'aggregate_origin') ~ 'aggregate_origin',
                                      str_detect(filename, 'specific_destination') ~ 'specific_destination',
                                      str_detect(filename, 'specific_origin') ~ 'specific_origin'),
                     # weights?
                     weight = str_replace(filename, '.*_([a-z][a-z])[_|\\.].*', '\\1'),
                     # row_normalize = TRUE/FALSE
                     row_normalize = !str_detect(filename, 'norowst'),
                     # index for convenience
                     idx = 1:length(filename))

# R to Stata comparison function
# compare_r_stata(4) compares the R and Stata results for the parameters in the
# 4th row of the parameters dataframe. 
compare_r_stata <- function(i) {

    # test_that environment gives pretty+informative reporting of results
    test_that(parameters$filename[i], {

        # load stata results
        tmp <- read_csv(parameters$filename[i]) %>%
               select(unit1, unit2, year, w, y, wy_stata = wy)

        # construct spatial weights using R
        tmp <- dyadic_wy(tmp, origin = 'unit1', destination = 'unit2', 
                         y = 'y', w = 'w', wy = 'wy_r', time = 'year', 
                         type = parameters$type[i], 
                         weights = parameters$weight[i], 
                         row_normalize = parameters$row_normalize[i])

        # compare R to Stata
        expect_equal(tmp$wy_r, tmp$wy_stata, tolerance = 1e-4)

     })
}

###############
#  Run tests  #
###############
for (type in c('specific_origin', 'specific_destination', 
               'aggregate_origin', 'aggregate_destination')) {
    # rows in parameters 
    idx <- parameters$idx[parameters$type == type]

    # which weights are tested for each type of spatial lag?
    w <- parameters$weight[idx] %>% unique %>% paste(collapse = ', ')

    # print message to identify the test
    context(paste0(type, ': ', w))

    # test
    for (i in idx) compare_r_stata(i)
}
