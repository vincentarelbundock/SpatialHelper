testthat::skip("save simulation results")

library(here)
library(tidyverse)
setwd(here::here('tests/testthat'))

# create a data/ directory if none exist
if (!file.exists('data')) {
    dir.create('data')
}

# replicability
set.seed(55)

# panel
dat <- expand.grid('unit1' = 1:4, 'unit2' = 1:4, 'year' = 1980:1984, stringsAsFactors = FALSE)
dat$w <- runif(nrow(dat), 0, 100)
dat$y <- sample(0:1, nrow(dat), replace = TRUE)
write_csv(dat,"data/pan.csv")

# cross-section
dat <- expand.grid('unit1' = letters[1:6], 'unit2' = letters[1:6], 'year' = 2010, stringsAsFactors = FALSE)
dat$w <- runif(nrow(dat), 0, 100)
dat$y <- sample(0:1, nrow(dat), replace = TRUE)
write_csv(dat,"data/cs.csv")
