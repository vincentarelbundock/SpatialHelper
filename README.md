SpatialHelper: A collection of helper functions for network analysis (TERGM) and spatial econometrics in R

# Installation

Install from Github:

```R
library(devtools)
devtools::install_github('vincentarelbundock/SpatialHelper')
library(SpatialHelper)
```

# Construct spatial-lagged variables in dyadic data

This follows: Neumayer, Eric, and Thomas Plümper. 2010. “Spatial Effects in Dyadic Data.” International Organization 64 (01): 145–166.

```R
# simulate dyadic data
unit = 1:10
dat = expand.grid('unit1' = unit, 'unit2' = unit, 'year' = 1980:1990, stringsAsFactors = FALSE)

# simulate weights
dat$w = rlnorm(nrow(dat))

# simulate dependent variable
dat$y = sample(as.numeric(dat$w > 2), nrow(dat), replace = TRUE)

# compute spatial lags
sl = dyadic_wy(dat, 
               origin = 'unit1',
               destination = 'unit2',
               time = 'year',
               y = 'y',
               w = 'w',
               weights = 'ik',
               type = 'aggregate_origin')
```

`SpatialHelper` can use the `furrr` package to run the command in parallel on multiple cores. To use 4 cores, just call the following code before executing `dyadic_wy`:

```r
library(furrr)
plan(multiprocess, workers = 4)
```

# Estimate a TERGM model

Load libraries:

```R
library(btergm)
```

Simulate two test datasets:

1.  `unit_time` is a unit/time panel data.frame with vertex attributes. 
2. `dyad_time` is a directed-dyad/time data.frame, where variables will serve as endogenous or exogenous networks in the estimation. 

Note that the `unit`, `unit1`, `unit2`, and `time` column names are hard-coded in the helper functions; they *must* be present in the data.frames.

```R
unit = letters
time = 1:10
unit_time = expand.grid('unit' = unit, 'time' = time, stringsAsFactors = FALSE) 
unit_time$x = rnorm(nrow(unit_time))
unit_time$k = rnorm(nrow(unit_time))

dyad_time = expand.grid('unit1' = unit, 'unit2' = unit, 'time' = time, stringsAsFactors = FALSE) 
dyad_time$w = rnorm(nrow(dyad_time))
dyad_time$e = rnorm(nrow(dyad_time))
dyad_time$z = as.numeric(ifelse(dyad_time$w + dyad_time$e > 0, 1, 0))

> head(unit_time)
  unit time          x           k
1    a    1  1.2643963  0.68617487
2    b    1  0.5872735 -0.54044849
3    c    1 -1.2506704 -0.22447699
4    d    1  1.2752751  0.20417358
5    e    1 -0.9055344 -0.70885111
6    f    1 -1.3746392  0.03542892

> head(dyad_time)
  unit1 unit2 time          w           e z
1     a     a    1 -0.5964175 -1.22861230 0
2     b     a    1  0.7060516  0.02783549 1
3     c     a    1  1.4097574 -0.43246407 1
4     d     a    1 -0.7798457 -0.09711169 0
5     e     a    1 -1.5351066 -0.69116781 0
6     f     a    1  1.7755478 -0.43492290 1
```

Prepare the network data and store it inside an `environment` object:

```R
# Convert the panel data to network data inside an environment
env = panel_to_network(unit_time, dyad_time)

# Identify the dependent network
env = dependent_network('z', env)
```

Attach the environment and estimate the model:

```R
attach(env)
f = z  ~ edges + twopath + nodecov('x') + nodecov('k') + edgecov(w) + istar(2)
mod = btergm(f, R = 500)
detach(env)

> summary(mod)
==========================
Summary of model fit
==========================

Formula:   z ~ edges + twopath + nodecov("x") + nodecov("k") + edgecov(w) 

Time steps: 10 

Bootstrapping sample size: 500 

Estimates and 95% confidence intervals:
                Estimate    2.5%   97.5%
edges           0.425652 -0.6223  1.9335
twopath         0.023176 -0.0203  0.0530
nodecov.x      -0.017120 -0.0725  0.0329
nodecov.k      -0.020880 -0.0904  0.0466
edgecov.w[[i]]  0.822661  0.7625  0.8935
```
