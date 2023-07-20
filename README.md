
# respeciate <img src="man/figures/logo.png" align="right" alt="" width="220" />

[![R build
status](https://github.com/atmoschem/respeciate/workflows/R-CMD-check/badge.svg)](https://github.com/atmoschem/respeciate/actions)

respeciate gives you access to the [US/EPA Speciate
v5.2](https://www.epa.gov/air-emissions-modeling/speciate) profiles in R

The installation is:

``` r
remotes::install_github("atmoschem/respeciate")
```

## example

Find profiles based on search criteria

``` r
library(respeciate)
x <- sp_find_profile("Ethanol")
x
#> respeciate profile reference
#> 0291 1070 1071 1132 1149 1301 1302 1303 1304 1314 ...
#>    > 160 profiles [showing first 10]
```

## speciate

``` r
p <- sp_profile("8833")
```

## plot

Plotting a profile

``` r
#profiles have a default plot option
plot(p)
```

<img src="man/figures/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

… using barplot syntax

``` r
#using base barplot 
p2 <- sp_profile(c(8833, 8850))
plot(p2, beside=TRUE)
```

<img src="man/figures/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
