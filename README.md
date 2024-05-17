
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
x <- rsp_find_profile("Ethanol")
x
#> respeciate profile list: 160
#> [NO SPECIES]
#>   (CODE 0291) Surface Coating Solvent - Methyl Alcohol
#>   (CODE 1070) Alcohols Production - Methanol - Purge Gas Vent
#>   (CODE 1071) Alcohols Production - Methanol - Distillation Vent
#>   (CODE 1132) Ethanolamines
#>   (CODE 1149) Methanol
#>   (CODE 1301) 10% Ethanol Composite (Hot Soak + Diurnal) Evaporative
#>     > showing 6 of 160
```

## speciate

``` r
p <- rsp_profile("8833")
```

## plot

Plotting a profile

``` r
#profiles have a default plot option
plot(p)
```

<img src="man/figures/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

… using lattice barchart syntax

``` r
p2 <- rsp_profile(c(8833, 8850))
plot(p2, key=list(space="top"))
```

<img src="man/figures/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
