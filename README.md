
# respeciate <img src="man/figures/logo.png" align="right" alt="" width="220" />

[![R build
status](https://github.com/atmoschem/respeciate/workflows/R-CMD-check/badge.svg)](https://github.com/atmoschem/respeciate/actions)

respeciate gives you access to the [US/EPA Speciate
v5.1](https://www.epa.gov/air-emissions-modeling/speciate) profiles

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
#> 8733 8736 8757 8758 8760 8761 8763 8764 8765 8766 8767 8768 8769 8770 8771 8772 
#> 8773 8827 8828 8829 8830 8831 8832 8833 8834 8835 8836 8837 8838 8839 8840 8841 
#> 8842 8843 8844 8845 8846 8847 8848 8849 8850 8851 8852 8853 8854 8855 8863 8864 
#> 8865 8866 8867 8868 8869 8870 8871 8872 8884 8885 8886 8887 8888 8889 8934
#>    > 63 profiles
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

… or multiple profiles using barplot syntax

``` r
#using base barplot 
p2 <- sp_profile(c(8833, 8850))
plot(p2, beside=TRUE)
```

<img src="man/figures/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
