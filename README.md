
# respeciate <img src="man/figures/logo.png" align="right" alt="" width="220" />

[![R build
status](https://github.com/atmoschem/respeciate/workflows/R-CMD-check/badge.svg)](https://github.com/atmoschem/respeciate/actions)

respeciate gives you access to air pollutant emissions profiles in the
[US/EPA Speciate
v5.2](https://www.epa.gov/air-emissions-modeling/speciate) and [EU/JRC
SPECIEUROPE v2.0](https://source-apportionment.jrc.ec.europa.eu/)
archives via R.

The installation is:

``` r
remotes::install_github("atmoschem/respeciate")
```

## Example

The currently packaged SPECIATE and SPECIEUROPE archives:

``` r
library(respeciate)
# packaged archives
rsp_info()
#> respeciate: 0.3.5
#> source: SPECIATE 5.2
#>  [in respeciate since 0.2.0]
#>  Profiles: 6845; species: 3035
#> source: SPECIEUROPE 2.0
#>  [in respeciate since 0.3.1]
#>  Profiles: 285; species: 231
```

Searching that for a profile, e.g. using a keyword:

``` r
rsp_find_profile("cement")
#> respeciate profile list: 82
#> [NO SPECIES]
#>   (CODE US:2720110) Cement Kiln (Gas-Fired)
#>   (CODE US:272012.5) Cement Kiln (Gas-Fired)
#>   (CODE US:2720130) Cement Kiln (Gas-Fired)
#>   (CODE US:27201C) Cement Kiln (Gas-Fired)
#>   (CODE US:2720310) Cement Kiln (Coal-Fired)
#>   (CODE US:272032.5) Cement Kiln (Coal-Fired)
#>     > showing 6 of 82
```

Limiting the search to just SPECIEUROPE

``` r
rsp_find_profile("cement", source="eu")
#> respeciate profile list: 11
#> [NO SPECIES]
#>   (CODE EU:1) Cement
#>   (CODE EU:32) Cement kiln (coal fired)
#>   (CODE EU:71) Cement production dust
#>   (CODE EU:72) Cement production dust
#>   (CODE EU:73) Cement production dust
#>   (CODE EU:126) Cement kiln
#>     > showing 6 of 11
```

``` r
# getting the first profile in SPECIEUROPE
prf <- rsp(1, source="eu")
prf
#> respeciate: count 1
#>   EU:1 (38 species) Cement
plot(prf)
```

![](man/figures/get-1.png)<!-- -->

``` r
#comparing that profile with pm profiles in SPECIATE
rsp_match_profile(prf, rsp_us_pm(), 
                  output = "plot,summary",
                  layout=c(5,2))
```

![](man/figures/output.options-1.png)<!-- -->

    #>    .profile.id                            .profile  n         pd        srd
    #> 1      US:4377                         Cement Kiln 28 0.29765994 0.05338075
    #> 2     US:91004 Draft Cement Production - Composite 28 0.17778945 0.06022444
    #> 3      US:4378                         Cement Kiln 28 0.36391799 0.06283360
    #> 4      US:4332                         Cement Kiln 28 0.23612659 0.06310738
    #> 5      US:4365                  Vegetative Burning 25 0.46103010 0.06430434
    #> 6      US:4325                         Cement Kiln 27 0.31685832 0.08085236
    #> 7      US:4348                   Unpaved Road Dust 26 0.08515821 0.07900131
    #> 8      US:4376                         Cement Kiln 28 0.39199238 0.07911302
    #> 9      US:4205                     Paved Road Dust 24 0.11406813 0.08829917
    #> 10   US:12707C     Hogged Fuel Boiler / Dutch Oven 26 0.26458592 0.08331874
    #>          sid   nearness
    #> 1  0.2657993 0.01418856
    #> 2  0.2365555 0.01424642
    #> 3  0.2497328 0.01569161
    #> 4  0.2654109 0.01674938
    #> 5  0.3118844 0.02005552
    #> 6  0.2525269 0.02041740
    #> 7  0.2694941 0.02129038
    #> 8  0.2772226 0.02193192
    #> 9  0.2537020 0.02240167
    #> 10 0.2925727 0.02437679

Notes:

- The nearest match to the SPECIEUROPE EU:1 profile Cement from the US
  EPA SPECIATE PM subset is SPECIATE US:4377 Cement Kiln.
- In addition, 5/9 of the other nearest matches are cement-related
  sources.
