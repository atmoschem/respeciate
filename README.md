
# respeciate <img src="man/figures/logo.png" align="right" alt="" width="220" />

[![R build
status](https://github.com/atmoschem/respeciate/workflows/R-CMD-check/badge.svg)](https://github.com/atmoschem/respeciate/actions)

respeciate give you access to the [US/EPA Speciate
v5.1](https://www.epa.gov/air-emissions-modeling/speciate) profiles

The installation is:

``` r
remotes::install_github("atmoschem/respeciate")
```

## example

find PROFILE\_CODE

``` r
library(respeciate)

x <- find_code("Ethanol")
x$PROFILE_CODE
#>  [1] "8733" "8736" "8757" "8758" "8760" "8761" "8763" "8764" "8765" "8766"
#> [11] "8767" "8768" "8769" "8770" "8771" "8772" "8773" "8827" "8828" "8829"
#> [21] "8830" "8831" "8832" "8833" "8834" "8835" "8836" "8837" "8838" "8839"
#> [31] "8840" "8841" "8842" "8843" "8844" "8845" "8846" "8847" "8848" "8849"
#> [41] "8850" "8851" "8852" "8853" "8854" "8855" "8863" "8864" "8865" "8866"
#> [51] "8867" "8868" "8869" "8870" "8871" "8872" "8884" "8885" "8886" "8887"
#> [61] "8888" "8889" "8934"
```

## speciate

``` r
PROFILE_CODE = "8855"
dt <- spec(PROFILE_CODE)
#> Sum WEIGHT_PERCENT:  100
head(dt, 2)
#>   REF_Code PROFILE_CODE SPECIES_ID
#> 1  EPA2013         8855         30
#> 2  EPA2013         8855         44
#>                                                                          PROFILE_NAME
#> 1 Gasoline Exhaust - Tier 2 light-duty vehicles using 85% Ethanol - Composite Profile
#> 2 Gasoline Exhaust - Tier 2 light-duty vehicles using 85% Ethanol - Composite Profile
#>   PROFILE_TYPE MASTER_POLLUTANT QSCORE QSCORE_DESC QUALITY      CONTROLS
#> 1          GAS              TOG     NA                   B Not Available
#> 2          GAS              TOG     NA                   B Not Available
#>        PROFILE_DATE
#> 1 10/15/10 00:00:00
#> 2 10/15/10 00:00:00
#>                                                               PROFILE_NOTES
#> 1 Composite profile from four Tier 2 FFVs on one E85 fuel, LA92 test cycle.
#> 2 Composite profile from four Tier 2 FFVs on one E85 fuel, LA92 test cycle.
#>   TOTAL
#> 1   100
#> 2   100
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        TEST_METHOD
#> 1 The test cycle consists of a cold start Phase 1 (first 310 seconds), a stabilized Phase 2 (311-1427 second), a 600-second engine off soak, and a warm start Phase 3 (repeat of Phase 1 of the LA92).  Sampling for VOC included collection of whole air samples in canisters for analysis of speciated hydrocarbons (benzene, toluene, ethylbenzene, m-& p-,o-xylene, i.e. BTEX, styrene, n-hexane, naphthalene,1,3-butadiene, MTBE), and DNPH-coated Sep Pak cartridges sampling for carbonyl compounds (formaldehyde, acetaldehyde, acrolein).
#> 2 The test cycle consists of a cold start Phase 1 (first 310 seconds), a stabilized Phase 2 (311-1427 second), a 600-second engine off soak, and a warm start Phase 3 (repeat of Phase 1 of the LA92).  Sampling for VOC included collection of whole air samples in canisters for analysis of speciated hydrocarbons (benzene, toluene, ethylbenzene, m-& p-,o-xylene, i.e. BTEX, styrene, n-hexane, naphthalene,1,3-butadiene, MTBE), and DNPH-coated Sep Pak cartridges sampling for carbonyl compounds (formaldehyde, acetaldehyde, acrolein).
#>   NORMALIZATION_BASIS ORIGINAL_COMPOSITE STANDARD INCLUDES_INORGANIC GAS
#> 1                 TOG                  C        1                      0
#> 2                 TOG                  C        1                      0
#>   TEST_YEAR JUDGEMENT_RATING VINTAGE_RATING DATA_QUANTITY_RATING
#> 1      2010                5              5                    3
#> 2      2010                5              5                    3
#>            REGION SAMPLES LOWER_SIZE UPPER_SIZE SIBLING VERSION TOG_to_VOC
#> 1 San Antonio, TX                  0          0             4.3    1.78241
#> 2 San Antonio, TX                  0          0             4.3    1.78241
#>   TEMP_SAMPLE_C RH_SAMPLE PARTICLE_LOADING_ug_per_m3 ORGANIC_LOADING_ug_per_m3
#> 1            NA        NA                         NA                        NA
#> 2            NA        NA                         NA                        NA
#>   CATEGORY_LEVEL_1_Generation_Mechanism CATEGORY_LEVEL_2_Sector_Equipment
#> 1                            Combustion                    Mobile; Onroad
#> 2                            Combustion                    Mobile; Onroad
#>   CATEGORY_LEVEL_3_ Fuel_Product MASTER_POLLUTANT_EMISSION_RATE
#> 1              Gasoline; Ethanol                             NA
#> 2              Gasoline; Ethanol                             NA
#>   MASTER_POLL_EMISSION_RATE_UNIT ORGANIC_MATTER_to_ORGANIC_CARBON_RATIO
#> 1                                                                    NA
#> 2                                                                    NA
#>   MASS_OVERAGE_PERCENT CREATED BY CREATED Date MODIFIED BY MODIFIED DATE
#> 1                   NA                                                  
#> 2                   NA                                                  
#>   REVIEWED BY REVIEWED DATE Data_Origin
#> 1                                   EPA
#> 2                                   EPA
#>                                                   Keywords DOC_LINK Q_LINK
#> 1 Gasoline Exhaust; Tier 2 light-duty vehicle; 85% Ethanol                
#> 2 Gasoline Exhaust; Tier 2 light-duty vehicle; 85% Ethanol                
#>   WEIGHT_PERCENT INCLUDE_IN_SUM UNCERTAINTY_PERCENT UNCERTAINTY_METHOD
#> 1     0.01330965            Yes                 -99                N/A
#> 2     0.06784069            Yes                 -99                N/A
#>   ANALYTICAL_METHOD PHASE SPECIES_EMISSION_RATE SPECIES_EMISSION_RATE_UNIT
#> 1            GC-FID   GAS                    NA                           
#> 2            GC-FID   GAS                    NA                           
#>        CAS CAS no hyphen ALT_CAS SAROAD PAMS HAPS           SPECIES_NAME
#> 1  95-63-6         95636   95636           1    0 1,2,4-trimethylbenzene
#> 2 108-67-8        108678  108678  45207    1    0 1,3,5-trimethylbenzene
#>    SPEC_MW NonVOCTOG NOTE SRS ID     DSSTox_ID Molecular Formula
#> 1 120.1916         0       16261 DTXSID6021402             C9H12
#> 2 120.1916         0       25262 DTXSID6026797             C9H12
#>   OXYGEN_to_CARBON_RATIO    Smiles Notation VP_Pascal_EPI VP_Pascal_UM
#> 1                     NA CC1=CC(=C(C=C1)C)C      215.9823     479.1445
#> 2                     NA CC1=CC(=CC(=C1)C)C      267.9780     479.1445
#>   VP_Pascal_OPERA Duplicate_ID SYMBOL
#> 1        250.3854              BZ124M
#> 2        525.0247              BZ135M
#>                                                                                                                                                                                                                                                                                                   REFERENCE
#> 1 2013. 'EPAct/V2/E-89: Assessing the Effect of Five Gasoline Properties on Exhaust Emissions from Light-Duty Vehicles Certified to Tier 2 Standards Final Report on Program Design and Data Collection', US EPA, US DOE and CRC. EPA-420-R-13-004. https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100GA80.txt
#> 2 2013. 'EPAct/V2/E-89: Assessing the Effect of Five Gasoline Properties on Exhaust Emissions from Light-Duty Vehicles Certified to Tier 2 Standards Final Report on Program Design and Data Collection', US EPA, US DOE and CRC. EPA-420-R-13-004. https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100GA80.txt
#>                                                                                                                                                                                                                                                                                                                  REF_DESCRIPTION
#> 1 This program is Phase 3 of EPAct/V2/E-89, a joint testsing effort by EPA, DOE through NREL, and CRC.  It evaluates the effects of fuel property changes on exhaust emissions from Tier 2 light-duty gasoline vehicles (LDGVs) running on the following ethanol fuel blends: 0%, 10%, 15%, 20% and 85% ethanol-blended gasoline
#> 2 This program is Phase 3 of EPAct/V2/E-89, a joint testsing effort by EPA, DOE through NREL, and CRC.  It evaluates the effects of fuel property changes on exhaust emissions from Tier 2 light-duty gasoline vehicles (LDGVs) running on the following ethanol fuel blends: 0%, 10%, 15%, 20% and 85% ethanol-blended gasoline
#>                                                        LINK
#> 1 #https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100GA80.txt#
#> 2 #https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100GA80.txt#
```

## plot

``` r
library(ggplot2)
ggplot(dt[, c("WEIGHT_PERCENT", "SPECIES_NAME")], 
       aes(x = SPECIES_NAME, y = WEIGHT_PERCENT)) +
  geom_bar(stat = "identity") +
  coord_flip()
```

<img src="man/figures/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />
