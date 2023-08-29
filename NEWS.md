# Version 0.0 - Release Notes

* [0.2.3] 
    * released 2023-08-29 
    * added functions: sp_rescale_species, sp_dcast, sp_dcast_species  
    * sp_profile update: references not included by default 
    * sp_match: changed n to matches and added min.n (so more consistent 
    other args)
    * sp_species_cor: added heatmap and key list options 

* [0.2.2] 
    * released 2023-08-24 
    * now imports data.table; moving to data.table methods for speed 
    * changed news format because build_news missing previous md text 
    * added functions: sp_rescale_profile, sp_dcast_profile, sp_species_cor, 
    sp_profile_dist, sp_match_profile, sp_melt_wide
    * reduced object classes
    
* [0.2.1] 
    * released 2023-06-21 
    * updated sysdata (now using SPECIATE 5.2)
    * added sp_find_species 
    * extended sp_find_profile; now searches by species_name
    * simplified plot.respeciate

* [0.2.0] 
    * released 2021-05-26 
    * Karl Ropkins joined
    * moved your sysdata.rda to the package data folder
    * reset lazy.data to TRUE  
    * added an object class
    * added sp_find_profile (find_code but making object class)
    * added sp_profile (spec but making object class)
    * added crude print and plot methods for object classes
    * updated date and version
    * The new code is in R:speciate.0.2.r

* [0.1.0] 
    * released 2020-12-20  
    * Created respeciate
