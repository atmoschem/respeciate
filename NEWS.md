# Version 0.0 - Release Notes

* [0.2.7] 
    * released 2024-02-17
    * sp_plot_species update; species order forced, added reset.x 
    * sp_match_profile update; fit methods for pd and sid (Belis) 
    * sp_match_profile update; added sid variations 
    * pls_plot patch; forcing profile order   

* [0.2.6] 
    * released 2023-12-01
    * sp_build_rsp_x update; added further value handling 
    * summary.respeciate update; removed output print
    * pls_rebuild update; option to add species as marker  
    * now importing lattice and latticeExtra for plot paneling
    * pls_plots and plot.respeciate now using lattice/latticeExtra 
    * added sp_plot_profile and sp_profile_species 
    * reduced padding on log scale y-axis  

* [0.2.5] 
    * released 2023-10-25
    * sp_build_rsp_x update; x converted to data.frame (trouble with tibbles!)
    * pls_plot update; changed mar settings 
    * added first draft pls_test 

* [0.2.4] 
    * released 2023-10-14 
    * added functions: pls_fit_species 
    * added draft rsp_pls and rsp_x object classes 
    * added draft sp_build_rsp_x  
    * moved from importing data.table to data.table::as.data.table, etc... 

* [0.2.3] 
    * released 2023-09-26 
    * added functions: sp_rescale_species, sp_dcast, sp_dcast_species 
    sp_pad, sp_pls_profile 
    * added draft spq_ quick profile build  functions 
    * added pls_ functions for sp_pls_profile outputs
    * sp_profile update: references not included by default 
    * sp_match: changed n to matches; added min.n (so more consistent 
    with other functions); now averages x
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
