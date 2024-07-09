#####################
# to think about
#####################

# standardise

#     error messages, e.g. RSP> [function]: [issue] \n\t [fix]?

#     dropping the (re)SPECIATE shorthand for r package and SPECIATE together
#          (adding SPECIEUROPE makes this complicated)

#     made main respeciate object argument name rsp rather than x
#         that helps rsp_plot..() if it passed args to lattice
#              but not sure it really help with plot()...

#     changed the required columns when adding SPECIEUROPE
#          SPECIES_ID -> .species.id
#          SPECIES_NAME -> .species
#          PROFILE_CODE -> .profile.id
#          PROFILE_NAME -> .profile
#          WEIGHT_PERCENT -> .pc.weight


.rsp_old2ew <- function(x){
  #make previous data look like new data
  ##################################
  #.species.id
  if(!".species.id" %in% names(x)){
    if("SPECIES_ID" %in% names(x))
      x$.species.id <- x$SPECIES_ID
  }
  #.species
  if(!".species" %in% names(x)){
    if("SPECIES_NAME" %in% names(x))
      x$.species <- x$SPECIES_NAME
  }
  #.profile.id
  if(!".profile.id" %in% names(x)){
    if("PROFILE_CODE" %in% names(x))
      x$.profile.id <- x$PROFILE_CODE
  }
  #.profile
  if(!".profile" %in% names(x)){
    if("PROFILE_NAME" %in% names(x))
      x$.profile <- x$PROFILE_NAME
  }
  #.pc.weight
  if(!".pc.weight" %in% names(x)){
    if("WEIGHT_PERCENT" %in% names(x))
      x$.pc.weight <- x$WEIGHT_PERCENT
  }

  #out
  x
}


#     made ..rsp_ function common sources for merged SPECIATE + SPECIEUROPE data

#     can now combine or compare US and EU profiles with
#          rsp(), rsp_find_profile(), etc...
#               extra arg source = "us" limits you to just SPECIATE or
#                     "eu" for SPECIEUROPE

#      switched rsp_q_... functions to rsp_us_... when SPECIEUROPE added
#          reason: these function only access SPECIATE data
#                and a function like rsp_q_pm might be assumed to give user
#                all pm samples (from any source)
#          follow-on: should we have rsp_eu_... functions???

#####################
#to check
#####################

# I think all build/check issues associate with
#     [whatever]_test and its depends...
#        (not keeping unless we can get it to work better)


##############################
#current projects
##############################

# add specieurope data to package
#     DONE now TESTING ...
#     BUT also
#         think about how names changes will impact other packages???
#                  ATMOSCHEM, EMBRS, etc
#                     main one for those using respeciate is
#                               sysdata -> SPECIATE
#                  might also need to think about how they could use EU data...



###############################
# possible future projects ???
###############################

# move more to data.table
#     like to move more of code to use data.table objects/code
#         (rather than standard data.frame equivalents)
#         lot faster...
#         BUT needs time to think through, do, and test...
#      like to have a quicker library search rsp_match_profile
#      like to know how to quickly strip out columns which are all NAs

# extra functions
#      rsp_get... (pull?) [s'cut for those too lazy to learn .profile, etc...]
#      rsp_repair.. like pad but a reset to remove column and replace from source.
#      rsp_cbind/merge and rsp_rbind like functions (not sure if we need??)
#      rsp_barebones - (skeleton) strip to minimal columns (not needed...?)
#      rsp_pad_profile and rsp_pad_species s'cuts for rsp_pad (not needed..?)

# switch between respeciate and SPECIATE-like or SPECIEUROPE-like data structures
#      I have (unexported) .rsp_eu2us from early work merging US and EU archives
#           that converted EU data to US-like column naming
#           could think about a converter rsp -> SPECIATE

# improve rsp_find_species
#      current search .species for matches
#            US equivalent SPECIES_NAME has multiple names
#               could also search that but be an issue if exact requested...
#                   (partial = FALSE)
#       NB: not a fan of partial as a find formal (but in both rsp_find...
#            functions...

# rsp_match_profile improvement
#      like to make it quicker, (also under more data.table above)
#      like to make the function output multiple agreement measures
#            currently just does one at time: pd or sid
#            like to have a table of number (useable x/y pairs), pd and sid
#       could then plot, e.g. x=pd, y=sid, pt size = number of pairs,
#                and maybe colour-code by a sample descriptor...

# print.respeciate improvement
#      does not know if .species is missings and
#         calls any number of columns !0 if .species.id there but not .species

# improve the ..rsp_ merged (SPECIATE + SPECIEUROPE) data sources
#     the build involves going through the data sets and manually checking them...
#         and hardcoded adjustment for any misalignments...
#               e.g. .species.id/.species match US/EU until entry 2786...
#               see function scripts for latest versions and associated
#     in future like to automate as much as possible of the builds these

#     (related) the merge maps SPECIATE species metadata onto SPECIEUROPE species
#         but it is not always straightforward
#              the EU Profile.Type different to US PROFILE_TYPE...
#                    .profile.type could be more consistent
#              no US profile meta column keywords equivalent in EU/SPECIEUROPE...
#                    (current copying EU Profile for using in rsp_find_profile

# boxplot output option for rsp_plot_species
#     maybe x = species, y = .value, no group default...
#     maybe make this plot.type=1/default...
#           logic: current plot gives xyplot(type="l") is messy if there are
#           lots of species...

# better handling for the rsp ->(dcast)-> rsp_p/sw ->(melt)-> rsp (and rsp_x)
#     1. look at general handling for rsp and rsp_x
#            wondering if rsp_x objects should have a sample number id rather
#                 than .profile and .profile.id??
#                 (they are x matrices rather than m...)
#     2. look at melt padding for rsp_x or when rsp meta data is missing...
#            not sure how common the last case is ???
#                 wondering if unknown species need an auto labelling option...
#                       maybe compare with rsp_build_x
#     3. look at melt padding when some or all of .value or .pc.weight missing...
#            might need to be different for rsp and rsp_x ???
#            might also need a repair function
#     4. should we include rsp_pad_profile and rsp_pad_species as shortcuts ??
#     5. wondering if pad could have an extra argument
#            ref (a reference to pull meta data from...)
#               that could be used to add in any columns
#               e.g. this could be the pre-dcast data and passed back via melt

#  new function rsp_build_sim_x
#     make a simulation data set for example pls models
#     fun(rsp, n = 1, err=0, ...)
#          rsp is profile to build sim with
#          n is the n matrix (profile contributions) [check PLS docs]
#                default 1 would be rep'ed by the n.profile in rsp
#                but could be a data.frame or matrix, etc
#                     so maybe allow arguments like matrix by.row [check]
#                               so a vector of numbers coudl be turned into
#                               a meaningful n data from...
#          err is proportion err to add (default n = none)
#                could this be like n, so a matrix of errs could be added
#                also should err be an absolute to a multiplier for a
#                random error term...
#          would use with rsp_pls_profile as a quick example for documentation ???

#  new function rsp_mmpls_profile
#       like rsp_pls_profile but for molecular marker
#                before starting this have a think because there might be a
#                shortcut via


##############################
#setup code, misc code,
#testing code, etc
##############################

#currently no hooks, etc...

#globals
utils::globalVariables(c("SPECIATE", "SPECIEUROPE", ".SD", "ans", "control",
                         "PROFILE_TYPE", "SPECIES_ID", "WEIGHT_PERCENT",
                         ".profile.id", ".profile", "PROFILE_TYPE",
                         ".species.id", ".species", "SPEC_MW", ".profile.type",
                         ".pc.weight", ".", ".value", ".load", "Id"))

#to think about...

# all @import here
#    moved to data.table::as.data.table in code...
# #' @import data.table

#   data.table used by:
#         rsp_test_profile,
#         rsp_dcast..., rsp_melt...
#         rsp_cor_species
#         rsp_distance_profile
#         and others???
#               need to identify them

#' @importFrom lattice xyplot barchart panel.grid panel.xyplot panel.barchart
#' trellis.par.get simpleTheme yscale.components.default prepanel.default.xyplot
#' panel.abline
#' @importFrom latticeExtra doubleYScale panel.ablineq
#' @importFrom data.table ":="
#' @importFrom stats sd cophenetic cor cutree dist hclust heatmap AIC
#' as.formula coefficients formula lm nls nls.control predict update na.omit
#' @importFrom utils modifyList head packageVersion
#' @importFrom graphics axis barplot par legend lines rect text abline
#' grid mtext plot.new plot.window points polygon title
#' @importFrom grDevices cm.colors colorRampPalette as.graphicsAnnot
#' dev.flush dev.hold heat.colors rainbow

# notes

#might be able to drop legend?
#   check plot.respeciate



##################################
##################################
## common unexported 1
##################################
##################################

# ..rsp functions

############################
# unexported ..rsp scripts
############################

# these are unexported functions that extract data from the archives

# the reason these are unexported is because they account for features
# of the archives that prevent they merging completely.

# NOTE
###################
# if you want to merge data from US and EU archive, we recommend that
# you use these (or better yet the exported functions build using them)
# rather than go to SPECIATE and SPECIEUROPE directly

# there may be a small time/memory cost in doing that BUT it means
# all merged US+EU data will be consistently merged ...

#####################
# to think about
#####################

###############################
# possible future projects ???
###############################

# automate the species handling in ..rsp_species and ..rsp_weight functions
###############################
#    currently relying on developer to manual correct one when other is
#          updated...
#    maybe move common code to a separate ..rsp_ function...


################################
##############################
## common unexported
##############################
################################

# suggesting standardizing naming for these:: ..rsp_[function_description]

##################################
##  testing ..rsp as data mergers
##################################

# related issue some Specie are not the same for
#            eu:Specie.Id versus us:.species.id...

..rsp_species_meta <-function(){

  ##############################
  #speciate species.us
  ##############################
  species.us <- SPECIATE$SPECIES_PROPERTIES

  #current local tidies/corrections
  ########################
  species.us$CAS[species.us$CAS=="N/A"] <- NA
  #NOTE
  ########################
  #Heneicosanoic acid duplicated
  #but also lots of others - see also Duplicate_ID
  #    tried but discontinued following...
  ##species.us$.species.id[species.us$.species=="Heneicosanoic acid -duplicate"] <-
  ##   species.us$.species.id[species.us$.species=="Heneicosanoic acid -duplicate"] + 0.2

  #add common term for merge...
  #################################
  species.us$.species.id <- as.character(species.us$SPECIES_ID)
  #see merging step

  ############################
  #specieurope species.eu
  ############################
  species.eu <- SPECIEUROPE$source

  #current local tidies/corrections
  ########################
  #NOTE: same need to be done in ..rsp_weights_meta
  #      but point at which it is done is different...
  #      so variable naming will be different...
  species.eu <- species.eu[c("Specie.Id", "Specie", "Analythical.Method",
                             "Uncertainty.Method", "Sampling.Method", "Cas",
                             "Symbol")]
  species.eu <- species.eu[!duplicated(species.eu$Specie.Id),]
  species.eu$.species.id <- as.numeric(species.eu$Specie.Id)
  #ids above 2786 different for Us and EU data sets...
  #   so adding 0.5 to make species unqiue...
  species.eu$.species.id[species.eu$.species.id>2786] <-
    species.eu$.species.id[species.eu$.species.id>2786] + 0.5
  # most SPECIE/.species for these do not seem to be in both BUT..
  # aluminum oxide assigned in both with different ids..
  species.eu$.species.id[tolower(species.eu$Specie)=="aluminum oxide"] <- 2848

  #add common term for merge...
  #################################
  species.eu$.species.id <- as.character(species.eu$.species.id)

  #merged out!
  #########################
  #NOTE: this makes a large data set
  #    all columns from SPECIATE, SPECIEUROPE and what I add
  #         could simplify later but for now this retain all info
  #         for error/issue checking
  out <- as.data.frame(data.table::merge.data.table(
    data.table::as.data.table(species.us),
    data.table::as.data.table(species.eu),
    by = ".species.id", all.y=TRUE, all.x=TRUE,
    allow.cartesian=TRUE), suffixes = c(".us", ".eu"))
  #make common term .species (the species name)
  ########################################
  out$.species <- ifelse(is.na(out$SPECIES_NAME),
                         out$Specie,
                         .rsp_tidy_species_name(out$SPECIES_NAME))
  #current my simplified version of US name or Specie name
  #    if US name not there...
  out
}

#a <- ..rsp_species_meta()
#View(subset(a, !is.na(Specie))[,c(".species.id", "Specie", ".species")])


..rsp_profile_meta <- function(){

  #################
  #us profiles
  ################
  profiles.us <- SPECIATE$PROFILES
  profiles.us$.profile.id <- paste("US:", profiles.us$PROFILE_CODE, sep="")
  profiles.us$.profile <- profiles.us$PROFILE_NAME
  profiles.us$.profile.type <- paste("US:", profiles.us$PROFILE_TYPE, sep="")

  ######################
  #eu profile meta
  ######################
  profiles.eu <- SPECIEUROPE$source
  profiles.eu <- profiles.eu[!duplicated(profiles.eu$Id),]
  profiles.eu <- profiles.eu[c("Id", "Name", "Original.Name", "Country",
                               "Place", "Test.Year", "Profile.Type",
                               "Particle.Size", "Latitude", "Longitude")]
  profiles.eu$Keywords <- profiles.eu$Name
  profiles.eu$.profile.id <- paste("EU:", profiles.eu$Id, sep="")
  profiles.eu$.profile <- profiles.eu$Name
#####################
#like to add (or replace this with) the pm type in to this?
#####################
  profiles.eu$.profile.type <- paste("EU:", profiles.eu$Profile.Type, sep="")

  #######################
  #rbindlist out!
  #######################
  # stacking these because eu and us profiles are different...
  as.data.frame(data.table::rbindlist(list(data.table::as.data.table(profiles.us),
                                           data.table::as.data.table(profiles.eu)),
                                      fill=TRUE))
}


#issue some negatives in specieurope relative.amounts!!!

..rsp_weights_meta <- function(){

  ######################
  # us weights
  ######################
  pc.wts.us <- SPECIATE$SPECIES[c("PROFILE_CODE", "SPECIES_ID", "WEIGHT_PERCENT")]
  names(pc.wts.us) <- c(".profile.id", ".species.id", ".pc.weight")
  pc.wts.us$.profile.id <- paste("US:", pc.wts.us$.profile.id, sep="")
  pc.wts.us$.species.id <- as.character(pc.wts.us$.species.id)

  ##################
  #eu weights
  ##################
  pc.wts.eu <- SPECIEUROPE$source

  #corrections
  ######################
  #from ..rsp_species_id
  #    need to transpose any changes
  pc.wts.eu$Specie.Id[pc.wts.eu$Specie.Id>2786] <-
    pc.wts.eu$Specie.Id[pc.wts.eu$Specie.Id>2786] + 0.5
  pc.wts.eu$Specie.Id[tolower(pc.wts.eu$Specie)=="aluminum oxide"] <- 2848

  #other changes
  #######################
  pc.wts.eu <- pc.wts.eu[c("Id", "Specie.Id", "Relative.Mass")]
  names(pc.wts.eu) <- c(".profile.id", ".species.id", ".pc.weight")
  pc.wts.eu$.profile.id <- paste("EU:", pc.wts.eu$.profile.id, sep="")
  pc.wts.eu$.pc.weight[tolower(pc.wts.eu$.pc.weight)=="not detected"] <- NA
  pc.wts.eu$.pc.weight <- as.numeric(pc.wts.eu$.pc.weight) * 100
  pc.wts.eu$.species.id <- as.character(pc.wts.eu$.species.id)

  #rbindlist out!
  ################
  #  stacking....
  as.data.frame(data.table::rbindlist(
    list(data.table::as.data.table(pc.wts.us),
         data.table::as.data.table(pc.wts.eu)),
    fill=TRUE))
}


..rsp_references_meta <- function(){

  #####################
  #us references
  #####################
  refs.us <- as.data.frame(data.table::merge.data.table(
    data.table::as.data.table(SPECIATE$PROFILE_REFERENCE),
    data.table::as.data.table(SPECIATE$REFERENCES),
    by = "REF_Code", all.y=TRUE, all.x=TRUE,
    allow.cartesian=TRUE), suffixes = c(".us", ".eu"))
  #"REF_Code"        "PROFILE_CODE"    "REFERENCE"       "REF_DESCRIPTION" "LINK"
  names(refs.us)[names(refs.us)=="PROFILE_CODE"] <- ".profile.id"
  refs.us$.profile.id <- paste("US:", refs.us$.profile.id)

  ######################
  #eu references
  ######################
  refs.eu <- SPECIEUROPE$source[c("Id", "Reference")]
  refs.eu <- refs.eu[!duplicated(paste(refs.eu$Id, refs.eu$Reference)),]
  names(refs.eu) <- c(".profile.id", "REFERENCE")
  refs.eu$.profile.id <- paste("EU:", refs.eu$.profile.id)

  ######################
  #rbindlist out!
  ######################
  #stacking
  as.data.frame(data.table::rbindlist(list(data.table::as.data.table(refs.us),
                                           data.table::as.data.table(refs.eu)),
                                      fill=TRUE))
}















################################
##############################
## common unexported 2
##############################
################################

# suggesting standardizing naming .rsp_[function_description]


#.rsp_
#################################
#    tidy for rsp_x data setup

#basic build needs
#   profile_name and profile_code
#   species_name and species_id
#   weight_percent (and possibly .value)

#notes
#   think this can go because we now have rsp_build_x???
#       plus I don't think anyone but me (kr) has used it...

##.rsp_ <- function(x){
##  .o <- rsp_profile(x)
##  .o$.profile <- paste("test", .o$.profile, sep=">")
##  .o$.profile.id <- "test"
##  .o
##}


###############################
# think about ????
###############################

# combining some of these .rsp_ functions
#      as a new .rsp_tidy or .rsp_barebones
#          to simplify the data sent and work with in functions
#      then use a rsp_pad or rsp_meta/repair to add meta data when wanted???



#.rsp_eu2us(rsp)
##############################
# convert specieurope data to (re)speciate

.rsp_eu2us <- function(x){

  #note:
  #only meant for SPECIEUROPE and subsets of it...

  #make it respeciate-like
  #note:
  #   the if... names is redundant if x is from SPECIEUROPE
  if("Id" %in% names(x)){
    names(x)[names(x)=="Id"] <- ".profile.id"
  }
  if("Name" %in% names(x)){
      names(x)[names(x)=="Name"] <- ".profile"
  }
  if("Specie" %in% names(x)){
    names(x)[names(x)=="Specie"] <- ".species"
  }
  if("Specie.Id" %in% names(x)){
    names(x)[names(x)=="Specie.Id"] <- ".species.id"
    x$.species.id <- as.character(x$.species.id)
  }
  if("Relative.Mass" %in% names(x)){
    names(x)[names(x)=="Relative.Mass"] <- ".pc.weight"
  }
  if("Particle.Size" %in% names(x)){
    names(x)[names(x)=="Particle.Size"] <- "PROFILE_TYPE"
  }
  ##########################
  # note
  ##########################
  # this might generate a warning
  #      some SPECIEUROPE$source$Relative.Mass is a character vector and
  #      some are something like 'not detected'...
  # also wondering if these should in above if's
  #      if someone passed a us rsp to this it could corrupt structure...
  if(nrow(x)>0){
    ############################################
    # to think about
    ############################################
    # do we want a 'US:' prefix for profiles from SPECIATE ???
    #      like this ???
    #      (might need to think about handling...)
    #      [user could go rsp("eu:1") instead of rsp(1, source="us")]
    x$.profile.id <- paste("EU:", x$.profile.id, sep="")
    x$.pc.weight[x$.pc.weight=="not detected"] <- NA
    x$.pc.weight <- as.numeric(x$.pc.weight) * 100
    x$.value <- x$.pc.weight
  }
  class(x) <- class(x)[class(x)!="rsp_eu"]
  x
}





# .rsp_plot_output(p, p1.ls)
###################################
# standard output from plot

# this is messy
#   maybe rethink...
#     notes:
#     need default plot, then send data invisibly
#     other options just data, just plot, list...
#

# this is used by all rsp_plot... and pls_plot... functions
#   for output standardisation
#      so all will need updating and checking if this is changed
#      also pls_plot_profile uses rsp_plot_profile to make first layer
#            p1. handling in code body should also be checked...

.rsp_plot_output <- function(da, li, plt, output){
  output <- tolower(paste(output, sep=",", collapse=","))
  if(output=="default" | output =="plot,data"){
    plot(plt)
    return(invisible(da))
  }
  if(output=="plot"){
    return(plt)
  }
  if(output=="data"){
    return(da)
  }
  if(output=="list"){
    return(li)
  }
}



#.rsp_split_profile
#######################################
#split respeciate by profile

#currently not exported
#quick code assumed CODE is unique to profile

#need to test this

#not sure we are using this any more ???
#    i think rsp_test, then rsp_test.2 replaced
#    and code in plot.respeciate.old ???

.rsp_split_profile <- function(x){
  ref <- unique(x$.profile.id)
  lapply(ref, function(y) x[x$.profile.id==y,])
}






#.rsp_build_respeciate....
#################################
#   class builds

# dropped
#     rsp_build_respeciate.spcs
#     rsp_build_respeciate.ref

# hoping to drop last one...
#     as.respeciate to supersede

#rsp_build_respeciate.spcs <-
#  function(x, ...){
#build
#add .value
#    x <- rsp_tidy_profile(x)
#    class(x) <- c("respeciate.spcs", "data.frame")
#    x
#  }

#rsp_build_respeciate.ref <-
#  function(x, ...){
#build
#    class(x) <- c("respeciate.ref", "data.frame")
#    x
#  }

.rsp_build_respeciate <-
  function(x, ...){
    x <- as.data.frame(x)
    if(".pc.weight" %in% names(x)) {
      x$.value <- x$.pc.weight
    }
    class(x) <- c("respeciate", class(x))
    x
  }


#.rsp_plot_fix
#########################
# general tidy function for data before plotting
#    merges duplicate species in profiles
#    makes profile names unique if duplicated
#    tidies species names for use in labelling
#         warns about changes

#used by
###################
#plot.respeciate
#rsp_plot_profile

#uses by
####################
#.rsp_tidy_profile
#.rsp_test_respeciate
#.rsp_test_profile

.rsp_plot_fix <- function(x, silent = FALSE, ...){

  .x.args <- list(...)
  x <- .rsp_tidy_profile(x)

  ##test object type
  test <- .rsp_test_respeciate(x, level=2, silent=TRUE)
  if(test != "respeciate"){
    if(test %in% c("respeciate.profile.ref", "respeciate.species.ref")){
      stop("RSP> No plot method for respeciate.reference files.",
           call. = FALSE)
    } else {
      stop("RSP> suspect respeciate object!",
           call. = FALSE)
    }
    #don't stop - respeciate profile
  }
  #check for duplicates
  x <- .rsp_test_profile(x)


  if(any(x$.n>1) & !silent){
    warning(paste("RSP> found duplicate species in profiles (merged and averaged...)",
                  sep=""), call.=FALSE)
  }
  #shorten names for plotting
  x$.species <- .rsp_tidy_species_name(x$.species)

  ####################################
  #issue profile names are not always unique
  ####################################
  test <- x
  test$.species.id <- ".default"
  test <- .rsp_test_profile(test)

  ###################
  #rep_test
  #can now replace this with data.table version
  #BUT check naming conventions for .n
  ###################

  #does this need a warning?
  if(length(unique(test$.profile))<nrow(test)){
    if(!silent){
      warning(paste("RSP> found profiles with common names (making unique...)",
                    sep=""), call. = FALSE)
    }
    test$.profile <- make.unique(test$.profile)
    x <- x[names(x) != ".profile"]
    x <- merge(x, test[c(".profile", ".profile.id")], by=".profile.id")
    ############################
    #why not just
    #x$.profile <- make.unique(x$.profile)
    ############################
  }

  #out
  x
}




############################
#unexported function to
#test is x respeciate

##aa <- sp_profile(sp_find_profile("ae8", by="profile_type"))
##bb <- sp_find_profile("ethanol")
##bb <- sp_find_species("butane")

##rsp_test_respeciate(aa, level=1)
## level 1 TRUE for respeciate only (needs to look like respeciate)
## level likewise but TRUE for respeciate, respeciate.ref and respeciate.spcs

##might need to rethink this if I want to use it as is.respeciate method
##  it only allows 1 argument.

## could also test for .value

#used by
###############################
#.rsp_plot_fix


.rsp_test_respeciate <- function(x, level = 1,
                                silent = FALSE){
  test <- class(x)
  out <- "bad"
  test <- test[1]=="respeciate"
  #order below matters
  #maybe tidy??
  if(all(c(".species", ".species.id") %in% names(x))){
    out <- "respeciate.species.ref"
  }
  if(all(c(".profile", ".profile.id") %in% names(x))){
    out <- "respeciate.profile.ref"
  }
  if(all(c(".species", ".species.id", ".profile", ".profile.id",
           ".pc.weight") %in% names(x))){
    out <- "respeciate"
  }
  if(!silent){
    if(test && out=="bad"){
      warning("suspect respeciate object!")
    }
    if(nrow(x)==0){
      warning("empty respeciate object!")
    }
  }
  if(level==1){
    return(test)
  }
  return(out)
}

#######################
#tidy profile

#now using a .value column as a local version of .pc.weight...
#then .pc.weight remains as EPA made it even if we rescale...

## testing this idea at the moment
##     make .value using rsp_tidy_profile
##     enabled in plot.respeciate, sp_profile_rescale, sp_profile_dcast
##                rsp_test_profile

#used by
###############################
#.rsp_plot_fix
#rsp_pad

.rsp_tidy_profile <- function(x){
  #.value is local version of weight
  if(!".value" %in% names(x)){
    x$.value <- x$.pc.weight
  }
  x
}






###########################
#tidy species names

#currently not exported
#quick code to tidy species names


#note: not fully tested

#thinking about

#    option foreshorten any names longer than [n] characters???
#    similar function to tidy profile names

#used by
###############################
#plot.respeciate

.rsp_tidy_species_name <- function(x){

  #attempts shorten names by remove other versions
  #names seem to be in format a (or b || c)
  #where (guessing) a is main name and
  #         b and c are alternative terms.

  #not fully tested,
  #   might still be more cases this dies on

  #gsub("[(].*","", x) failed if name a includes brackets
  #example:#"(2-methylpropyl)benzene (or isobutylbenzene)"

  #sub("[(][^(]or+$", "", x) failed if b or c includes brackets
  #also left space at end so needed sub("^\\s+|\\s+$", "", x)

  #sometimes it is "( or "
  x <- gsub(" [(] or ", " (or ", x)
  #next removes from last "(or" onwards
  x <- gsub("[(]or .*","", x)
  sub("^\\s+|\\s+$", "", x)
}


################################
#rsp_test_profile

#file:///C:/Users/trakradmin/Downloads/datatable.pdf
##rsp_test_profile(aa)

#used by
###############################
#.rsp_plot_fix

.rsp_test_profile <- function(x){

  #set up .value if not there
  x <- .rsp_tidy_profile(x)

  #######################################
  #track and return original class?
  #      testing
  #######################################
  tmp <- class(x)
  xx <- data.table::as.data.table(x)
  out <- xx[,
            .(.profile = .profile[1],
              .species = .species[1],
  #######################
  #test
  ########################
              #SPEC_MW = SPEC_MW[1],
  ########################
              .total = sum(.value, na.rm = TRUE),
              .value = mean(.value, na.rm = TRUE),
              .n = length(.value[!is.na(.value)]),
              .sd = sd(.value, na.rm = TRUE)
            ),
            by=.(.profile.id, .species.id)]

  #might regret .value
  out$.pc.weight <- out$.value
  #note: I *think* this is fine as long as x is never permanently replaced
  #      with the rsp_test_profile without a warning
  #           might be cases where we want to change .pc.weight

  #           change case making an average profile

  #output
  #might regret this...
  #   but would like to leave user free to use e.g. dplyr rather than
  #       data.table if that is their preference
  out <- as.data.frame(out)
  class(out) <- tmp
  out
}

# now replaced with rsp_test_profile

#rsp_test (old version)

#base case idiot test

#rsp_test <- function(x){
#  .prf <- unique(x$.profile.id)
#  ans <- lapply(.prf, function(y){
#    temp <- subset(x, .profile.id==y)
#    .spc <- unique(temp$.species.id)
#    ans <- lapply(.spc, function(z){
#      temp2 <- subset(temp, .species.id==z)
#      data.frame(.profile.id = y,
#                 .profile = temp2$.profile[1],
#                 .species.id = z,
#                 .species = temp2$.species[1],
#                 COUNT = length(temp2$.pc.weight[!is.na(temp2$.pc.weight)]),
#                 TOTAL = sum(temp2$.pc.weight[!is.na(temp2$.pc.weight)]),
#                 SPEC_MW = temp2$SPEC_MW[1],
#                 .pc.weight=mean(temp2$.pc.weight, na.rm=TRUE))
#    })
#    do.call(rbind, ans)
#  })
#  do.call(rbind, ans)
#}

#require(dplyr)
#test1 <- function(x){
#  x %>%
#    group_by(.profile.id, .species.id) %>%
#    summarise(.profile = .profile[1],
#              .species = .species[1],
#              SPEC_MW = SPEC_MW[1],
#              total = sum(.pc.weight, na.rm=T),
#              mean = mean(.pc.weight, na.rm=T),
#             sd = sd(.pc.weight, na.rm=T),
#             n = length(.pc.weight[!is.na(.pc.weight)]))
#}
#aa <- sp_profile(sp_find_profile("ae6", by="profile_type")$.profile.id)
#require(data.table)
#test2 <- function(x){
#  #######################################
#  #could track and return original class?
#  #######################################
#  xx <- as.data.table(x)
#  out <- xx[,
#            .(.profile = .profile[1],
#              .species = .species[1],
#              SPEC_MW = SPEC_MW[1],
#              .total = sum(.pc.weight, na.rm = TRUE),
#              .pc.weight = mean(.pc.weight, na.rm = TRUE),
#              .n = length(.pc.weight[!is.na(.pc.weight)]),
#              .sd = sd(.pc.weight, na.rm = TRUE)
#            ),
#            by=.(.profile.id, .species.id)]
#  #output
#  #currently data.frame
#  #could be respeciate
#  as.data.frame(out)
#}

####################################
#.rsp_col_key
####################################

#color key for correlation matrices

#used by:
##################################
#    rsp_cor_species

#started with:
#https://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette

#removed dev.new

#see also regarding adding to existing plot
#https://www.statmethods.net/advgraphs/layout.html

# One figure in row 1 and two figures in row 2
# row 1 is 1/3 the height of row 2
# column 2 is 1/4 the width of the column 1
##attach(mtcars)
##layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
##       widths=c(3,1), heights=c(1,2))
##hist(wt)
##hist(mpg)
##hist(disp)

# Add boxplots to a scatterplot
##par(fig=c(0,0.8,0,0.8), new=TRUE)
##plot(mtcars$wt, mtcars$mpg, xlab="Car Weight",
##     ylab="Miles Per Gallon")
##par(fig=c(0,0.8,0.55,1), new=TRUE)
##boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
##par(fig=c(0.65,1,0,0.8),new=TRUE)
##boxplot(mtcars$mpg, axes=FALSE)
##mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)

# Function to plot color bar

#currently not exporting

#notes
#change min and max or .min and .max
#move title into plot?
#add key.style?
#add key.position?
#how to set plot range when na is included verus excluded
#    needs to be a function of min/max range not 0.5, 1.5, etc
#    maybe 0.25 ish??? maybe tick or half tick range????

#key is the data to be color-scaled or its range
#     currently just used for range

#cols is col.palette to apply to key range
#   the col ramp is messy... seems to need border
#        that could be issue if transparency using...

#x, y are key (proportional, 0 to 1) positions

#na.col color of na's
#na.cex scaling for na box width
#    do we need any other NA controls??
#          e.g. na.border if needed to be different to main border???

#axes to do
#    not using at moment
#        but would separate axes and border colours
#             not sure it is wanted/needed...

#ticks, nticks axes tick positions and number of ticks, respectively
#     think about these

#title still work in progress
#     not nice version on type=1


#have type = 1/2 for horizontal/vertical
#   think about types 3 and 4
#        horizontal with text at top
#        vertical with text on other side

#think about exporting this once sorted???
#   or passing to another package/better home
#       (not sure how that fits with package remit)


.rsp_col_key <- function(key, cols, x, y = NULL,
                         ticks, nticks,
                         na.col = "grey", na.cex = 0.25,
                         title = "", axes, bg, border,
                         type = 2,
                         ...){

  #setup
  op <- par(no.readonly = TRUE)

  if(missing(x)){
    #currently just doing this option
    #like key.pos "top-left", key.style = 1 (horizontal, annotation below)
    x <- 0.1
  }
  if(is.null(y)){
    y <- 0.9
  }
  .min <- min(key, na.rm=TRUE)
  .max <- max(key, na.rm=TRUE)
  if(missing(ticks)){
    ticks <- pretty(c(.min, .max), 3)
  }
  if(missing(nticks)){
    nticks <- length(ticks)
  }
  .na.width <- na.cex * (.max-.min)
  if(missing(bg)){
    bg <- "white"
  }
  if(missing(border)){
    border <- "black"
  }
  scale <- (length(cols)-1)/(.max-.min)

  #print(.max-.min)
  #print(.na.width)
  #print(.min)
  #print(.max)

  #key.style 1
  if(type==1){
    #horizontal, header before, annotation after
    #margins
    .mai <- c(0.1,0.1,0.1,0.1)
    if(title ==""){
      #no title
      .fig <- c(x-0.1, x+0.1, y-0.04, y+0.1)
      .wdt <- 12
    } else {
      #title
      .fig <- c(x-0.1, x+0.1, y-0.12, y+0.1)
      .wdt <- 15
    }

    if(is.na(na.col)){
      .brd <- c(.na.width, .na.width)
    } else {
      .brd <- c(.na.width, .na.width*2)
    }

    #position col key
    par(fig = .fig, mai = .mai, new=TRUE)

    #plot col key

    #region
    plot(c(.min -.brd[2], .max+(.brd[1]*0.5)), c(-1, .wdt),
         type='n', bty='n', xaxt='n', xlab='',
         yaxt='n', ylab='', main="", font.main = 1)
    #bg + border
    rect(.min -.brd[2], -1, .max+(.brd[1]*0.5),  .wdt,
         col=bg, border=border)
    #title
    if(title !=""){
      text(.min+((.max-.min)/2)+(.na.width*0.75), 13, labels=title, col="black", cex=0.75)
    }
    #col scale
    for (i in 1:(length(cols)-0)) {
      x <- (i-1)/scale + .min
      rect(x,5,x+1/scale,10,col=cols[i], border=NA)
    }
    #axes
    lines(c(.min, .max), c(5,5), col="black")
    for (i in ticks) {
      lines(c(i,i), c(5,4),col="black")
    }
    #axes annotation
    text(ticks, rep(2, length(ticks)), labels=ticks,
         cex=0.75, adj=0.5)
    #na block
    if(!is.na(na.col)){
      rect(.min-(.na.width*0.5), 5,.min-(.na.width*1.5), 10,  col=na.col, border="black")
      text(.min-.na.width, 2, labels="NA", col="black", cex=0.75)
    }

  }

  if(type==2){
    #horizontal, header before, annotation after
    #margins
    .mai <- c(0.1,0.1,0.1,0.1)
    if(title ==""){
      #no title
      .fig <- c(x-0.05, x+0.05, y-0.2, y+0.1)
      .wdt <- 12
    } else {
      #title
      .fig <- c(x-0.05, x+0.05, y-0.2, y+0.1)
      .wdt <- 15
    }

    if(is.na(na.col)){
      .brd <- c(.na.width, .na.width)
    } else {
      .brd <- c(.na.width, .na.width*2)
    }

    #position col key
    par(fig = .fig, mai = .mai, new=TRUE)

    #plot col key

    #region
    plot(c(-1, .wdt), c(.min-.brd[2], .max+(.brd[1]*0.5)),
         type='n', bty='n', xaxt='n', xlab='',
         yaxt='n', ylab='', main="", font.main = 1)
    #bg + border
    rect(-1, .min-.brd[2], .wdt, .max+(.brd[1]*0.5),
         col=bg, border=border)
    #title
    if(title !=""){
      text(.min+((.max-.min)/2)+(.na.width*0.75), 13, labels=title,
           col="black", cex=0.75)
    }

    #for (i in 1:(length(lut)-1)) {
    #  y = (i-1)/scale + min
    #  rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    #}

    #col scale
    ####################
    #note
    ####################
    #this needs work because rect needs colored border
    #which kills transparent ranges...
    #does that matter
    for (i in 1:(length(cols))) {
      y <- (i-1)/scale + .min
      rect(5,y-(1/scale),10,y,col=cols[i], border=cols[i])
    }
    #axes
    lines(c(5,5), c(.min, .max), col="black")
    for (i in ticks) {
      lines(c(5,4), c(i,i), col="black")
    }
    #axes annotation
    text(rep(2, length(ticks)), ticks, labels=ticks,
         cex=0.75, adj=0.5)
    #na block
    if(!is.na(na.col)){
      rect(5, .min-(.na.width*0.5), 10, .min-(.na.width*1.5), col=na.col, border="black")
      text(2, .min-.na.width, labels="NA", col="black", cex=0.75)
    }

  }

  par(op)
}

#plot(iris$Sepal.Length, iris$Sepal.Width)
#rsp_col_key(c(1,-1), colorRampPalette(c("light green", "yellow", "orange", "red"))(100), title="testing")




################
################
## unexported
## from pls.plot...
################
################

# profile code order
# get profile order in case you need it latter...

.rsp_profile_code_order <- function(data){
  .tmp <-  data.table::as.data.table(data)[, .(ans=length(unique(.profile.id))),by=".species"]
  .tmp <- subset(.tmp, ans == max(.tmp$ans, na.rm=TRUE))$.species
  .tmp <- subset(data, .species %in% .tmp)
  sort(unique(.tmp$.profile.id))
}


#log axis hander
#based on lattice text book method

#issues??
#   could be problem with y padding when log=T and .value range is wide...

.rsp_yscale.component.log10 <- function(lim, ...) {
  ans <- yscale.components.default(lim = lim, ...)
  tick.at <- pretty(lim)
  tick.at <- tick.at[tick.at == floor(tick.at)]
  tick.at <- tick.at[tick.at < max(lim, na.rm=TRUE) & tick.at > min(lim, na.rm=TRUE)]
  ans$left$ticks$at <- tick.at
  ans$left$labels$at <- tick.at
  ans$left$labels$labels <- c(format(10^(tick.at),
                                     drop0trailing = TRUE,
                                     scientific = FALSE))
  #print(ans$left$labels$labels)
  #######################
  #need to sort of right labeling
  #   dropped for now...
  #ans$right <- ans$left
  ans
}


#lattice panel pal
#based on panel handler in loa

.rsp_panelPal <- function(.name, .ls, .panel, ...){
  .x.args <- list(...)
  if(!.name %in% names(.x.args) || !is.logical(.x.args[[.name]]) ||
     .x.args[[.name]]){
    .name2 <- paste("^", .name, "[.]", sep="")
    if(.name %in% names(.x.args) && is.list(.x.args[[.name]])){
      .tmp <- .x.args[[.name]]
      if(length(.tmp)>0){
        names(.tmp) <- paste(.name, names(.tmp), sep=".")
        .x.args <- modifyList(.tmp, .x.args)
      }
    }
    .x.args <- .x.args[grepl(.name2, names(.x.args))]
    if(length(.x.args)>0){
      names(.x.args) <- gsub(.name2, "", names(.x.args))
      .ls <- modifyList(.ls, .x.args)
    }
    do.call(.panel, .ls)
  }
}



# could move this into the function...

.rsp_panel.pie <-
  function (x, y=NULL, groups=NULL, subscripts, totals=NULL,
            labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45,
            col = NULL, border = 1, lty = NULL, main = NULL, ...)
  {

    #this is graphics::pie with a couple of modifications...
    #many thanks to...
    #R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation
    #for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.

    #if (!is.numeric(x) || any(is.na(x) | x < 0))
    #    stop("'x' values must be positive.")

    #########################
    #measurement totals
    .y <- totals[subscripts]
    ref <- sapply(unique(groups), function(g){
      sum(.y[groups==g], na.rm=TRUE)
    })
    .total <- mean(ref, na.rm=TRUE)

    ##########################
    #profile contributions to model
    # as percentage of measurements
    ans <- sapply(unique(groups), function(g){
      sum(y[groups==g], na.rm=TRUE)
    })
    ans <- (ans / .total) * 100

    #####################
    #cheat because following comes from
    #pie function in base r...
    x <- ans

    if (is.null(labels))
      labels <- as.character(unique(groups))
    else labels <- as.graphicsAnnot(labels)
    labels = paste(labels, " (",
                   round(ans, digits=1), "%)", sep = "")

    if (any(x == 0)) {
      labels <- labels[x != 0]
      col <- col[x != 0]
      x <- x[x != 0]
    }
    my.tot <- sum(x, na.rm=TRUE)
    ########################
    #this adds extra void area
    #  if does not account for
    #  99 percent of the
    #  measurements
    if (my.tot < 99) {
      x <- c(x, 100 - my.tot)
      labels <- c(labels, "[hide]")
      col <- c(col, NA)
      init.angle <- init.angle + (((100 - my.tot)/200) * 360)
    }
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)

    ######################
    #????
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim

    ########################
    #col setting
    #   this needs generalising like
    #   other pls_plot
    if (is.null(col))
      col <- if (is.null(density))
        c("white", "lightblue", "mistyrose", "lightcyan",
          "lavender", "cornsilk")
    else par("fg")

    ########################
    #border setting
    #   needs generalising...
    if (!is.null(border))
      border <- rep_len(border, nx)

    ##############
    #lty
    #   needs generalising...
    if (!is.null(lty))
      lty <- rep_len(lty, nx)

    ##############
    #angle of segment
    angle <- rep(angle, nx)
    if (!is.null(density))
      density <- rep_len(density, nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    ###########################
    #like to nudge these if percent before and
    #  this one are both small
    #  (making labels close)

    for (i in 1L:nx) {
      if (!as.character(labels[i]) == "[hide]") {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        lattice::lpolygon(c(P$x, rev(P$x * 0.5)), c(P$y, rev(P$y *
                                                               0.5)), density = density[i], angle = angle[i],
                          border = border[1], col = col[i], lty = lty[i])
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
          lattice::llines(c(1, 1.2) * P$x, c(1, 1.2) * P$y)
          lattice::ltext(1.3 * P$x, 1.3 * P$y, labels[i], xpd = TRUE,
                         cex=0.7, adj = ifelse(P$x < 0, 1, 0), ...)
        }
      }
    }
    lattice::ltext(0, 0, label = paste("sum\n", signif(my.tot, 3), "%",
                                       sep = ""), cex=0.7)
  }





#think about
#######################################
# printing amount missing as a segment
# adding plot arg control like in plot.respeciate
# adding args to change the displacement of labels

.rsp_profile_pie <- function (x, labels = names(x), edges = 200, radius = 0.8,
                              clockwise = FALSE,
                              init.angle = if (clockwise) 90 else 0,
                              density = NULL, angle = 45, col = NULL,
                              border = NULL, lty = NULL, main = NULL, ...)
{
  #this is graphics::pie with a couple of modifications...
  #many thanks to...
  #R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation
  #for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.

  #print(labels)
  #print(col)

  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)

  #added to remove any source with a zero contribution
  #but hold labels and col alignment
  if(any(x==0)){
    labels <- labels[x!=0]
    col <- col[x!=0]
    x <- x[x!=0]
  }
  my.tot <- sum(x)
  if(my.tot < 99){
    x <- c(x, 100-my.tot)
    labels <- c(labels, "[hide]")
    col <- c(col, NA)
    init.angle <- init.angle + (((100-my.tot)/200)*360)
  }

  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col))
    col <- if (is.null(density))
      c("white", "lightblue", "mistyrose", "lightcyan",
        "lavender", "cornsilk")
  else par("fg")
  #  if (!is.null(col))
  #    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  if (!is.null(lty))
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    density <- rep_len(density, nx)
  twopi <- if (clockwise)
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }

  for (i in 1L:nx) {

    if(!as.character(labels[i]) == "[hide]"){
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
      #changed shape to include hole
      polygon(c(P$x, rev(P$x*0.5)), c(P$y, rev(P$y*0.5)),
              density = density[i], angle = angle[i],
              border = border[i], col = col[i], lty = lty[i])
      P <- t2xy(mean(x[i + 0:1]))
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        # 1.2 and 1.3 are the extenders when moving labels way from
        # the pie plot itself
        lines(c(1, 1.2) * P$x, c(1, 1.2) * P$y)
        text(1.3 * P$x, 1.3 * P$y, labels[i], xpd = TRUE,
             adj = ifelse(P$x < 0, 1, 0), ...)
      }
    }
  }

  text(0,0, label=paste("sum\n",signif(my.tot, 3), "%", sep=""))
  title(main = main, ...)
  invisible(NULL)
}



###########################
###########################
## pls_refit_species
###########################
###########################


# superseded by pls_fit_species
# not not exported

# need to update the model handling so it is like sp_pls_profile
#     this would sort power issue above
#          also means the user can change setting themselves
#          THINK ABOUT THIS
#               they could make a pls that was not positively constrained


.rsp_pls_refit_species <- function(pls, name, power=1,
                                   ...){
  .xx <- pls_report(pls)
  #name might want to be case-non-sensitive at some point
  #think about how to do this one...
  .data <- .xx[.xx$.species==name,]
  #get and hold all the m_ values
  #update profile contributions for named species
  .ms <- names(.data)[grepl("^m_", names(.xx))]
  .xs <- gsub("^m_", "", .ms)
  .for <- paste("(`", .ms, "`*`", .xs, "`)",
                sep="", collapse = "+")
  .for <- as.formula(paste("test~", .for))
  .da <- .data[!names(.data) %in% .xs]


  .ls <- lapply(.xs, function(x){0})
  names(.ls) <- .xs

  #################
  #user might want to set this???

  .ls2 <- lapply(.xs, function(x){.data[1, x]})
  names(.ls2) <- .xs

  mod <- nls(.for, data=.da,
             #weights = 1/(.out$test^push), # think about weighting
             start=.ls2, lower=.ls,
             algorithm="port",
             control=nls.control(tol=1e-5) #think about tolerance
  )

  .data[.xs] <- data.frame(t(coefficients(mod)))

  #lazy
  .ans <- .data

  for(i in .ans$.profile.id){
    .ii <- subset(.ans, .profile.id==i)
    .ii <- .ii[names(.ii) %in% names(pls[[i]]$args$data)]
    .sp.ord <- unique(pls[[i]]$args$data$.species.id)
    pls[[i]]$args$data <- subset(pls[[i]]$args$data, .species!=name)
    pls[[i]]$args$data <- rbind(pls[[i]]$args$data, .ii)
    #put back in right order
    pls[[i]]$args$data <-
      pls[[i]]$args$data[order(ordered(pls[[i]]$args$data$.species.id,
                                       levels=.sp.ord)),]
    #rebuild model
    .for <- as.character(formula(pls[[i]]$mod))
    .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
    .ms <- names(pls[[i]]$args$data)
    .ms <- .ms[!.ms %in% c(".species.id", ".species", "test")]
    .ls <- lapply(.ms, function(x){0})
    names(.ls) <- paste("m_", .ms, sep="")
    .da <- pls[[i]]$args$data

    pls[[i]]$mod <- nls(.for, data=.da,
                        weights = (1/.da$test)^power, # think about weighting
                        start=.ls, lower=.ls,
                        algorithm="port",
                        control=nls.control(tol=1e-5,
                                            warnOnly = TRUE) #think about tolerance
    )
  }

  invisible(pls)

}



####################################
####################################
## pls_fit_parent
####################################
####################################

# superseded by pls_fit_species
# not now exported

# (like pls_refit_species)
# like to drop power from formals
#   maybe ignore or pass overwrites via ...?

# need to update the model handling so it is like sp_pls_profile
#     this would sort power issue above
#          also means the user can change setting themselves
#          THINK ABOUT THIS
#               they could make a pls that was not positively constrained
#      this would also remove the start, lower and upper options
#           from the formals...

# parent could already be in x
#    then parent could just be the name of parent???

# also a case for using this to add a non-parent to x
#    e.g. pls_fit_unknown_species...
#    to fit a species to the existing model as a source apportion of
#        that unknown...
#    in which case maybe this should just be a wrapper for that
#        with the start, lower and upper like below

# if we are setting start and lower
#     start = lower if start is missing might be safer...


.rsp_pls_fit_parent <- function(pls, parent, power=1,
                                start=100,
                                lower=50, upper=200, ...){

  .out <- pls_report(pls)
  #parent should only have one species
  #and have same profiles as pls model data
  #and its contribution to all sources is set by .value

  .out <- subset(.out, .species.id == unique(.out$.species.id)[1])
  .test <- c(".profile.id", ".value", ".pc.weight")
  .test <- names(parent)[names(parent) %in% .test]
  .data <- parent[.test]
  names(.data)[2] <- "parent"
  .data <- merge(.out, .data[c(1:2)])

  #formula
  .ms <- names(.data)[grepl("^m_", names(.out))]
  .for <- paste("(`", .ms, "`*`", gsub("^m_", "n_", .ms), "`)",
                sep="", collapse = "+")
  .for <- as.formula(paste("parent~", .for))

  .ns <- .ms
  names(.ns) <- gsub("^m_", "n_", .ms)
  .ls <- lapply(.ns, function(x){start})
  .ls2 <- lapply(.ns, function(x){lower})
  .ls3 <- lapply(.ns, function(x){upper})

  mod <- nls(.for, data=.data,
             #weights = (1/.out$test)^power, # think about weighting
             start=.ls,
             lower=.ls2,
             upper=.ls3,
             algorithm="port",
             control=nls.control(tol=1e-5) #think about tolerance
  )
  .ans <- data.frame(
    .profile.id = .data$.profile.id,
    .species.id = parent$.species.id[1],
    .species = parent$.species[1],
    t(coefficients(mod)),
    test = .data$parent
  )
  names(.ans) <- gsub("^n_", "", names(.ans))
  for(i in .ans$.profile.id){
    .ii <- subset(.ans, .profile.id==i)
    .ii <- .ii[names(.ii) != ".profile.id"]
    pls[[i]]$args$data <-
      rbind(pls[[i]]$args$data, .ii)
    #rebuild model
    .for <- as.character(formula(pls[[i]]$mod))
    .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
    .ms <- names(pls[[i]]$args$data)
    .ms <- .ms[!.ms %in% c(".species.id", ".species", "test")]
    .ls <- lapply(.ms, function(x){0})
    names(.ls) <- paste("m_", .ms, sep="")
    .da <- pls[[i]]$args$data

    pls[[i]]$mod <- nls(.for, data=.da,
                        weights = (1/.da$test)^power, # think about weighting
                        start=.ls, lower=.ls,
                        algorithm="port",
                        control=nls.control(tol=1e-5) #think about tolerance
    )
  }

  pls

}





#######################################
########################################
## .rsp_get_[something]_from_pls
#####################################
#######################################

#for use with pls outputs

#note: these current expect pls_report([rsp_pls]) as ONLY input dat

.rsp_get_m_from_pls <- function(dat){

  #get m profiles from a pls model
  #############################################

  #currently assumes you are giving it pls_report output...
  #

  #get m data
  ###########################
  .refs <- names(dat)[grepl("^[.]m_", names(dat))]
  .tmp <- dat[c(".species", .refs)]
  .tmp <- .tmp[!duplicated(.tmp$.species),]

  #restructure
  #########################
  #renaming columns
  .tmp <- data.table::melt.data.table(data.table::as.data.table(.tmp),
                                      id.var=".species")
  .tmp <- as.data.frame(.tmp)
  names(.tmp)[names(.tmp)=="variable"] <- ".profile.id"
  .tmp$.profile.id <- as.character(.tmp$.profile.id)
  .tmp$.profile.id <- gsub("^.m_", "", .tmp$.profile.id)
  names(.tmp)[names(.tmp)=="value"] <- ".value"
  #addition cheats so it is respeciate-like
  .tmp$.profile <- .tmp$.profile.id
  .tmp$.species.id <- .tmp$.species
  .tmp$.pc.weight <- .tmp$.value
  ##similay using rsp_build_x
  ##makes rsp_x but some codes may not be assigned...
  #.p1.prof <- unique(.tmp$.profile.id)
  #.ans <- rsp_build_x(.tmp, test.rsp=FALSE)
  #.cheat <- .ans$.species.id[is.na(.ans$.species.id)]
  #if(length(.cheat)>0){
  #  .ans$.species.id[is.na(.ans$.species.id)] <- .ans$.species[is.na(.ans$.species.id)]
  #}

  #output
  #want to be rsp_x at some point...
  .tmp
}

.rsp_get_prop_from_pls <- function(dat){

  #get x/.value table from pls model...
  #########################################

  #currently assumes you are giving it pls_report output...

  #get x data, etc
  .tmp <- names(dat)
  .tmp <- .tmp[grep("^.x_", .tmp)]
  .refs <- c(.tmp, "pred")
  .sp.ref <- unique(dat$.species)
  #make summary pls. prop.table
  .ans2 <- lapply(.sp.ref, function(x){
    .tmp <- subset(dat, .species==x)
    .d2 <- .tmp[1, c(".species", .refs)]
    for(.ref in .refs){
      #use only paired cases to calculate skew...
      .tmp2 <- .tmp[c(.ref, ".value")]
      .tmp2[.tmp2==0] <- NA
      .tmp2 <- na.omit(.tmp2)
      .d2[, .ref] <- sum(.tmp2[,.ref], na.rm=TRUE) / sum(.tmp2[,".value"], na.rm=TRUE)
    }
    .d2
  })
  .ans2 <- do.call(rbind, .ans2)

  #restructure to output
  .ans2 <- .ans2[names(.ans2)!="pred"]
  .ans2 <- data.table::melt(data.table::as.data.table(.ans2),
                            id.var=".species")
  .ans2 <- as.data.frame(.ans2)
  names(.ans2)[names(.ans2)=="variable"] <- ".profile.id"
  .ans2$.profile.id <- gsub("^[.]x_", "", as.character(.ans2$.profile.id))
  names(.ans2)[names(.ans2)=="value"] <- ".prop"

  #output
  .ans2

}










#####################
#testing
#####################

#playing

#function(x, subset,...){
#  ans <- match.call()
#  ans <- as.character(ans)
#  return(ans)
#}

#ggplot example
#require(ggplot2)
#ggplot() + geom_col(aes(y=.species, x=.pc.weight), data=aa) + facet_grid(.~.profile)










###########################
#diagnostic
##########################

#######################
##rethink this!!!!
#######################

#parking for now


#https://www.rpubs.com/NYang/ANLY530-IRIS

#devtools::unload(); devtools::load_all()
#a <- xxx_test(); subset(a, a$.ref)$.profile

#xxx_test <- function(){
#
#  .tmp <- sysdata$PROFILES
#  .out <-as.data.table(sp_dcast_profile(sp_profile(.tmp$.profile.id)))
#  .tmp <- .tmp[c(".profile.id", "Keywords")]
#  .tmp$.ref <- grepl("wildfire", tolower(.tmp$Keywords)) |
#    grepl("burning", tolower(.tmp$Keywords))
#  .out <- merge(.out, .tmp)
#  .out <- .out[-1599,]
#  #glfit<-glm(as.numeric(.out$.ref)~.out$`Organic carbon` + .out$Nitrate +
#  #             .out$Sulfate, family = 'binomial')
#  glfit<-glm(as.numeric(.out$.ref)~.out$Calcium + .out$Lead +
#                            .out$Zinc +.out$Manganese, family = 'binomial')
#  print(summary(glfit))
#  .out$.pred <- NA
#  .out$.pred[as.numeric(names(predict(glfit)))] <- predict(glfit, type="response")
#  .out
#  #glfit
#}



######################################
#matching
######################################

#Camp Fire

#pb (lead) 0.04 - 0.13 ug/m3
#zinc      0.15 - 0.45
#manganese 0.008 - 0.012
#calcium   0.1-0.2
#organic carbon 20-100

#



########################################
#pca
########################################

#see notes at
#https://www.ime.usp.br/~pavan/pdf/PCA-R-2013
#on r pca methods

#https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
#https://stats.stackexchange.com/questions/56089/using-varimax-rotated-pca-components-as-predictors-in-linear-regression
#https://stackoverflow.com/questions/34944179/doing-pca-with-varimax-rotation-in-r
#on varimax rotation
