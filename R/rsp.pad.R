#' @name rsp.pad
#' @title Meta-data padding respeciate data sets
#' @aliases rsp_pad

#' @description Functions for padding \code{respeciate} objects.

#' @description \code{rsp_pad} pads a supplied respeciate profile data set
#' with profile and species meta-data.
#' @param rsp A \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles.
#' @param pad character, type of meta data padding, current options
#' \code{'profile'}, \code{'species'}, \code{'weight'}, \code{'reference'},
#' \code{'standard'} (default; all but \code{'reference'}), and \code{'all'}
#' (all).
#' @param drop.nas logical, discard any rows where the \code{respeciate}
#' species amount column \code{.pc.weight} is \code{NA}, default \code{TRUE}.
#' @return \code{rsp_pad} returns supplied \code{respeciate} data set, with
#' requested additional profile and species meta-data added as additional
#' \code{data.frame} columns. See Note.
#' @note Some data handling can remove respeciate meta-data,
#' and \code{rsp_pad}s provide a quick rebuild/repair. For example,
#' \code{\link{rsp_dcast}}ing to a (by-species or by-profile) widened
#' form strips some meta-data, and padding is used as part of the
#' \code{\link{rsp_melt_wide}} to re-add this meta-data
#' when returning the data set to its standard long form.
#' @seealso \code{\link{rsp_pad}}

#NOTE

#' @rdname rsp.pad
#' @export
## #' @import data.table
#   now done in xxx.r

#in development
# new pad

# renamed speciate data sysdata -> SPECIATE
# added SPECIEUROPE
#     this currently only pad from SPECIATE...
#         think about how it can handle both...
#         also think about default handling for rsp_x at same time...




## think about

# improving the respeciate class handling
# in -> dcast -> melt -> out

#I do not think this needs the pad argument!!!!
#    or we add an 'all' default???
#        I think it just needs to check with common columns
#            and merge by those
#   testing this with current version of rsp_pad
#        true default is rsp_pad(x, pad="standard")
#            SO refs would be lost with dcast then melt
#        could allow pad as logical or character


# then a reset function would just remove a column and re-pad???

# might want to specify data.table::as.data.table???

# think about a rsp_repair_weight function
#    (or rsp_reset ???)

#not sure we need drop.nas here...
#    think melt is making them...

#does this work for species_id and profile_name?
#should it....

#currently running with respeciate object class
#class at moment
#      "respeciate" (not extension) #standard librray output
#      "respeciate", "rsp_x", # respeciate-like datasets
#      "respeciate", "rsp_pw" or "rsp_piw" # wide by profile
#      "respeciate", "rsp_sw" or "rsp_siw" # wide by species

#      "respeciate", "rsp_pi" # profile info
#      "respeciate", "rsp_si" # species info

# with the second not being defined???
#    assuming this does cause a build problem
#    it is just being used as be an identifier for the respeciate object type
#        standard (long), wide by profile, wide by species,
#        profile info, species info, etc ...
#   might be able to replace this with comment
#        just not sure if the as.data.table/as.data.frame steps would
#        kill that...

#test
#b <- rsp_dcast_species(rsp_us_pm.ae8())
#c <- b[c(".profile.id", ".species", ".pc.weight")]
#x <- rsp_pad(c)


rsp_pad <- function(rsp, pad = "standard", drop.nas = TRUE){

  #should pad allow TRUE/FALSE???
  #should argument within sp_pad be method??

  #tidy rsp
  # this is just adding .value if not there...
  x <- .rsp_tidy_profile(rsp)

  #save class
  .cls <- class(x)
  out <- data.table::as.data.table(x)

  #profile
  if(any(c("profile", "profiles", "standard", "all") %in% tolower(pad))){
    .test <- c(".profile.id", ".profile")
    .test <- .test[.test %in% names(out)]
    if(length(.test)>0){
      profiles <- ..rsp_profile_meta()
      .tmp <- names(profiles)[!names(profiles) %in%
                                intersect(names(out), names(profiles))]
      if(length(.tmp)>0){
        profiles <- profiles[c(.test[1], .tmp)]
        out <- data.table::merge.data.table(data.table::as.data.table(out),
                                            data.table::as.data.table(profiles),
                                            by = .test[1], all.y=FALSE,
                                            all.x=TRUE, allow.cartesian=TRUE)
      }
    }
  }

  #species
  if(any(c("species", "standard", "all") %in% tolower(pad))){
    .test <- c(".species.id", ".species")
    .test <- .test[.test %in% names(out)]
    if(length(.test)>0){
      species <- ..rsp_species_meta()
      .tmp <- names(species)[!names(species) %in%
                                intersect(names(out), names(species))]
      if(length(.tmp)>0){
        species <- species[c(.test[1], .tmp)]
        out <- data.table::merge.data.table(data.table::as.data.table(out),
                                            data.table::as.data.table(species),
                                            by = .test[1], all.y=FALSE,
                                            all.x=TRUE, allow.cartesian=TRUE)
      }
    }
  }

  #weight
  if(any(c("weight", "weights", "standard", "all") %in% tolower(pad))){
    .test <- c(".species.id", ".profile.id")
    .test <- .test[.test %in% names(out)]
    if(length(.test)>1){
      weights <- ..rsp_weights_meta()
      .tmp <- names(weights)[!names(weights) %in%
                               intersect(names(out), names(weights))]
      if(length(.tmp)>0){
        weights <- weights[c(.test[1:2], .tmp)]
        out <- data.table::merge.data.table(data.table::as.data.table(out),
                                            data.table::as.data.table(weights),
                                            by = .test[1:2], all.y=FALSE,
                                            all.x=TRUE, allow.cartesian=TRUE)
      }
    }
  }

  #references
  if(any(c("reference", "references", "all") %in% tolower(pad))){
    .test <- c(".profile.id")
    .test <- .test[.test %in% names(out)]
    if(length(.test)>0){
      refs <- ..rsp_references_meta()
      .tmp <- names(refs)[!names(refs) %in%
                               intersect(names(out), names(refs))]
      if(length(.tmp)>0){
        refs <- species[c(.test[1], .tmp)]
        out <- data.table::merge.data.table(data.table::as.data.table(out),
                                            data.table::as.data.table(refs),
                                            by = .test[1], all.y=FALSE,
                                            all.x=TRUE, allow.cartesian=TRUE)
      }
    }
  }

  #################################################
  #need to think about this
  #################################################
  #weight_percent not there if we don't pad weights
  #     or profiles and will be NA for anything not
  #     in the SPECIATE/SPECIEUROPE archive
  #likewise might want a
  #     if both there only discard if both na...
  #################################################
  #drop.nas.
  if(drop.nas){
    if(".pc.weight" %in% names(out)){
      out <- out[!is.na(out$.pc.weight),]
    }
    if(".value" %in% names(out)){
      out <- out[!is.na(out$.value),]
    }
  }

  #rsp/rsp_profile reorders
  #   not sure if this is a good idea but could add as option
  #      to turn off/on
  if(".profile.id" %in% names(out)){
    out <- out[order(out$.profile.id, decreasing = FALSE),]
  }

  #not sure how to handle output...
  # could return as input class
  # see notes
  out <- as.data.frame(out)

  #.rsp_build_respeciate(out)
  class(out) <- .cls
  out

}





rsp_pad.old <- function(rsp, pad = "standard", drop.nas = TRUE){

  #should pad allow TRUE/FALSE???
  #should argument within sp_pad be method??

  #tidy rsp
  x <- .rsp_tidy_profile(rsp)

  #save class
  .cls <- class(x)
  out <- data.table::as.data.table(x)

  #profile
  if(any(c("profile", "profiles", "standard", "all") %in% tolower(pad))){
    PROFILES <- data.table::as.data.table(SPECIATE$PROFILES)
    PROFILES$PROFILE_CODE <- paste("US:", PROFILES$PROFILE_CODE, sep="")
    .tmp <- intersect(names(out), names(PROFILES))
    if(length(.tmp)>0){
      out <- data.table::merge.data.table(out, PROFILES, by = .tmp, all.y=TRUE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  #species
  if(any(c("species", "standard", "all") %in% tolower(pad))){
    SPECIES_PROPERTIES <- data.table::as.data.table(SPECIATE$SPECIES_PROPERTIES)
    SPECIES_PROPERTIES$SPECIES_ID <- as.character(SPECIES_PROPERTIES$SPECIES_ID)
    .tmp <- intersect(names(out), names(SPECIES_PROPERTIES))
    if(length(.tmp) >0){
      out <- data.table::merge.data.table(out, SPECIES_PROPERTIES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  #species weights
  #    might not want to add weights if rsp_x???
  if(any(c("weight", "weights", "standard", "all") %in% tolower(pad))){
    SPECIES <- data.table::as.data.table(SPECIATE$SPECIES)
    SPECIES$PROFILE_CODE <- paste("US:", SPECIES$PROFILE_CODE, sep="")
    SPECIES$SPECIES_ID <- as.character(SPECIES$SPECIES_ID)
    .tmp <- intersect(names(out), names(SPECIES))
    if(length(.tmp) >0){
      out <- data.table::merge.data.table(out, SPECIES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }
  if(all(is.na(out$WEIGHT_PERCENT)) && ".value" %in% names(out)){
    out$WEIGHT_PERCENT <- out$.value
  }

  #references
  if(any(c("reference", "references", "all") %in% tolower(pad))){
    PROFILE_REFERENCE <- data.table::as.data.table(SPECIATE$PROFILE_REFERENCE)
    PROFILE_REFERENCE$PROFILE_CODE <- paste("US:", PROFILE_REFERENCE$PROFILE_CODE, sep="")
    REFERENCES <- data.table::as.data.table(SPECIATE$REFERENCES)

    .tmp <- intersect(names(out), names(PROFILE_REFERENCE))
    if(length(.tmp) >0){
      out <- data.table::merge.data.table(out, PROFILE_REFERENCE, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
    .tmp <- intersect(names(out), names(REFERENCES))
    if(length(.tmp) >0){
      out <- data.table::merge.data.table(out, REFERENCES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  #################################################
  #need to think about this
  #weight_percent not there if up don't pad weights
  #     or profiles and will be NA for anything not
  #     in the SPECIATE archive
  #################################################
  #drop.nas.

  if(drop.nas){
    out <- out[!is.na(out$WEIGHT_PERCENT),]
  }

  #sp_profile reorders
  #   not sure if it is a good idea but could add as option
  #      here would be
  #      out <- out[order(out$PROFILE_CODE, decreasing = FALSE),]

  #not sure how to handle output...
  # could return as input class
  # see notes
  out <- as.data.frame(out)
  #.rsp_build_respeciate(out)
  class(out) <- .cls
  out

}



############################
#not exporting
############################

#earlier versions

#holding until testing on new code finished




#sp_pad.old <- function(x, pad = "species", drop.nas = TRUE){
#
#  #tidy x
#  x <- rsp_tidy_profile(x)
#  #save class
#  .cls <- class(x)
#  out <- data.table::as.data.table(x)
#
#  #set up padding for melts...
#  .long <- "nothing"
#  if(pad=="species"){
#    .long <- "SPECIES_NAME"
#  }
#  if(pad=="profile"){
#    .long <- "PROFILE_CODE"
#  }
#
#  PROFILES <- data.table::as.data.table(sysdata$PROFILES)
#  SPECIES_PROPERTIES <- data.table::as.data.table(sysdata$SPECIES_PROPERTIES)
#  if(.long=="PROFILE_CODE"){
#    #add in profile then species info
#    out <- merge(out, PROFILES, by = .long, all.y=FALSE,
#                 all.x=TRUE, allow.cartesian=TRUE)
#    .tmp <- intersect(names(out), names(SPECIES_PROPERTIES))
#    out <- merge(out, SPECIES_PROPERTIES, by = .tmp, all.y=FALSE,
#                all.x=TRUE, allow.cartesian=TRUE)
#  }
#  if(.long=="SPECIES_NAME"){
#    #add in species then profiles info
#    out <- merge(out, SPECIES_PROPERTIES, by = .long, all.y=FALSE,
#                 all.x=TRUE, allow.cartesian=TRUE)
#    .tmp <- intersect(names(out), names(PROFILES))
#    out <- merge(out, PROFILES, by = .tmp, all.y=FALSE,
#                 all.x=TRUE, allow.cartesian=TRUE)
#  }
#  #to get weight_percentage etc
#  if(pad %in% c("species", "profile", "weight")){
#    SPECIES <- data.table::as.data.table(sysdata$SPECIES)
#    .tmp <- intersect(names(out), names(SPECIES))
#    out <- merge(out, SPECIES, by = .tmp, all.y=FALSE,
#                 all.x=TRUE, allow.cartesian=TRUE)
#    } else {
#    #not great but...
#    #if not padding WEIGHT_PERCENT has to be .value
#    out$WEIGHT_PERCENT <- out$.value
#  }
#  #drop.nas.
#  if(drop.nas){
#    out <- out[!is.na(out$WEIGHT_PERCENT),]
#  }
#  #############################
#  ##thinking about
#  ##  removing all columns of just NAs...
#  #   out[,which(unlist(lapply(out, function(x)!all(is.na(x))))), with=FALSE]
#  ##  works if data.table object...
#  ############################
#
#  #not sure how to handle output...
#  #see notes
#  out <- as.data.frame(out)
#  rsp_build_respeciate(out)
#}




