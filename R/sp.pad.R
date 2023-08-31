#' @name sp.pad
#' @title (re)SPECIATE profile padding functions
#' @aliases sp_pad

#' @description Functions for padding \code{respeciate} objects.

#' @description \code{sp_pad} pads a supplied (re)SPECIATE profile data set
#' with profile and species meta-data.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param pad character, type of meta data padding, current options
#' \code{'profile'}, \code{'species'} or \code{'weight'}.
#' @param drop.nas logical, discard any rows where \code{WEIGHT_PERCENT} is
#' \code{NA}, default \code{TRUE}.
#' @return \code{sp_pad} returns \code{x}, with requested additional profile
#' and species meta-data added as additional \code{data.frame} columns.
#' See Note.
#' @note Some data handling can remove (re)SPECIATE meta-data,
#' and \code{sp_pad}s provide a quick rebuild/repair. For example,
#' \code{\link{sp_dcast}}ing to a (by-species or by-profile) widened
#' form strips some meta-data, and padding is used as part of the
#' \code{\link{sp_melt_wide}} and padding is used to re-add this meta-data
#' when returning the data set to its standard long form.

#NOTE

#' @rdname sp.pad
#' @export
## #' @import data.table
#   now done in xxx.r

#in development

## think about

# improving the respeciate class handling
# dcast -> melt -> out

#I do not think this needs the pad argument!!!!
#    or we add an 'all' default???
#        I think it just needs to check with common columns
#            and merge by those

# then a reset function would just remove a column and re-pad???

# might want to specify data.frame::as.data.table???

#think about a sp_repair_weight function

#not sure we need drop.nas here...
#    think melt is making them...

#does this work for species_id and profile_name?
#should it....

#need to think about respeciate object class
#class have
#      "respeciate", ".rsp"
#      "respeciate", ".rsp.wp"
#      "respeciate", ".rsp.ws"
#      "respeciate", ".rsp.pi"
#      "respeciate", ".rsp.si"
# with the second not being defined???
#    assuming this does cause a build problem
#    it would just be an identifier for the respeciate object type
#        standard (long), wide by profile, wide by species,
#        profile info, species info...


#test
#b <- sp_dcast_species(spq_pm.ae8())
#c <- b[c("PROFILE_CODE", "SPECIES_NAME", "WEIGHT_PERCENT")]
#x <- sp_pad(c)



sp_pad <- function(x, pad = "species", drop.nas = TRUE){

  #tidy x
  x <- rsp_tidy_profile(x)
  #save class
  .cls <- class(x)
  out <- as.data.table(x)

  #set up padding for melts...
  .long <- "nothing"
  if(pad=="species"){
    .long <- "SPECIES_NAME"
  }
  if(pad=="profile"){
    .long <- "PROFILE_CODE"
  }

  PROFILES <- as.data.table(sysdata$PROFILES)
  SPECIES_PROPERTIES <- as.data.table(sysdata$SPECIES_PROPERTIES)
  if(.long=="PROFILE_CODE"){
    #add in profile then species info
    out <- merge(out, PROFILES, by = .long, all.y=FALSE,
                 all.x=TRUE, allow.cartesian=TRUE)
    .tmp <- intersect(names(out), names(SPECIES_PROPERTIES))
    out <- merge(out, SPECIES_PROPERTIES, by = .tmp, all.y=FALSE,
                all.x=TRUE, allow.cartesian=TRUE)
  }
  if(.long=="SPECIES_NAME"){
    #add in species then profiles info
    out <- merge(out, SPECIES_PROPERTIES, by = .long, all.y=FALSE,
                 all.x=TRUE, allow.cartesian=TRUE)
    .tmp <- intersect(names(out), names(PROFILES))
    out <- merge(out, PROFILES, by = .tmp, all.y=FALSE,
                 all.x=TRUE, allow.cartesian=TRUE)
  }
  #to get weight_percentage etc
  if(pad %in% c("species", "profile", "weight")){
    SPECIES <- as.data.table(sysdata$SPECIES)
    .tmp <- intersect(names(out), names(SPECIES))
    out <- merge(out, SPECIES, by = .tmp, all.y=FALSE,
                 all.x=TRUE, allow.cartesian=TRUE)
    } else {
    #not great but...
    #if not padding WEIGHT_PERCENT has to be .value
    out$WEIGHT_PERCENT <- out$.value
  }
  #drop.nas.
  if(drop.nas){
    out <- out[!is.na(out$WEIGHT_PERCENT),]
  }

  #not sure how to handle output...
  #see notes
  out <- as.data.frame(out)
  rsp_build_respeciate(out)
}







