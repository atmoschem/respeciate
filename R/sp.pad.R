#' @name sp.pad
#' @title (re)SPECIATE profile padding functions
#' @aliases sp_pad

#' @description Functions for padding \code{respeciate} objects.

#' @description \code{sp_pad} pads a supplied (re)SPECIATE profile data set
#' with profile and species meta-data.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param pad character, type of meta data padding, current options
#' \code{'profile'}, \code{'species'}, \code{'weight'}, \code{reference},
#' \code{'standard'} (default; all but \code{'reference'}), and \code{'all'}
#' ('all').
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
# in -> dcast -> melt -> out

#I do not think this needs the pad argument!!!!
#    or we add an 'all' default???
#        I think it just needs to check with common columns
#            and merge by those
#   testing this with current version of sp_pad
#        true default is sp_pad(x, pad="standard")
#            SO refs would be lost with dcast then melt
#        could allow pad as logical or character


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


sp_pad <- function(x, pad = "standard", drop.nas = TRUE){


  #should pad allow TRUE/FALSE???
  #should argument within sp_pad be method??

  #tidy x
  x <- rsp_tidy_profile(x)
  #save class
  .cls <- class(x)
  out <- as.data.table(x)

  #profile
  if(any(c("profile", "profiles", "standard", "all") %in% tolower(pad))){
    PROFILES <- as.data.table(sysdata$PROFILES)
    .tmp <- intersect(names(out), names(PROFILES))
    if(length(.tmp)>0){
      out <- merge(out, PROFILES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  #species
  if(any(c("species", "standard", "all") %in% tolower(pad))){
    SPECIES_PROPERTIES <- as.data.table(sysdata$SPECIES_PROPERTIES)
    .tmp <- intersect(names(out), names(SPECIES_PROPERTIES))
    if(length(.tmp) >0){
      out <- merge(out, SPECIES_PROPERTIES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  #species weights
  if(any(c("weight", "weights", "standard", "all") %in% tolower(pad))){
    SPECIES <- as.data.table(sysdata$SPECIES)
    .tmp <- intersect(names(out), names(SPECIES))
    if(length(.tmp) >0){
      out <- merge(out, SPECIES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  #references
  if(any(c("reference", "references", "all") %in% tolower(pad))){
    PROFILE_REFERENCE <- as.data.table(sysdata$PROFILE_REFERENCE)
    REFERENCES <- as.data.table(sysdata$REFERENCES)
    .tmp <- intersect(names(out), names(PROFILE_REFERENCE))
    if(length(.tmp) >0){
      out <- merge(out, PROFILE_REFERENCE, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
    .tmp <- intersect(names(out), names(REFERENCES))
    if(length(.tmp) >0){
      out <- merge(out, REFERENCES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

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
  rsp_build_respeciate(out)

}



############################
#not exporting
############################

#earlier versions

#holding until testing on new code finished

sp_pad.old <- function(x, pad = "species", drop.nas = TRUE){

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




