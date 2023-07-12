#' @name sp.find
#' @title re(SPECIATE) sp_find_ functions
#' @aliases sp_find_profile sp_find_species

###########################
#keep think about the names
###########################
#  wondering if find_profile_info, sp_find_profile_info or
#    find_sp_profile_info would be better?
#  like to make it keep these are not profiles..?

#' @description \code{sp_find_} functions are for use with (re)SPECIATE
#' source data in R...
#' @description \code{sp_find_} functions search main data sets
#' in the (re)SPECIATE archive using supplied search terms.
#' \code{\link{sp_find_profile}} searches for profile records and
#' \code{\link{sp_find_species}} searches for species records.
#' @param ... character(s), any search term(s) to use when searching
#' the local (re)SPECIATE archive for relevant records.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'} for \code{\link{sp_find_profile}} and
#' \code{'species_names'} for \code{\link{sp_find_species}}.
#' @param partial logical, if \code{TRUE} (default)
#' \code{sp_find_} functions use partial matching.
#' @return \code{sp_find_profile} returns a object of
#' \code{respeciate.ref} class, a \code{data.frame} of
#' profile information.
#' \code{sp_find_species} returns a object of
#' \code{respeciate.spcs} class, a \code{data.frame} of
#' species information.

#' @examples \dontrun{
#' profile <- "Ethanol"
#' pr <- sp_find_profile(profile)
#' pr
#'
#' species <- "Ethanol"
#' sp <- sp_find_species(species)
#' sp}
#'

#might want to replace this with example that
#   finds profile containing ethanol?


#' @rdname sp.find
#' @export

sp_find_profile <- function(..., by = "keywords", partial = TRUE) {
  #extract profile info from archive
  out <- sysdata$PROFILES
  terms <- c(...)
  ###################################
  #ignoring case because missing loads...
  #   might want to think about spaces as well???
  #currently same in sp_find_species
  #   should think about using common code???
  #   also error messaging if by is not known???
  ###################################
  #how to handle searching a profile
  #that contains a specific species
  #    this is all the profiles that have an entry for species_id 529
  #    sysdata$SPECIES$PROFILE_CODE[sysdata$SPECIES$SPECIES_ID==529]
  #    could use sp_find_species to get species info, then
  ###################################
  if(tolower(by) %in% c("species_name")){
    ############################
    #special case
    #search by species_name
    #    could add species_id, cas, etc?
    ###############################
    species <- sysdata$SPECIES
    ref <- out$PROFILE_CODE
    for(ti in terms){
      ans <- sp_find_species(ti, by=by, partial=partial)
      terms <- species$PROFILE_CODE[species$SPECIES_ID %in% ans$SPECIES_ID]
      ref <- ref[ref %in% terms]
    }
    out <- out[out$PROFILE_CODE %in% ref,]
  } else {
    for(ti in terms){
      ref <- out[,tolower(names(out))==by]
      if(nrow(out)>0){
        out <- if(partial){
          out <- out[grep(ti, ref, ignore.case = TRUE),]
        } else {
          out <- out[tolower(ti)==tolower(ref),]
        }
      }
    }
  }
  out <- rsp_build_respeciate.ref(out)
  return(out)
}


#' @rdname sp.find
#' @export

sp_find_species <- function(..., by = "species_name", partial = TRUE) {
  #extract species info from archive
  out <- sysdata$SPECIES_PROPERTIES
  terms <- c(...)
  for(ti in terms){
    ref <- out[,tolower(names(out))==by]
    if(nrow(out)>0){
      out <- if(partial){
        out <- out[grep(ti, ref, ignore.case = TRUE),]
      } else {
        out <- out[tolower(ti)==tolower(ref),]
      }
    }
  }
  #out <- PROFILES[grep(term, PROFILES[[by]], ignore.case = TRUE), ]
  out <- rsp_build_respeciate.spcs(out)
  return(out)
}

