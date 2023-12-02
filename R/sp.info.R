#' @name sp.info
#' @title re(SPECIATE) information
#' @aliases sp_info sp_profile_info sp_species_info sp_find_profile
#' sp_find_species

###########################
#keep think about the names
###########################
#  sp_profile_info used to be sp_find_profile
#  sp_species_info used to be sp_find_species
#  both sp_find_ functions are currently sp_info_ wrappers
#  should remove at some point???

#########################
#to think about
#########################

#


#' @description Functions that provide (re)SPECIATE
#' source information.
#' \code{sp_info} generates a brief version report for the currently installed
#' (re)SPECIATE data sets.
#' \code{sp_profile_info} searches the currently installed (re)SPECIATE
#' data sets for profile records.
#' \code{sp_species_info} searches the currently installed (re)SPECIATE
#' data sets for species records.

#' @param ... character(s), any search term(s) to use when searching
#' the local (re)SPECIATE archive for relevant records using
#' \code{sp_profile_info} or \code{sp_species_info}.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'} for \code{sp_profile_info} and
#' \code{'species_names'} for \code{sp_species_info}.
#' @param partial logical, if \code{TRUE} (default)
#' \code{sp_profile_info} or \code{sp_profile_info} use partial matching.

#' @return \code{sp_info} provides a brief version information report on the
#' currently installed (re)SPECIATE archive.
#' @return \code{sp_profile_info} returns a \code{data.frame} of
#' profile information, as a \code{respeciate} object.
#' \code{sp_species_info} returns a \code{data.frame} of
#' species information as a \code{respeciate} object.

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


#######################
#sp_info
#######################

# tidy output???
#    little messy???

#like to include summary(factor(sysdata$PROFILES$PROFILE_TYPE))
#  or something like that

# this is not currently catchable!!!!
# a <- sp_info() #a = NULL

#' @rdname sp.info
#' @export

sp_info <- function() {
  #extract profile info from archive
  .ver <- "source: SPECIATE 5.2\n\t[in (re)SPECIATE since 0.2.0]"
  .ver <- paste(.ver, "\n\t[now (re)SPECIATE ", packageVersion("respeciate"), "]", sep="")
  .pro <- length(unique(sysdata$PROFILES$PROFILE_CODE))
  .spc <- length(unique(sysdata$SPECIES_PROPERTIES$SPECIES_ID))
  cat(.ver, "\n\tProfiles: ", .pro, "\n\tSpecies: ", .spc, "\n", sep = "")
}


#' @rdname sp.info
#' @export

sp_profile_info <- function(..., by = "keywords", partial = TRUE) {
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
  out <- rsp_build_respeciate(out)
  return(out)
}

#' @rdname sp.info
#' @export

#wrapper for above

sp_find_profile <- function(...){
  sp_profile_info(...)
}

#' @rdname sp.info
#' @export

sp_species_info <- function(..., by = "species_name", partial = TRUE) {
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
  out <- rsp_build_respeciate(out)
  return(out)
}

#' @rdname sp.info
#' @export

#wrapper for above

sp_find_species <- function(...){
  sp_species_info(...)
}
