#' @name sp.001
#' @title sp_ functions
#' @aliases sp_find_profile sp_find_species sp_profile

#' @description sp functions for use with (re)speciate data in R...

#reversed order of documentation,
#started with the find function...

#' @description \code{sp_find} functions search main data sets
#' in the (re)SPECIATE archive using supplied search terms.
#' \code{\link{sp_find_profile}} searches for profile records and
#' \code{\link{sp_find_species}} searches for species records.
#' @param ... for \code{sp_find} functions, character(s), any
#' search term(s) to use when searching the local (re)SPECIATE archive for
#' relevant records.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'} for \code{\link{sp_find_profile}} and
#' \code{'species_names'} for \code{\link{sp_find}_species}.
#' @param partial logical, if \code{TRUE} (default)
#' \code{sp_find} functions use partial matching.
#' @return \code{sp_find_profile} returns a object of
#' \code{respeciate.ref} class, a \code{data.frame} of
#' profile information.
#' @examples \dontrun{
#' profile <- "Ethanol"
#' dt <- sp_find_profile(profile)
#' dt}
#'

#' @rdname sp.001
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

#' @return \code{sp_find_species} returns a object of
#' \code{respeciate.spcs} class, a \code{data.frame} of
#' species information.
#' @examples \dontrun{
#' species <- "Ethanol"
#' sp <- sp_find_species(species)
#' sp}
#'

#' @rdname sp.001
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






#' @description \code{\link{sp_profile}} extracts a
#' SPECIATE profile from the local (re)SPECIATE archive.
#' @param code character or numeric, the SPECIATE code
#' of the required profile (EPA SPECIATE term PROFILE_CODE).
#' @return \code{sp_profile} returns a object of
#' \code{respeciate} class, a \code{data.frame} containing a
#' speciate profile.
#' @references
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#' @examples \dontrun{
#' x <- sp_profile(c(8833, 8850))
#' plot(x)}

#NOTE

#get_profile allows you to get multiple profiles
#not sure this is staying

utils::globalVariables(c("SPECIES_ID"))

#' @rdname sp.001
#' @export

sp_profile <- function(code) {
  #handle numerics/characters
  #######################
  #could replace code with ...???
  ######################
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  #handle multiple codes
  ############################
  #go direct with %in% ???
  #text as sp_profile2 ???
  ############################
  df <- lapply(code, function(x){
    df <- PROFILES[PROFILES$PROFILE_CODE == x, ]
    df <- merge(df, SPECIES, by = "PROFILE_CODE")
    df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID")
    df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE")
    df <- merge(df, REFERENCES, by = "REF_Code")
    df
  })
  #build
  df <- do.call(rbind, df)
  df <- rsp_build_respeciate(df)
  return(df)
}


