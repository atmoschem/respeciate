#' @name rsp.info
#' @title Information about data sets currently in respeciate
#' @aliases rsp_info rsp_profile_info rsp_species_info rsp_find_profile
#' rsp_find_species

###########################
# keep thinking about the names
###########################
#  rsp_profile_info/ rsp_find_profile
#  rsp_species_info/ rsp_find_species

# currently thinking about making
#    a page for rsp_info
#    and a page for rsp_find_... functions
#        (and dropping the rsp_profile_info, etc, )

#########################
#to think about
#########################

# testing idea to remove need to set source...
#    following seems messy
#        rsp(rsp_find_profile("diesel", source="eu"), source="eu")
#    using profile_code = US:... and EU:... for source used
#        now need to set source when you want to be specific...
#    notes:
#        seems to be working BUT...
#    rsp_find_species might have issue
#        because species info can be coming from either
#        SPECIATE or SPECIEUROPE...
#    data.table::rbindlist seems to be forcing WEIGHT_PERCENT to character
#        when bind is list(us, NULL)...

#' @description Functions that provide respeciate
#' source information.
#' \code{rsp_info} generates a brief version report for the currently installed
#' respeciate data sets.
#' \code{rsp_profile_info} searches the currently installed respeciate
#' data sets for profile records.
#' \code{rsp_species_info} searches the currently installed respeciate
#' data sets for species records.

#' @param ... character(s), any search term(s) to use when searching
#' the local respeciate archive for relevant records using
#' \code{rsp_profile_info} or \code{rsp_species_info}.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'} for \code{rsp_profile_info} and
#' \code{'species_names'} for \code{sp_species_info}.
#' @param partial logical, if \code{TRUE} (default)
#' \code{rsp_profile_info} or \code{rsp_profile_info} use partial matching.
#' @param source character, the data set to search: \code{'us'}
#' US EPA SPECIATE; \code{'eu'} JRC SPECIEUROPE; or, \code{'all'} (default)
#' both archives.
#' @return \code{rsp_info} provides a brief version information report on the
#' currently installed respeciate archive.
#' @return \code{rsp_profile_info} returns a \code{data.frame} of
#' profile information, as a \code{respeciate} object.
#' \code{rsp_species_info} returns a \code{data.frame} of
#' species information as a \code{respeciate} object.
#' @seealso \code{\link{SPECIATE}} and \code{\link{SPECIEUROPE}}
#' @references
#' For SPECIATE:
#'
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#'
#' For SPECIEUROPE:
#'
#' Pernigotti, D., Belis, C.A., Spano, L., 2016. SPECIEUROPE: The
#' European data base for PM source profiles. Atmospheric Pollution Research,
#' 7(2), pp.307-314. DOI: https://doi.org/10.1016/j.apr.2015.10.007
#'


#' @examples \dontrun{
#' profile <- "Ethanol"
#' pr <- rsp_find_profile(profile)
#' pr
#'
#' species <- "Ethanol"
#' sp <- rsp_find_species(species)
#' sp}
#'

#might want to replace this with example that
#   finds profile containing ethanol?


#######################
#rsp_info
#######################

# tidy output???
#    little messy???

#like to include summary(factor(SPECIATE$PROFILES$PROFILE_TYPE))
#  or something like that

# this is not currently catchable!!!!
# a <- sp_info() #a = NULL

#' @rdname rsp.info
#' @export

rsp_info <- function() {
  #extract profile info from archive

  .epa <- paste("source: SPECIATE 5.2\n\t[in respeciate since 0.2.0]",
                "\n\tProfiles: ", length(unique(SPECIATE$PROFILES$PROFILE_CODE)),
                "; species: ", length(unique(SPECIATE$SPECIES_PROPERTIES$SPECIES_ID)),
                sep="")
  .eu <- paste("source: SPECIEUROPE \n\t[in respeciate since 0.3.1]",
               "\n\tProfiles: ", length(unique(SPECIEUROPE$source$Id)),
               "; species: ", length(unique(SPECIEUROPE$source$Specie.Id)),
               sep="")
  .sts <- paste("respeciate: ", packageVersion("respeciate"), "", sep="")
  cat(.sts, "\n", .epa, "\n", .eu, "\n", sep = "")
}


#' @rdname rsp.info
#' @export

rsp_profile_info <- function(..., by = "keywords", partial = TRUE,
                             source = "all") {

  #extract profile info from archive
  if(!source %in% c("us", "eu", "all")){
    stop("RSP> unknown source...",
         call.=FALSE)
  }
  if(source %in% c("us", "all")){
    out.us <- SPECIATE$PROFILES
    out.us$PROFILE_CODE <- paste("US:", out.us$PROFILE_CODE, sep="")
    species.us <- SPECIATE$SPECIES
    species.us$PROFILE_CODE <- paste("US:", species.us$PROFILE_CODE, sep="")
  } else {
    out.us <- NULL
    species.us <- NULL
  }
  if(source %in% c("eu", "all")){
    temp <- .rsp_eu2us(SPECIEUROPE$source)
    #.rsp_eu2us does next line...
    #out.eu$PROFILE_CODE <- paste("EU:", out.eu$PROFILES$PROFILE_CODE, sep="")
    out.eu <- temp[c("PROFILE_CODE", "PROFILE_NAME", "PROFILE_TYPE", "Original.Name", "Country",
                 "Place", "Test.Year", "Profile.Type", "Latitude", "Longitude")]
    out.eu$Keywords <- out.eu$PROFILE_NAME
    out.eu <- out.eu[!duplicated(out.eu$PROFILE_CODE),]
    species.eu <- temp[c("SPECIES_ID", "SPECIES_NAME", "Analythical.Method", "Uncertainty.Method",
                      "Sampling.Method", "Cas", "Symbol", "PROFILE_CODE")]
    #species.eu <- species.eu[!duplicated(species.eu$SPECIES_ID),]
  } else {
    out.eu <- NULL
    species.eu <- NULL
  }
  out <- as.data.frame(data.table::rbindlist(list(out.us, out.eu), fill=TRUE))
  species <- as.data.frame(data.table::rbindlist(list(species.us, species.eu), fill=TRUE))

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
  #    SPECIATE$SPECIES$PROFILE_CODE[SPECIATE$SPECIES$SPECIES_ID==529]
  #    could use sp_find_species to get species info, then
  ###################################
  if(tolower(by) %in% c("species_name")){
    ############################
    #special case
    #search by species_name
    #    could add species_id, cas, etc?
    ###############################
    ref <- out$PROFILE_CODE
    for(ti in terms){
      ans <- rsp_species_info(ti, by=by, partial=partial)
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

  ##########################
  # to check
  ##########################
  #    should this now be as.respeciate ???
  out <- .rsp_build_respeciate(out)
  class(out) <- unique(c("rsp_pi", class(out)))
  return(out)
}

#' @rdname rsp.info
#' @export

#wrapper for above

rsp_find_profile <- function(...){
  rsp_profile_info(...)
}

#' @rdname rsp.info
#' @export

rsp_species_info <- function(..., by = "species_name", partial = TRUE,
                             source = "all") {
  #extract species info from archive
  if(!source %in% c("us", "eu", "all")){
    stop("RSP> unknown source...",
         call.=FALSE)
  }
  if(source %in% c("us", "all")){
    out.us <- SPECIATE$SPECIES_PROPERTIES
    out.us <- data.table::as.data.table(out.us)
  } else {
    out.us <- NULL
  }
  if(source %in% c("eu", "all")){
    temp <- .rsp_eu2us(SPECIEUROPE$source)
    out.eu <- temp[c("SPECIES_ID", "SPECIES_NAME", "Analythical.Method", "Uncertainty.Method",
                  "Sampling.Method", "Cas", "Symbol")]
    out.eu <- out.eu[!duplicated(out.eu$SPECIES_ID),]
    out.eu <- data.table::as.data.table(out.eu)
  } else {
    out.eu <- NULL
  }
  terms <- c(...)
  out <- as.data.frame(data.table::rbindlist(list(out.us, out.eu), fill=TRUE))

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
  ##########################
  # to check
  ##########################
  #    should this now be .respeciate ???
  out <- .rsp_build_respeciate(out)
  class(out) <- unique(c("rsp_si", class(out)))
  return(out)
}

#' @rdname rsp.info
#' @export

#wrapper for above

rsp_find_species <- function(...){
  rsp_species_info(...)
}
