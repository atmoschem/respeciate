#' @name rsp.find
#' @title Information about data sets currently in respeciate
#' @aliases rsp_find rsp_find_profile rsp_find_species

#######################
# recent changes:
#######################

# in 0.3.1

# separated rsp_info and rsp_find.../rsp_..._info functions
#      also dropped forms of rsp_..._info functions...


#########################
#to think about
#########################

## testing idea to remove need to set source...

#    following seemed messy
#        rsp(rsp_find_profile("diesel", source="eu"), source="eu")
#    using .profile.id = US:... and EU:... for source used
#        Only need to set source when you want to be specific...
#    notes:
#        seems to be working BUT...
#    rsp_find_species might have issue
#        because species info can be coming from either
#        SPECIATE or SPECIEUROPE... and overlap is not perfect
#              see notes

#  a <- rsp_find_profile("composite", by="profile_name")
#  rsp_find_profile("pm", by=".profile.type", ref=a)


#' @description Functions that provide respeciate
#' source information.
#' \code{rsp_find_profile} searches the currently installed respeciate
#' data sets for profile records.
#' \code{rsp_find species} searches the currently installed respeciate
#' data sets for species records.

#' @param ... character(s), any search term(s) to use when searching
#' the local respeciate archive for relevant records using
#' \code{rsp_find_profile} or \code{rsp_find_species}.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'} for \code{rsp_find_profile} and
#' \code{'.species'} for \code{sp_find_species}.
#' @param partial logical, if \code{TRUE} (default)
#' \code{rsp_find_profile} and \code{rsp_find_species} use partial matching.
#' @param source character, the data set to search: \code{'us'}
#' US EPA SPECIATE; \code{'eu'} JRC SPECIEUROPE; or, \code{'all'} (default)
#' both archives.
#' @param ref any \code{respeciate} object, \code{data.frame} or similar
#' that profile or species information can be extracted from.

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


#' @rdname rsp.find
#' @export

rsp_find_profile <- function(..., by = "keywords", partial = TRUE,
                             source = "all", ref = NULL) {

  #extract profile info from archive
  if(!tolower(source) %in% c("us", "eu", "all")){
    stop("RSP> unknown source...",
         call.=FALSE)
  }
  out <- ..rsp_profile_meta()
  if(tolower(source)=="us"){
    out <- subset(out, grepl("^US:", out$.profile.id))
  }
  if(tolower(source)=="eu"){
    out <- subset(out, grepl("^EU:", out$.profile.id))
  }

  #if ref provided get similar from ref
  # and use that as out...
  if(!is.null(ref)){
    ref <- ref[names(ref) %in% names(out)]
    ref <- ref[!duplicated(ref$.profile.id),]
    out <- ref
  }

  #search for requested in
  terms <- c(...)

  if(tolower(by) %in% c(".species")){
    ############################
    #special case
    #search by .species
    #     think about this???
    ###############################
    species <- ..rsp_weights_meta()
    # using weights for the species and profile mapping

    ref <- out$.profile.id
    for(ti in terms){
      ans <- rsp_species_info(ti, by=by, partial=partial)
      terms <- species$.profile.id[species$.species.id %in% ans$.species.id]
      ref <- ref[ref %in% terms]
    }
    out <- out[out$.profile.id %in% ref,]
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

#' @rdname rsp.find
#' @export

#wrapper for above

rsp_profile_info <- function(...){
  rsp_find_profile(...)
}

#' @rdname rsp.find
#' @export

rsp_find_species <- function(..., by = ".species", partial = TRUE,
                             source = "all", ref = NULL) {
  #extract species info from archive
  if(!source %in% c("us", "eu", "all")){
    stop("RSP> unknown source...",
         call.=FALSE)
  }
  out <- ..rsp_species_meta()
  if(tolower(source)=="us"){
    out <- out[!is.na(out$SPECIES_ID),]
  }
  if(tolower(source)=="eu"){
    out <- out[!is.na(out$Species.Id),]
  }
  if(!is.null(ref)){
    ref <- ref[names(ref) %in% names(out)]
    ref <- ref[!duplicated(ref$.species.id),]
    out <- ref
  }

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
  ##########################
  # to check
  ##########################
  #    should this now be .respeciate ???
  out <- .rsp_build_respeciate(out)
  class(out) <- unique(c("rsp_si", class(out)))
  return(out)
}

#' @rdname rsp.find
#' @export

#wrapper for above

rsp_species_info <- function(...){
  rsp_find_species(...)
}
