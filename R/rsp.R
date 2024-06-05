#' @name rsp
#' @title Getting archived profiles
#' @aliases rsp rsp_profile

#' @description  Getting source profile(s) from the local respeciate archives.

#' @param ... The function assumes all inputs (except \code{include.refs}
#' and \code{source}) are profile identifiers: namely, \code{PROFILE_CODE}
#' and \code{Species.Id} in SPECIATE and SPECIEUROPE, respectively, or
#' potential sources of profile information and requests these
#' form the local respeciate archives. Typically, simple
#' objects like character and numeric vectors, as assumed to be profile
#' identifiers and composite data-types like \code{respeciate} or
#' \code{data.frame} objects are assumed to contain a column named
#' \code{.profile.id}, the \code{respeciate} equivalent of \code{PROFILE_CODE}
#' and \code{Species.Id}. All recovered identifiers are requested
#' and unrecognized ids (and duplicates) are ignored.
#' @param include.refs logical, if profile reference information should be
#' included when extracting the requested profile(s) from the archive, default
#' \code{FALSE}.
#' @param source character, the local archive to request a profile from:
#' \code{'us'} US EPA SPECIATE, \code{'eu'} EU JRC SPECIEUROPE, or \code{'all'}
#' (the default) both.
#' @return \code{rsp_profile} or the short-hand \code{rsp} return an object of
#' \code{respeciate} class, a \code{data.frame} containing one or more profile
#' from the local respeciate archive.
#' @note The option \code{include.refs} adds profile source reference
#' information to the returned \code{respeciate} data set. The default option
#' is to not include these because some SPECIATE profiles have several
#' associated references and including these replicates records, once per
#' reference.
#' \code{respeciate} code is written to handle this but if you are developing
#' own methods or code and include references in any profile build you may be
#' biasing some analyses in favor of those multiple-reference profile unless
#' you check and account such cases.
#' @seealso \code{\link{SPECIATE}} and \code{\link{SPECIEUROPE}} regarding
#' data sources; and, \code{\link{rsp_find_profile}}
#' and \code{\link{rsp_find_species}} regarding archive searching.
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
#' x <- rsp_profile(8833, 8850)
#' plot(x)}


##     (now importing via xxx.r)
##     #' @import data.table

# importing data.table generates a warning I can't fix...

# but means we need to set data.table specifically
#      data.table::as.data.table, etc??
#          see xxx.r for data.table imports and notes...


#NOTES
#######################

# 0.3. notes
# went from sp_profile to rsp_profile (and rsp)
# dropped code argument
# using as.respeciate in generics to build rsp object
# dropping generic unexported rsp_build_respeciate(x)
#    replaced with as.respeciate

# 0.3.1 notes
# went to rsp as main (rsp_profile as wrapper)
# went from sysdata to SPECIATE as SPECIATE source
#      (when adding SPECIEUROPE)
# added source argument, default 'us' (get from SPECIATE)

#to think about
#######################

## rsp_import_profile to import a profile from an external source
##     extension of rsp_build_x ???
##           might be very code intensive..?

## local function to pad data using database(s) meta info???

#####################
#to think about
#####################

# not sure but I think main SPECIATE build is:
#    (default; include.refs = FALSE) [source="us"]
#    PROFILES[subset.requested.codes]>>SPECIES>>SPECIES_PROPERTIES
#    (full build; include.refs = TRUE) [source="us"]
#    PROFILES[subset.requested.codes]>>SPECIES>>SPECIES_PROPERTIES>>PROFILE_REFERENCE>>REFERENCES
#    (BUT this is replicating profiles with more than 1 reference...)

# SPECIEUROPE build is simpler because it is just one data frame
#    [saved as list(source=[data.frame]), in case we need to add any supporting meta-data]
#    (default; include.refs = FALSE) [source="eu"]
#    source[subset.requested.codes];remove(REFRENCES)
#    (full build; include.refs = TRUE) [source="em"]
#    source[subset.requested.codes]

#' @rdname rsp
#' @export

rsp <- function(..., include.refs=FALSE, source="all") {

  # ... handling
  #     v 0.3 (kr)
  ######################

  ###################################
  #need to change this to .profile.id
  ###################################

  # ... currently allows:
  # data.frame-like objects containing profile_code column
  #      so data.frame, respeciate, etc...
  # numeric and character vectors
  .try <- lapply(list(...), function(.code){
    if(is.data.frame(.code) && ".profile.id" %in% names(.code)){
      .code <- unique(.code$.profile.id)
    }
    if(is.numeric(.code)) {
      .code <- as.character(.code)
    }
    if(!is.character(.code)) {
      warning("RSP> unexpected PROFILE ID source found and ignored",
              call.=FALSE)
      .code <- NULL
    }
    .code
  })
  code <- do.call(c, .try)

  #SPECIATE/SPECIEUROPE handling
  ###################################
  #this and archive searching functions
  #     are currently case insensitive
  if(!tolower(source) %in% c("all", "us", "eu")){
    stop("RSP> unknown 'source' requested...",
         call.=FALSE)
  }
  code <- gsub("us:|us:", "", tolower(code))
  if(tolower(source)=="us"){
    code <- paste("US:", code, sep="")
  }
  if(tolower(source)=="eu"){
    code <- paste("EU:", code, sep="")
  }
  if(tolower(source)=="all"){
    code <- c(paste("US:", code, sep=""), paste("EU:", code, sep=""))
  }

  #build profiles from ..rsp sources
  ######################################
  #do we need to make it more like previous???
  ######################################
  prof <- data.table::as.data.table(..rsp_profile_meta())
  out <- subset(prof, tolower(.profile.id) %in% tolower(code))
  out <- merge(out, ..rsp_weights_meta(),
               by = ".profile.id", all.y=FALSE, all.x=TRUE,
               allow.cartesian=TRUE)
  out <- merge(out, ..rsp_species_meta(),
               by = ".species.id", all.y=FALSE, all.x=TRUE,
               allow.cartesian=TRUE)
  if(include.refs){
    out <- merge(out, ..rsp_references_meta(),
                 by = ".profile.id", all.y=FALSE, all.x=TRUE,
                 allow.cartesian=TRUE)
  }

  ###############################
  #tidy and return
  ################################
  #reorder profiles (like previous)
  out <- out[order(out$.profile.id, decreasing = FALSE),]
  #add weights...
  out$.pc.weight <- as.numeric(out$.pc.weight)
  out$.value <- out$.pc.weight
  rsp <- as.respeciate(out, test.rsp=FALSE)
  return(rsp)
}


#' @rdname rsp
#' @export


rsp_profile <- function(...) { rsp(...) }


#might be dropping this...

rsp.old <- function(..., include.refs=FALSE, source="all") {
  .try <- lapply(list(...), function(.code){
    if(is.data.frame(.code) && "PROFILE_CODE" %in% names(.code)){
      .code <- unique(.code$PROFILE_CODE)
    }
    if(is.numeric(.code)) {
      .code <- as.character(.code)
    }
    if(!is.character(.code)) {
      warning("RSP> unexpected 'PROFILE_CODE' source found and ignored",
              call.=FALSE)
      .code <- NULL
    }
    .code
  })
  code <- do.call(c, .try)
  if(!source %in% c("all", "us", "eu")){
    stop("RSP> unknown 'source' requested...",
         call.=FALSE)
  }
  if(tolower(source) %in% c("us", "all")){
    #################################
    #get SPECIATE profile using code
    #################################
    PROFILES <- data.table::as.data.table(SPECIATE$PROFILES)
    SPECIES <- data.table::as.data.table(SPECIATE$SPECIES)
    SPECIES_PROPERTIES <- data.table::as.data.table(SPECIATE$SPECIES_PROPERTIES)
    PROFILE_REFERENCE <- data.table::as.data.table(SPECIATE$PROFILE_REFERENCE)
    REFERENCES <- data.table::as.data.table(SPECIATE$REFERENCES)
    ##########################
    #testing tolower below
    #   as a fix for code arg case sensitivity
    ##########################
    #  could test replacing some of this with rsp_pad???

    dt <- PROFILES[tolower(PROFILES$PROFILE_CODE) %in% gsub("^us:", "", tolower(code)),]
    dt <- merge(dt, SPECIES, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE,
                allow.cartesian=TRUE)
    dt <- merge(dt, SPECIES_PROPERTIES, by = "SPECIES_ID", all.y=FALSE,
                all.x=TRUE, allow.cartesian=TRUE)
    if(include.refs){
      dt <- merge(dt, PROFILE_REFERENCE, by = "PROFILE_CODE", all.y=FALSE,
                  all.x=TRUE, allow.cartesian=TRUE)
      dt <- merge(dt, REFERENCES, by = "REF_Code", all.y=FALSE, all.x=TRUE,
                  allow.cartesian=TRUE)
    }
    dt <- dt[order(dt$PROFILE_CODE, decreasing = FALSE),]
    #add .value if weight_percent to copy...
    dt$WEIGHT_PERCENT <- as.numeric(dt$WEIGHT_PERCENT)
    if("WEIGHT_PERCENT" %in% names(dt) & !".value" %in% names(dt)) {
      dt$.value <- dt$WEIGHT_PERCENT
    }
    #print(head(dt$WEIGHT_PERCENT))
    dt$PROFILE_CODE <- paste("US:", dt$PROFILE_CODE, sep="")
    dt.us <- dt
  } else {
    dt.us <- NULL
  }
  if(tolower(source) %in% c("eu", "all")){
    ######################################
    #currently not data.table-ing this...
    ######################################
    x <- SPECIEUROPE$source
    x <- subset(x, tolower(as.character(Id)) %in% gsub("^eu:", "", tolower(code)))
    if(!include.refs){
      x <- x[names(x) != "Reference"]
    }
    dt.eu <- data.table::as.data.table(.rsp_eu2us(x))
  } else {
    dt.eu <- NULL
  }

  #output
  x <- data.table::rbindlist(list(dt.us, dt.eu), fill=TRUE)
  ###################################
  #note
  ####################################
  #data.table::rbindlist seems to be forcing
  #    SPECIATE WEIGHT_PERCENT to character
  x$WEIGHT_PERCENT <- as.numeric(x$WEIGHT_PERCENT)
  rsp <- as.respeciate(x, test.rsp=FALSE)
  return(rsp)
}








