#' @name rsp
#' @title rsp_profile
#' @aliases rsp rsp_profile


#' @description  Getting profile(s) from the R (re)SPECIATE archive

#' @param ... The function assumes all inputs (except \code{include.refs})
#' are \code{SPECIES_CODE}s (the unique descriptor the EPA assigns to all
#' profiles in SPECIATE) or sources of profile information and requests these
#' form the local (re)SPECIATE archive. Typically, simple
#' objects like character and numeric vectors, as assumed to profile codes and
#' composite data-types like \code{respeciate} objects or \code{data.frame},
#' are assumed to contain a named \code{PROFILE_CODE} column. All potential
#' profile codes are requested and unrecognized codes are ignored.
#' @param include.refs logical, if profile reference information should be
#' included when extracting the requested profile(s) from the archive, default
#' \code{FALSE}.
#' @return \code{rsp_profile} or the short-hand \code{rsp} return an object of
#' \code{respeciate} class, a \code{data.frame} containing one or more profile
#' from the local (re)SPECIATE archive.
#' @note The option \code{include.refs} adds profile source reference
#' information to the returned \code{respeciate} data set. The default option
#' is to not include these because some profiles have several associated
#' references and including these replicates records, once per reference.
#' \code{respeciate} code is written to handle this but if you are developing
#' own methods or code and include references in any profile build you may be
#' biasing some analyses in favor of those multiple-reference profile unless
#' you check and account such cases.
#' @references
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#' @examples \dontrun{
#' x <- rsp_profile(8833, 8850)
#' plot(x)}

#NOTES
#######################

# 0.3. notes
# went from sp_profile to rsp_profile (and rsp)
# dropped code argument
# using as.respeciate in generics to build rsp object


#to think about
#######################

#add functions to build or add respeciate-like data of own,
#              e.g. x matrices for pls modelling

#  (build functions started as separate script, rsp.build.R)

## rsp_import_profile to import a profile from an external source
##     extension of above to import data from specific sources
##           might be very code intensive..?

## local function to pad data using database???

#' @rdname rsp
#' @export
##     (now importing via xxx.r)
##     #' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

#####################
#to think about
#####################
# not sure but I think something in the main build:
#    (default; include.refs = FALSE)
#    PROFILES>>SPECIES>>SPECIES_PROPERTIES
#    (full build; include.refs = TRUE)
#    PROFILES>>SPECIES>>SPECIES_PROPERTIES>>PROFILE_REFERENCE>>REFERENCES
# is replicating profiles.
#

#v 0.2
#   based on previous sp_profile but using data.table
#   (0.1 version currently unexported sp_profile.old)

rsp_profile <- function(..., include.refs=FALSE) {

  # code currently handles:
  # respeciate.ref, data.frames containing profile_code,
  # numerics and characters

  #######################
  #could replace code AND ... with just ...???
  #   but would need to think about options
  #   if any in ... were data.frames
  ######################
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

  ################
  #previous....
  ################
  #if(is.data.frame(code) && "PROFILE_CODE" %in% names(code)){
  #  code <- unique(code$PROFILE_CODE)
  #}
  #if(is.numeric(code)) code <- as.character(code)
  #if(!is.character(code)) {
  #  stop("unexpected 'code' class",
  #       call.=FALSE)
  #}

  PROFILES <- data.table::as.data.table(sysdata$PROFILES)
  SPECIES <- data.table::as.data.table(sysdata$SPECIES)
  SPECIES_PROPERTIES <- data.table::as.data.table(sysdata$SPECIES_PROPERTIES)
  PROFILE_REFERENCE <- data.table::as.data.table(sysdata$PROFILE_REFERENCE)
  REFERENCES <- data.table::as.data.table(sysdata$REFERENCES)

  ##########################
  #testing tolower below
  #   as a fix for code arg case sensitivity
  ##########################
  #  could test replacing some of this with sp_pad???
  #      IF sp_pad stays
  dt <- PROFILES[tolower(PROFILES$PROFILE_CODE) %in% tolower(code),]
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
  x <- as.data.frame(dt)
  if("WEIGHT_PERCENT" %in% names(x) & !".value" %in% names(x)) {
    x$.value <- x$WEIGHT_PERCENT
  }

  # note
  ######################################

  #dropping generic unexported rsp_build_respeciate(x)
  #    replacing with as.respeciate
  #    could do similar elsewhere if not used widely elsewhere ???

  #output
  rsp <- as.respeciate(x, test.rsp=FALSE)
  return(rsp)
}

#' @rdname rsp
#' @export
rsp <- function(...) { rsp_profile(...) }







