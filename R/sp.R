#' @name sp
#' @title sp_profile
#' @aliases sp_profile


#' @description sp function to get profile(s) from the R (re)SPECIATE archive

#' @param code character, numeric or data.frame, the SPECIATE code
#' of the required profile (EPA SPECIATE identifier PROFILE_CODE). This is
#' typically one or concatenated character or numeric entries, but can also
#' be a \code{respeciate} object or similar \code{data.frame} containing
#' the \code{code}s as a named \code{PROFILE_NAME} column.
#' @param ... additional arguments, ignored except by \code{sp_profile} which
#' treats these as additional sources for \code{code}.
#' @param include.refs logical, (for \code{sp_profile} only) include profile
#' reference information when getting the requested profile(s) from the
#' archive, default \code{FALSE}.
#' @return \code{sp_profile} returns a object of
#' \code{respeciate} class, a \code{data.frame} containing a
#' (re)SPECIATE profile.
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
#' x <- sp_profile(c(8833, 8850))
#' plot(x)}

#NOTES
#######################

#to think about
#######################

#add functions to build or add respeciate-like data of own,
#              e.g. x matrices for pls modelling

#  (build functions started as separate script, sp.build.R)

## sp_import_profile to import a profile from an external source
##     extension of above to import data from specific sources
##           might be very code intensive..?

## local function to pad data using database???

#' @rdname sp
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

sp_profile <- function(code, ..., include.refs=FALSE) {

  # code currently handles:
  # respeciate.ref, data.frames containing profile_code,
  # numerics and characters

  #######################
  #could replace code AND ... with just ...???
  #   but would need to think about options
  #   if any in ... were data.frames
  ######################
  .try <- lapply(list(code, ...), function(.code){
    if(is.data.frame(.code) && "PROFILE_CODE" %in% names(.code)){
      .code <- unique(.code$PROFILE_CODE)
    }
    if(is.numeric(.code)) {
      .code <- as.character(.code)
    }
    if(!is.character(.code)) {
      warning("unexpected 'code' object found and ignored",
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
  if("WEIGHT_PERCENT" %in% names(x)) {
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








