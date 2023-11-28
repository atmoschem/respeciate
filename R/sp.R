#' @name sp
#' @title sp_ functions
#' @aliases sp_profile sp_build_rsp_x


#' @description sp function to get profiles from the R (re)SPECIATE archive

#' @description \code{\link{sp_profile}} extracts a
#' SPECIATE profile from the local (re)SPECIATE archive.
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
#' @param x (for \code{sp_build}s only) A \code{data.frame} or similar (i.e.
#' something that can be converted to a \code{data.frame} using
#' \code{as.data.frame}) to be converted into a \code{respeciate} object for
#' comparison with SPECIATE profiles.
#' @param profile_name,profile_code (for \code{sp_build}s only;
#' \code{character})  The name of the column in \code{x} containing
#' profile name and code, respectively. If not already named according
#' to SPECIATE conventions, at least one of these will need to be assigned.
#' @param species_name,species_id (for \code{sp_build}s only;
#' \code{character})  The name of the column in \code{x} containing
#' species name and id, respectively. If not already named according
#' to SPECIATE conventions, at least one of these will need to be assigned.
#' @param value (for \code{sp_build}s only; \code{character})  The name
#' of the column in \code{x} containing measurement values. If not already
#' named according to SPECIATE conventions, this will need to be assigned.
#' @return \code{sp_profile} returns a object of
#' \code{respeciate} class, a \code{data.frame} containing a
#' (re)SPECIATE profile.
#'
#' \code{sp_build}s attempt to build and return a (re)SPECIATE-like profile
#' that can be compared with with data in re(SPECIATE).
#' @note With \code{sp_profile}:
#'
#' The option \code{include.refs} adds profile source reference
#' information to the returned \code{respeciate} data set. The default option
#' is to not include these because some profiles have several associated
#' references and including these replicates records, once per reference.
#' \code{respeciate} code is written to handle this but if you are developing
#' own methods or code and include references in any profile build you may be
#' biasing some analyses in favor of those multiple-reference profile unless
#' you check and account such cases.
#'
#' With \code{sp_build}s:
#'
#' It is particularly IMPORTANT that you use EPA SPECIATE conventions when
#' assign species information if you want to compare your data with SPECIATE
#' profiles. Currently, working on option to improve on this (and very happy
#' to discuss if anyone has ideas), but current best suggestion is: (1)
#' identify the SPECIATE species code for all the species in your data set,
#' and (2) assign these as \code{species_id} when \code{sp_build}ing. The
#' function will then associate the \code{species_name}.
#'
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

#add functions???

## sp_build_profile to make a profile locally
##     needs profile_name, profile_code
##           species_name, species_id
##           weight_percent (and possibly .value)

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
  df <- PROFILES[tolower(PROFILES$PROFILE_CODE) %in% tolower(code),]
  df <- merge(df, SPECIES, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE,
              allow.cartesian=TRUE)
  df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID", all.y=FALSE,
              all.x=TRUE, allow.cartesian=TRUE)
  if(include.refs){
    df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE", all.y=FALSE,
                all.x=TRUE, allow.cartesian=TRUE)
    df <- merge(df, REFERENCES, by = "REF_Code", all.y=FALSE, all.x=TRUE,
                allow.cartesian=TRUE)
  }
  df <- df[order(df$PROFILE_CODE, decreasing = FALSE),]

  #build
  #note: currently adding .value in rsp_build_respeciate
  #      could do it here?
  #          leaving there for now... because we would
  #          still have to do it there for self-build or
  #          imported profiles...
  df <- rsp_build_respeciate(as.data.frame(df))
  return(df)
}



##############################
# sp_build_rsp_x
##############################

# notes
##############################

# sp_build_rsp_x currently converts x as.data.frame(x)
#     if tibble is loaded, any tibbles complicate things

#     BUT might want to revisit this because it looked like:
#           the data structure was fine but
#           print.respeciate was having problems...

#           BUT might be other problems I did not spot

#           BUT be nice if c("respeciate", class("tibble")) could be use...
#               to retain the data type history
#               and drop back to tibble rather than data.frame....


#' @rdname sp
#' @export

sp_build_rsp_x <-
  function(x, profile_code, profile_name,
           species_name, species_id,
           value, ...){


    # light build for a rsp_x data object
    # might need spec_mwt

    ###########################
    # current build rules
    ###########################

    # must be a data.frame or something that can be converted
    #        using as.data.frame(x)

    # profile and species columns must be character...

    # profile_name:  if not there, if sent in call use,
    #                              else if there use profile_code
    # profile_code:  if not there, if sent in call use,
    #                              else if there use profile_name
    # species_name:  if not there, if sent in call use,
    #                              else if there use use species_id to look-up
    #                              if any missing, warn
    # species_id:    if not there, if sent in call use,
    #                              else if there use species_name to look-up
    #                              if any missing, warn
    # .value:        if not there, if sent in call use.
    #                              (NEW/TESTING) else if there use WEIGHT_PERCENT
    # WEIGHT_PERCENT:if not there, if sent in call use
    #                              else if there use .value to look-up

    # don't build/error if any of these missing and end of build

    # redundant?
    # currently not using ...
    .x.args <- list(...)

    #adding the as.data.frame because
    #    code is not planning nicely with Dennis' tibbles
    #        if tibble is loaded before respeciate...
    x <- as.data.frame(x)

    #rationalise this?...
    #    could they be else options when
    #        check for species and profile columns?
    ################################
    # notes
    # profile and species columns all need to character
    #    user could supply any thing  and previously
    #        only applying as.character when making something new...
    #    else may at start then when making something new...
    #    (at end did not work for species if building one of species_name
    #         and species_id from other...)
    # also
    #    do values need to be as.numeric???
    if("PROFILE_NAME" %in% names(x)){
      x$PROFILE_NAME <- as.character(x$PROFILE_NAME)
    }
    if("PROFILE_CODE" %in% names(x)){
      x$PROFILE_CODE <- as.character(x$PROFILE_CODE)
    }
    if("SPECIES_NAME" %in% names(x)){
      x$SPECIES_NAME <- as.character(x$SPECIES_NAME)
    }
    if("SPECIES_ID" %in% names(x)){
      x$SPECIES_ID <- as.character(x$SPECIES_ID)
    }

    #if not there and sent in call

    #note:
    #current making all BUT values, character class
    if(!"PROFILE_NAME" %in% names(x) & (!missing(profile_name))){
      if(!profile_name %in% names(x)){
        stop("sp_build> '", as.character(profile_name)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$PROFILE_NAME <- as.character(x[, profile_name])
    }
    if(!"PROFILE_CODE" %in% names(x) & (!missing(profile_code))){
      if(!profile_code %in% names(x)){
        stop("sp_build> '", as.character(profile_code)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$PROFILE_CODE <- as.character(x[, profile_code])
    }
    if(!"SPECIES_NAME" %in% names(x) & (!missing(species_name))){
      if(!species_name %in% names(x)){
        stop("sp_build> '", as.character(species_name)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$SPECIES_NAME <- as.character(x[, species_name])
    }
    if(!"SPECIES_ID" %in% names(x) & (!missing(species_id))){
      if(!species_id %in% names(x)){
        stop("sp_build> '", as.character(species_id)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$SPECIES_ID <- as.character(x[, species_id])
    }
    if(!".value" %in% names(x)){
      if(missing(value)){
        if("WEIGHT_PERCENT" %in% names(x)){
          x$.value <- x[, "WEIGHT_PERCENT"]
        } else {
          stop("sp_build> 'value' not found for 'x'...",
               sep="", call. = FALSE)
        }
      } else {
        if(!value %in% names(x)){
           stop("sp_build> '", as.character(value)[1],
                "' not in 'x'...", sep="", call. = FALSE)
        }
      }
      x$.value <- x[, value]
    }
    #################
    #old
    #################
    #if(!".value" %in% names(x) & (!missing(value))){
    #  if(!value %in% names(x)){
    #    stop("sp_build> '", as.character(value)[1],
    #         "' not in 'x'...", sep="", call. = FALSE)
    #  }
    #  x$.value <- x[, value]
    #}

    #if still not there try to assign using what is there

    if("PROFILE_NAME" %in% names(x) & !"PROFILE_CODE" %in% names(x)){
      x$PROFILE_CODE <- x$PROFILE_NAME
    }
    if("PROFILE_CODE" %in% names(x) & !"PROFILE_NAME" %in% names(x)){
      x$PROFILE_NAME <- x$PROFILE_CODE
    }
    test <- c("SPECIES_NAME", "SPECIES_ID")[c("SPECIES_NAME", "SPECIES_ID")
                                            %in% names(x)]
    if(length(test)==1){
      #one there, other as look-up
      .tmp <- data.table::as.data.table(
        sysdata$SPECIES_PROPERTIES[c("SPECIES_NAME", "SPECIES_ID")]
      )
      .tmp$SPECIES_NAME <- as.character(.tmp$SPECIES_NAME)
      .tmp$SPECIES_ID <- as.character(.tmp$SPECIES_ID)
      x <- merge(data.table::as.data.table(x),
                 data.table::as.data.table(.tmp),
                 all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)
      x <- as.data.frame(x)
    }
    if(".value" %in% names(x) & !"WEIGHT_PERCENT" %in% names(x)){
      x$WEIGHT_PERCENT <- x$.value
    }

    #test what we have
    ########################

    .test <- c("PROFILE_NAME", "PROFILE_CODE", "SPECIES_NAME", "SPECIES_ID",
               ".value", "WEIGHT_PERCENT")
    .test <- .test[!.test %in% names(x)]
    if(length(.test)>0){
      stop("sp_build> bad data structure, expected column(s) missing/unassigned:\n",
           paste(.test, sep="", collapse = ", "), "\n", sep="", call.=FALSE)
    }
    if(any(is.na(x$SPECIES_ID)) | any(is.na(x$SPECIES_NAMES))){
      warning("sp_build> suspect species data, values missing:\n",
              "(respeciate needs valid species entries)\n",
              sep="", call.=FALSE)
    }

    #output
    ######################

    x <- as.data.frame(x)
    class(x) <- c("rsp_x", "respeciate", "data.frame")
    x
  }













#############################
#unexported & previous code
#############################

#sp_profile v 0.1
#now unexported

rsp_profile.old <- function(code) {
  #handle numerics/characters
  #######################
  #could replace code with ...???
  ######################
  if(class(code)[1] == "respeciate" && "PROFILE_CODE" %in% names(code)){
    code <- unique(code$PROFILE_CODE)
  }
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  #handle multiple codes
  ############################
  #replace previous lapply with a direct %in%
  ##  df <- lapply(code, function(x){
  ##    df <- PROFILES[PROFILES$PROFILE_CODE == x, ]
  ##    ...
  ##  })
  ##  df <- do.call(rbind, df)
  #testing as sp_profile.2
  #faster with data.table
  ############################
    df <- PROFILES[PROFILES$PROFILE_CODE %in% code,]
    df <- merge(df, SPECIES, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE)
    df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID", all.y=FALSE, all.x=TRUE)
    df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE)
    df <- merge(df, REFERENCES, by = "REF_Code", all.y=FALSE, all.x=TRUE)
    df <- df[order(df$PROFILE_CODE, decreasing = FALSE),]
##  })
  #build
  df <- rsp_build_respeciate(df)
  return(df)
}



