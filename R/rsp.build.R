#' @name rsp.build
#' @title Building respeciate-like Objects
#' @aliases rsp_build_x


#' @description rsp function(s) to reconfigure data.frames (and similar
#' object classes) for use with data and functions in re(SPECIATE).

#' @param x \code{data.frame} or similar (i.e.
#' something that can be coerced into a \code{data.frame} using
#' \code{as.data.frame}) to be converted into a \code{respeciate} object.
#' @param profile_name,profile_code (\code{character}) The name of the column
#' in \code{x} containing profile name and code records, respectively. If not
#' already named according to SPECIATE conventions, at least one of these will
#' need to be assigned.
#' @param species_name,species_id (\code{character}) The name of the column
#' in \code{x} containing species name and id records, respectively. If not
#' already named according to SPECIATE conventions, at least one of these will
#' need to be assigned.
#' @param value (\code{character})  The name of the column in \code{x}
#' containing measurement values. If not already named according to SPECIATE
#' conventions, this will need to be assigned.
#' @return \code{rsp_build}s attempt to build and return a (re)SPECIATE-like
#' object that can be compared with data from re(SPECIATE).
#' @note If you want to compare your data with profiles in the SPECIATE archive,
#' you need to use EPA SPECIATE conventions when assigning species names and
#' identifiers. Currently, we are working on options to improve on this (and
#' very happy to discuss if anyone has ideas), but current best suggestion is:
#' (1) identify the SPECIATE species code for each of the species in your data set,
#' and (2) assign these as \code{species_id} when \code{rsp_build}ing. The
#' function will then associate the \code{species_name} from SPECIATE species
#' records.

#NOTES
#######################

#to think about
#######################

## sp_build_profile to make a profile locally
##     needs profile_name, profile_code
##           species_name, species_id
##           weight_percent (and possibly .value)

##############################
# sp_build_rsp_x
##############################

# notes
##############################

# 0.3. notes
# went from sp_build_rsp_x to rsp_build_x
# using as.respeciate and adding rsp_x


# rsp_build_x currently converts x as.data.frame(x)
#     if tibble is loaded, any tibbles complicate things

#     BUT might want to revisit this because it looked like:
#           the data structure was fine but
#           print.respeciate was having problems...

#           BUT might be other problems I did not spot

#           BUT be nice if c("respeciate", class("tibble")) could be used...
#               to retain the data type history
#               and drop back to tibble rather than data.frame....


#' @rdname sp.build
#' @export

rsp_build_x <-
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

    #pass via as.speciate to build rsp_x
    #   note: this replaces previous local testing
    x <- as.respeciate(x, test.rsp=TRUE)
    #slip in rsp_x tag
    class(x) <- unique(c("rsp_x", class(x)))
    x
  }










