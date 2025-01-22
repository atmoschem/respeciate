#' @name rsp.build
#' @title Building respeciate-like Objects
#' @aliases rsp_build_x rsp_build_simx


#' @description rsp function(s) to reconfigure data.frames (and similar
#' object classes) for use with data and functions in \code{respeciate}.

#' @param x \code{data.frame} or similar (i.e.
#' something that can be coerced into a \code{data.frame} using
#' \code{as.data.frame}) to be converted into a \code{respeciate} object.
#' @param profile_name,profile_id (\code{character}) The names of the columns
#' in \code{x} containing profile names and identifiers, respectively. If not
#' already named according to \code{respeciate} conventions, at least one
#' of these will need to be assigned.
#' @param species_name,species_id (\code{character}) The names of the columns
#' in \code{x} containing species name and identifiers, respectively. If not
#' already named according to \code{respeciate} conventions, at least one of
#' these will need to be assigned.
#' @param value (\code{character}) The name of the column in \code{x}
#' containing measurement values. If not already named according to \code{respeciate}
#' conventions, this will need to be assigned.
#' @param ... (any other arguments) currently ignored.
#' @param m \code{respeciate} data set of source profiles intended to be used
#' as the source profiles (or M) matrix when building a simulated data set for
#' use with a PLS model (see \code{rsp_pls_x})
#' @param n a numeric object, e.g. a \code{vector}, \code{matrix}, \code{data.frame} or
#' a similar object that can be coerced into a \code{data.frame} of suitable
#' dimensions for use as the source strength matrix (N) to build a simulated data set for
#' use with a PLS model (see \code{rsp_pls_x}).
#' @return \code{rsp_build}s attempt to build and return a \code{respeciate}-like
#' object that can be directly compared with data from \code{respeciate}.
#'
#' \code{rsp_build_x} is the standard object builder.
#'
#' \code{rsp_build_simx} builds a simulation of an \code{x} data set based on
#' the `linear combination of profiles` model applied in conventional source
#' apportionment. (See below and \code{rsp_pls_x})
#'
#' @note If you want to compare your data with profiles in the \code{respeciate} archive,
#' you need \code{respeciate} conventions when assigning species names and
#' identifiers. We are working on options to improve on this (and
#' very happy to discuss if anyone has ideas), but current best suggestion is:
#' (1) identify the \code{respeciate} species code for each of the species in
#' your data set, and (2) assign these as \code{species_code} when \code{rsp_build}ing.
#' The function will then associate the \code{species_name} from \code{respeciate}
#' species records.

#NOTES
#######################

#to think about
#######################

## think this needs more/better documentation

#     but only me using it at moment

## think about a rsp_build_profile to make a profile locally

#     would need profile_name and/or profile_id
#         species_name and/or species_id
#         value (like .value(rsp),
#                 weight_percent(SPECIATE) or
#                 relative.mass (SPECIEUROPE)

#    maybe something like
#         .rsp_get_m_from_pls (unexported)

## rsp_build_sim_x

#     status: in-development (below)

#     to simulate a rsp_x data set
#         .rsp_build_sim_x (m, n, err, ...)
#               m - (or rsp) a set of profiles
#               n - something that can be built into an n matrix
#               err - a set of error that can be applied to the model
#         this is simulation studies with the pls functions
#

##############################
# rsp_build_x
##############################

# notes
##############################

# 0.3. notes
# went from sp_build_rsp_x to rsp_build_x
# using as.respeciate and adding rsp_x subclass
#       now searches ..rsp_species_meta()[c(".species", ".species.id")
#           for the .species.id to .species mapping

# rsp_build_x currently converts x to data.frame (as.data.frame(x))
#     if tibble is loaded, tibbles currently complicates things...
#     BUT might want to revisit this because it looked like:
#           the data structure was fine but
#           print.respeciate was having problems...
#           BUT might be other problems I did not spot
#           BUT be nice if c("respeciate", class("tibble")) could be used...
#               to retain the data type history
#               and drop back to tibble rather than data.frame....


#' @rdname rsp.build
#' @export

rsp_build_x <-
  function(x, profile_id, profile_name,
           species_name, species_id,
           value, ...){

    # light build for a rsp_x data object
    # might want spec_mwt
    #        (could map from ..rsp_species_meta)

    ###########################
    # current build rules
    ###########################

    # must be a data.frame or something that can be converted
    #        using as.data.frame(x)

    # profile and species columns must be character...

    # profile_name:  if not there, if sent in call use,
    #                              else if there use profile_id
    # profile_id:  if not there, if sent in call use,
    #                              else if there use profile_name
    # species_name:  if not there, if sent in call use,
    #                              else if there use use species_id to look-up
    #                              if any missing, warn
    # species_id:    if not there, if sent in call use,
    #                              else if there use species_name to look-up
    #                              if any missing, warn
    # .value:        if not there, if sent in call use.
    #                              (NEW/TESTING) else if there use .pc.weight
    # .pc.weight:if not there, if sent in call use
    #                              else if there use .value to look-up

    # should error if any of these missing at end of build

    # redundant?
    # currently only using to turn warning off...
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
    if(".profile" %in% names(x)){
      x$.profile <- as.character(x$.profile)
    }
    if(".profile.id" %in% names(x)){
      x$.profile.id <- as.character(x$.profile.id)
    }
    if(".species" %in% names(x)){
      x$.species <- as.character(x$.species)
    }
    if(".species.id" %in% names(x)){
      x$.species.id <- as.character(x$.species.id)
    }

    #if not there and sent in call

    #note:
    #current making all BUT values, character class
    if(!".profile" %in% names(x) & (!missing(profile_name))){
      if(!profile_name %in% names(x)){
        stop("rsp_build> '", as.character(profile_name)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$.profile <- as.character(x[, profile_name])
    }
    if(!".profile.id" %in% names(x) & (!missing(profile_id))){
      if(!profile_id %in% names(x)){
        stop("rsp_build> '", as.character(profile_id)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$.profile.id <- as.character(x[, profile_id])
    }
    if(!".species" %in% names(x) & (!missing(species_name))){
      if(!species_name %in% names(x)){
        stop("rsp_build> '", as.character(species_name)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$.species <- as.character(x[, species_name])
    }
    if(!".species.id" %in% names(x) & (!missing(species_id))){
      if(!species_id %in% names(x)){
        stop("rsp_build> '", as.character(species_id)[1],
             "' not in 'x'...", sep="", call. = FALSE)
      }
      x$.species.id <- as.character(x[, species_id])
    }
    if(!".value" %in% names(x)){
      if(missing(value)){
        if(".pc.weight" %in% names(x)){
          x$.value <- x[, ".pc.weight"]
        } else {
          stop("rsp_build> 'value' not found for 'x'...",
               sep="", call. = FALSE)
        }
      } else {
        if(!value %in% names(x)){
           stop("rsp_build> '", as.character(value)[1],
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

    if(".profile" %in% names(x) & !".profile.id" %in% names(x)){
      x$.profile.id <- x$.profile
    }
    if(".profile.id" %in% names(x) & !".profile" %in% names(x)){
      x$.profile <- x$.profile.id
    }
    test <- c(".species", ".species.id")[c(".species", ".species.id")
                                            %in% names(x)]
    if(length(test)==1){
      #one there, other as look-up
      .tmp <- data.table::as.data.table(
        ..rsp_species_meta()[c(".species", ".species.id")]
      )
      .tmp$.species <- as.character(.tmp$.species)
      .tmp$.species.id <- as.character(.tmp$.species.id)
      x <- merge(data.table::as.data.table(x),
                 data.table::as.data.table(.tmp),
                 all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)
      x <- as.data.frame(x)
    }
    if(".value" %in% names(x) & !".pc.weight" %in% names(x)){
      x$.pc.weight <- x$.value
    }

    #pass via as.speciate to build rsp_x
    #   note: this replaces previous local testing
    test.rsp <- if("test.rsp" %in% names(.x.args)){
      .x.args$test.rsp
    } else {
      TRUE
    }
    x <- as.respeciate(x, test.rsp=test.rsp)
    #slip in rsp_x tag
    class(x) <- unique(c("rsp_x", class(x)))
    x
  }






#################################
# rsp_build_simx
#################################

#use: build simulation dat for pls demo...

# notes
###############################

# might not keep current arguments

# m from PLS model typically reference data set to use to build simulation from
#    for example rsp object of one or more profiles
# n something that can be scaled to make an n matrix for a PLS model
# ... others ??? to be decided...

# in development
################################

#########################
#current example
#########################

# devtools::unload(); devtools::load_all()
# m <- rsp("US:8992VBS", "US:8996VBS")
# x <- rsp_build_simx(m, c(1,2,3,3,2,1))
##  US:8992VBS US:8996VBS .sample
##1          1          3       1
##2          2          2       2
##3          3          1       3
# mod <- rsp_pls_x(x, m)
# pls_plot_species(mod, id=1:29)

# to do/sort
#####################

# add formal args for errs
#      could be like n but errors to go with x ???
#      BUT might also need a function to apply the errors???

# decide to show/not show the data.table/frame ??
#      option to show, option to keep ???

# decide how to try

# think about issue with dt[,.(sum)]
#      seemed to give ans + 1 but only SOMETIMES

# decide what to do about warning for perfect models (like above)
#      get with pls_plot_species(mod, id=1:29)
#         but maybe source is one of the pls_functions ???

# expand docs, build proper links and link to rsp_pls_x

# intending using this for trivial example


#' @rdname rsp.build
#' @export

rsp_build_simx <-
  function(m, n=1, ...){
    # set up
    .xargs <- list(...)
    .pro <- unique(m$.profile.id)
    #n
    # can be vector, matrix or data.frame
    # maybe better way of doing this??
    if(is.numeric(n) & is.vector(n)){
      .tmp <- .xargs[names(.xargs) %in% names(formals(matrix))]
      .tmp$data <- n
      .tmp$ncol <- length(.pro)
      n <- do.call(matrix, .tmp)
    }
    #handling if
    if(is.matrix(n)){
      n <- as.data.frame(n)
      names(n) <- .pro
      #row.names(n) <- paste("SIM", 1:nrow(n), sep="")
    }
    if(!is.data.frame(n)){
       stop("rsp_build> can't convert 'n' to data.frame",
            call.=FALSE)
    }
    if(ncol(n)!=length(.pro)){
      stop("rsp_build> 'n' wrong dimensions",
           call.=FALSE)
    }
    names(n) <- .pro
    n$.sample <- row.names(n)

    print(n)
    #rearrange n to merge with m
    n <- data.table::melt.data.table(data.table::as.data.table(n),
                                     variable.name = ".profile.id",
                                     value.name = ".load",
                                     id.var=".sample")
    out <- data.table::merge.data.table(
      data.table::as.data.table(m), data.table::as.data.table(n),
      by=".profile.id", allow.cartesian=TRUE
    )
    #rescale .value (x$.value = m$.value * n$.load)
    if(".value" %in% names(out)){
      out <- out[, .value:=.value*.load]
    }
    #might not want next update...
    #if(".pc.weight" %in% names(out)){
    #  out$.pc.weight <- out$.pc.weight * out$.load
    #}
    out <- out[, .(
      .species = .species[1],
      #.cheat = paste(.value, collapse="+"),
      #.v2 = sum(.value, na.rm = TRUE),
      # this seemed to be reporting [right.answer]+1 during testing
      .value = sum(c(.value[!is.na(.value)],0))
    ), by=c(".species.id", ".sample")]
    #tidy to output
    out <- as.data.frame(out)
    out$.profile.id <- out$.sample
    out <- out[names(out)[names(out) %in% c(".species", ".species.id",
                                            ".profile.id", ".value")]]
    #output as rsp_x
    #  this currently fixes some missing columns
    rsp_build_x(out)
  }




