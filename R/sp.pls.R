#' @name sp.pls
#' @title (re)SPECIATE profile Positive Least Squares
#' @aliases sp_pls_profile pls_report pls_test pls_fit_species
#' pls_refit_species pls_rebuild pls_plot
#' pls_plot_species pls_plot_profile

#' @description Functions for Positive Least Squares (PSL) fitting of
#' (re)SPECIATE profiles

#' @description
#' \code{sp_pls_profile} builds PSL models for supplied profile(s) using
#' the \code{\link{nls}} function, the 'port' algorithm and a lower
#' limit of zero for all model outputs to enforce the positive fits. The
#' modeled profiles are typically from an external source, e.g. a
#' measurement campaign, and are fit as a linear additive series of reference
#' profiles, here typically from (re)SPECIATE, to provide a measure of
#' source apportionment based on the assumption that the profiles in the
#' reference set are representative of the mix that make up the modeled
#' sample. The \code{pls_} functions work with \code{sp_pls_profile}
#' outputs, and are intended to be used when refining and analyzing
#' these PLS models.

#' @param x A \code{respeciate} object, a \code{data.frame} of
#' profiles in standard long form, intended for PLS modelling.
#' @param ref A \code{respeciate} object, a \code{data.frame} of
#' profiles also in standard long form, used as the set of candidate
#' source profiles when fitting \code{x}.
#' @param power A numeric, an additional factor to be added to
#' weightings when fitting the PLS model. This is applied in the form
#' \code{weight^power}, and increasing this, increases the relative
#' weighting of the more heavily weighted measurements. Values in the
#' range \code{1 - 2.5} are sometimes helpful.
#' @param ... additional arguments, typically ignored or passed on to
#' \code{\link{nls}}.
#' @param pls A \code{sp_pls_profile} output, only used by \code{pls_}
#' functions.
#' @param species for \code{pls_fit_species}, a data.frame of
#' measurements of an additional species to be fitted to an existing
#' PLS model, or for \code{pls_refit_species} a character vector of the
#' names of species already included in the model to be refit. Both are
#' multiple-\code{species} wrappers for \code{pls_rebuild}, a general-purpose
#' PLS fitter than only handles single \code{species}.
#' @param refit.profile (for \code{pls_fit_species}, \code{pls_refit_species}
#' and \code{pls_rebuild}) logical. When fitting a new \code{species} (or
#' refitted an existing \code{species}), all other species in the reference
#' profiles are held 'as is' and added \code{species} is fit to the source
#' contribution time-series of the previous PLS model. By default, the full PLS
#' model is then refit using the revised \code{ref} source profile to generate
#' a PLS model based on the revised source profiles (i.e., ref + new species
#' or ref + refit species). However, this second step can be omitted using
#' \code{refit.profile=FALSE} if you want to use the supplied \code{species}
#' as an indicator rather than a standard member of the apportionment model.
#' @param as.marker for \code{pls_rebuild}, \code{pls_fit_species} and
#' \code{pls_refit_species}, \code{logical}, default \code{FALSE}, when
#' fitting (or refitting) a species, treat it as source marker.
#' @param drop.missing for \code{pls_rebuild}, \code{pls_fit_species} and
#' \code{pls_refit_species}, \code{logical}, default \code{FALSE}, when
#' building or rebuilding a PLS model, discard cases where \code{species}
#' is missing.
#' @param n (for \code{pls_plot}s only) numeric or character
#' identifying the species or profile to plot. If numeric, these are treated
#' as indices of the species or profile, respectively, in the PLS model; if
#' character, species is treated as the name of species and profile is treated
#' as the profile code. Both can be concatenated to produce multiple plots and
#' the special case \code{n = -1} is a short cut to all species or profiles,
#' respectively.
#' @param type (for \code{pls_plot}s only) numeric, the plot type if
#' multiple options are available.
#' @param log (for \code{pls_plot_profile} only) logical, if \code{TRUE} this
#' applies 'log' scaling to the primary Y axes of the plot.

#########################
# need to check terminology for this...
#      The zero handling is a based on offset in plot(..., log="y", off.set)
#      but automatically estimated...

#' @return \code{sp_pls_profile} returns a list of nls models, one per
#' profile/measurement set in \code{x}. The \code{pls_} functions work with
#' these outputs. \code{pls_report} generates a \code{data.frame} of
#' model outputs, and is used of several of the other \code{pls_}
#' functions. \code{pls_fit_species}, \code{pls_refit_species} and
#' \code{pls_fit_parent} return the supplied \code{sp_pls_profile} output,
#' updated on the basis of the \code{pls_} function action.
#' \code{pls_plot}s produce various plots commonly used in source
#' apportionment studies.

#' @note This implementation of PLS applies the following modeling constraints:
#'
#' 1. It generates a model of \code{x} that is positively constrained linear
#' product of the profiles in \code{ref}, so outputs can only be
#' zero or more.  Although the model is generated using \code{\link{nls}},
#' which is a Nonlinear Least Squares (NLS) model, the fitting term applied
#' in this case is linear.
#'
#' 2. The number of species in \code{x} must be more that the number of
#' profiles in \code{ref} to reduce the likelihood of over-fitting.
#'
#'

# GENERAL NOTES

# TO DO
# link to CMB as crude form of CMB and reference?

# these all need code tidying

# check individual function notes


############################
############################
## sp_pls_profile
############################
############################

#' @rdname sp.pls
#' @export

##   now importing locally where possible
##   data.table::[function]
##   #' @import data.table

#This is version 2

#version 1 combined version2 and pls_report
#now separated because it simplified pls model reworking

#currently keeping the function args
#   might not need to do this BUT
#   model does not seem to be tracking them when ...

# check power handling is right

sp_pls_profile <- function(x, ref,
                            power = 1,
                            ...){

  ##################
  #from rough code
  ##################

  ########################
  #only allowing profiles < species
  if(length(unique(ref$PROFILE_CODE)) >= length(unique(x$SPECIES_ID))){
    stop("sp_pls: #.need species > #.profiles, more species or less profiles?",
         call. = FALSE)
  }

  x.args <- list(...)

  ####################
  #make sure we only have one species / profile
  ####################
  #tidying
  .pr.cd <- unique(x$PROFILE_CODE)
  ##  .xx <- respeciate:::rsp_tidy_profile(x)
  .xx <- lapply(.pr.cd, function(y){
    .x <- x[x$PROFILE_CODE==y,]
    .x <- sp_average_profile(.x, y, .x$PROFILE_NAME[1])
    .x
  })
  .xx <- data.table::rbindlist(.xx)
#############################
#currently just dropping them
#can't fit negatives
  .xx <- .xx[.xx$.value >= 0, ]
  .xx <- .xx[!is.na(.xx$.value),]
#############################
  #should be same! redundant
  .pr.cd <- unique(.xx$PROFILE_CODE)

  ####################
  #reduce ref to just species in x
  ###################
  #no point to look at any species not in x
  ref <- subset(ref, SPECIES_ID %in% unique(.xx$SPECIES_ID))

  ###################
  #nudge
  ###################
  #dropping nudge from version 2
  ##
  #nb: method was nudge before analysis
  #and a nudge back after
  #   nudge(identified.species)->pls->report->nudge back(identified.species)

  #if(!is.null(nudge)){
  #  for(i in nudge){
  #    #ref might have both WEIGHT_PERCENT and .value
  #    ref[ref$SPECIES_NAME==i, "WEIGHT_PERCENT"] <-
  #      ref[ref$SPECIES_NAME==i, "WEIGHT_PERCENT"] * 10
  #    .xx[.xx$SPECIES_NAME==i, "WEIGHT_PERCENT"] <-
  #      .xx[.xx$SPECIES_NAME==i, "WEIGHT_PERCENT"] * 10
  #    .xx[.xx$SPECIES_NAME==i, ".value"] <-
  #      .xx[.xx$SPECIES_NAME==i, ".value"] * 10
  #  }
  #}

  ##############################
  #main step/ once per profile
  ##############################
  #can we replace this with data.table
  ans <- lapply(.pr.cd, function(y){
    .test <- try({
      #need to try this because it does not always work
      .x <- as.data.frame(.xx[.xx$PROFILE_CODE==y,])
      .x <- sp_average_profile(.x, "test", "1_test")

      #might not need one of this-and-same-above
      #might be better doing it here...
      .tmp <- subset(ref, ref$SPECIES_ID %in% unique(.x$SPECIES_ID))

      #could change this with rbindlist version??
      .ref <- intersect(names(.x), names(.tmp))
      .out <- rbind(.x[.ref], .tmp[.ref])
      .out <- sp_dcast_profile(.out)

      #build formula and model args
      .tmp <- names(.out)
      .tmp <- .tmp[!.tmp %in% c("SPECIES_ID", "SPECIES_NAME", "test")]
      #zero cases for port function
      .ls <- paste("m_", .tmp, sep="")
      .ls2 <- lapply(.ls, function(x){0})
      names(.ls2) <- .ls
      .for <- paste("(m_", .tmp, "*`", .tmp, "`)", sep="", collapse = "+")
      .for <- as.formula(paste("test~", .for))
      .wt <- 1/.out$test
      ############################
      #note
      ############################
      #nls wants lower and upper as vectors
      #but seems to handle lists
      #   should check how this is done?
      #   might not translate sesnibly...
      #   pass upper, default INF???

      .out[is.na(.out)] <- 0  #testing

      args <- list(formula = .for,
                   data=.out,
                   start=.ls2,
                   lower=.ls2,
                   weights=.wt,
                   algorithm="port",
                   control=nls.control(tol=1e-5))
      args <- modifyList(args, x.args[names(x.args) %in% names(args)])
      args$weights <- args$weights^power
      x.args <- list(power=power)

      #run nls/pls
      #####################
      mod <- do.call(nls, args)
#      mod <- nls(.for, data=.out,
#                 weights = (1/.out$test)^power, # think about weighting
#                 start=.ls2, lower=.ls2,
#                 algorithm="port",
#                 control=nls.control(tol=1e-5) #think about tolerance
#      )

      #if we need to calculate AIC on a case-by-case basis...
      #for model, I think we need to use stats:::logLik.nls for AIC calc...
      #see
      #https://stackoverflow.com/questions/39999456/aic-on-nls-on-r
      #(currently calculating AIc on the lm model on the overall fit on
      # all species in all profiles as part of pls_report)

      ###################################
      #currently all-data stats in pls_report
      #  and returning list of models
      ###################################
      ##.tmp <- summary(mod)$coefficients
      ##.p.mod <- .tmp[,4]
      ##names(.p.mod) <- gsub("m_", "p_", names(.p.mod))
      ##.out <- data.frame(PROFILE_CODE = y,
      ##           t(.tmp[,1]),
      ##           t(.p.mod))
      ##.out

      #output list of mod + data
      ################################
      #could add args?
      #  then drop power from pls_ function formals
      #       or allow as an overwrite only...
      list(mod=mod,          #model outputs
           args=args,        #model args
           x.args=x.args)    #rsp args
    }, silent = TRUE)
    if(class(.test)[1]=="try-error"){
      NULL
    } else {
      .test
    }
  })
  names(ans) <- .pr.cd

  #returns the list of nls models
  #(assuming all viable, one per profile_code in x)

  #testing class options
  class(ans) <- unique(c("rsp_pls", class(ans)))
  return(ans)

}


#############################
#############################
## pls_report
#############################
#############################

#' @rdname sp.pls
#' @export

##   now imports from xxx.r
##   #' @import data.table

# this is the model report table
# other pls_ functions use output
#    so take care when changing anything...

# to think about
###############################

# drop intercept from diagnostics model..?
#    can't decide if it should be there
#    not in the pls_plot which are based on conventional SA plots...

# calculate the x_[profile] (contributions) in pls_report
#    currently doing this in several of the pls_plot's

# should the diagnostics be calculated per-species???
#    if some species very large and some very small
#        doing them on an all results basis will be overly positive

pls_report <- function(pls){

  ans <- lapply(names(pls), function(x){
    .xx <- pls[[x]]
    if(!is.null(.xx)){
      .out <- .xx$args$data
      .tmp <- summary(.xx$mod)$coefficients
      .p.mod <- .tmp[,4]
      names(.p.mod) <- gsub("m_", "p_", names(.p.mod))
      .out <- data.frame(PROFILE_CODE = x,
                         .out,
                         t(.tmp[,1]),
                         t(.p.mod),
                         pred = predict(.xx$mod, newdata=.xx$args$data),
                         check.names=FALSE)
      .out
    } else {
      NULL
    }
  })
  ans <- data.table::rbindlist(ans, use.names=TRUE, fill=TRUE)
  if(nrow(ans)==0){
    return(as.data.frame(ans))
  }

  #####################
  #thinking about
  #####################
  #    adding x_[profile] (m_[profile] * profile) calculations here
  #    currently done on fly in some plots...

  ans$.value <- ans$test

  #######################################
  # previous
  #     as all-species step
  #######################################
  ##    .mod <- lm(pred ~ 0 + .value, data = .out)
  ##    .out$adj.r.sq <- summary(.mod)$adj.r.squared
  ##    .out$slope <- summary(.mod)$coefficients[1, 1]
  ##    .out$p.slope <- summary(.mod)$coefficients[1, 4]
  ##    .out$AIC <- AIC(.mod)
  ##    .out

  #################################
  # replacing with...
  #################################
  #by species calculate stats
  #    guessing this could be done in data.table???
  .sp.ref <- unique(ans$SPECIES_NAME)
  .tmp <- lapply(.sp.ref, function(x){
    .tmp <- subset(ans, SPECIES_NAME==x)
    #################
    # note
    #################
    #    was previouslys pred ~ .value
    #         and reported intercept and intercept p
    #
    .mod <- lm(pred ~ 0 + .value, data = .tmp)
    ###########
    #(also noted in sp_pls_profile)
    #if we need to calculate aic based on the method parameters...
    #need to read this:
    #https://stackoverflow.com/questions/39999456/aic-on-nls-on-r
    #see stats:::logLik.nls for AIC calc...
    .s.mod <- suppressWarnings(summary(.mod))
    ####################
    #above suppress warnings
    #    is to hide the perfect fit warning
    #        you get if you fit a marker...
    #            option to jitters still there
    #############
    data.frame(SPECIES_NAME = x,
               adj.r.sq = .s.mod$adj.r.squared,
               slope = .s.mod$coefficients[1, 1],
               p.slope = .s.mod$coefficients[1, 4],
               AIC = AIC(.mod)
    )
  })
  .tmp <- data.table::rbindlist(.tmp)
  ans <- merge(ans, .tmp, by="SPECIES_NAME")

  as.data.frame(ans)
}




#############################
#############################
## pls_test
#############################
#############################

#' @rdname sp.pls
#' @export

##   now imports from xxx.r
##   #' @import data.table

# this is the model tests
# this builds from pls_report

pls_test <- function(pls){
  .rp <- pls_report(pls)
  #species
  .tmp<- lapply(unique(.rp$SPECIES_NAME), function(i){
    .ans <- subset(.rp, SPECIES_NAME==i)
    data.frame(SPECIES_NAME = i,
               adj.r.sq = .ans$adj.r.sq[1],
               slope=.ans$slope[1],
               p.slope=.ans$p.slope[1],
               AIC = .ans$AIC[1])
  })
  .sp <- data.table::rbindlist(.tmp)

  #ref profiles
  .pn <- names(.rp)[grepl("^p_", names(.rp))]
  .ans <- data.table::as.data.table(.rp)[, lapply(.SD,
                                                  function(x){length(x[x>0.05])/length(x)}),
                                         .SDcols = .pn]
  .ans <- as.data.frame(.ans)
  .ans <- (1 - .ans)*100
  names(.ans) <- gsub("^p_", "gp_", names(.ans))

  list(.species=.sp,
       .pls = .ans)
}







####################################
####################################
## pls fitting
####################################
####################################

#includes
#   pls_fit_species and
#   pls_refit_species
#   pls_rebuild


#' @rdname sp.pls
#' @export

pls_fit_species <- function(pls, species, power=1,
                            refit.profile=TRUE,
                            as.marker=FALSE,
                            drop.missing=FALSE,
                            ...){
  #wrapper for multiple fits of new data to a pls model
  .id <- unique(species$SPECIES_NAME)
  for(i in .id){
    .sub.sp <- subset(species, SPECIES_NAME==i)
    .test <- try(pls_rebuild(pls, species=.sub.sp, power=power,
                             refit.profile=refit.profile,
                             as.marker=as.marker,
                             drop.missing=drop.missing,
                             ...),
        silent=TRUE)
    if(class(.test)[1]=="try-error"){
      warning("RSP_PLS> failed to fit: ", i, sep="")
    } else {
      pls <- .test
    }
  }
  pls
}



#' @rdname sp.pls
#' @export

pls_refit_species <- function(pls, species, power=1,
                              refit.profile=TRUE,
                              as.marker=FALSE,
                              drop.missing=FALSE,
                              ...){
  #wrapper for multiple fits of new data to a pls model
  .id <- species
  for(i in .id){
    .test <- try(pls_rebuild(pls, species=i, power=power,
                             refit.profile=refit.profile,
                             as.marker=as.marker,
                             drop.missing=drop.missing,
                             ...),
                 silent=TRUE)
    #pass back the error???
    if(class(.test)[1]=="try-error"){
      warning("RSP_PLS> failed to fit: ", i, sep="",
              call.=FALSE)
    } else {
      pls <- .test
    }
  }
  pls
}



#' @rdname sp.pls
#' @export


#############################
#this needs a lot of work
#############################

# pls_fit_species and pls_refit_species
#      are now multiple use wrappers for this...
#         they for loop try(pls_rebuild(...))

# (like pls_(re)fit_'s)
# like to drop power from formals
#   maybe ignore or pass from previous, but have option to overwrite via ...?

# need to update the model handling so it is like sp_pls_profile
#     this would sort power issue above
#          also means the user can change setting themselves
#          THINK ABOUT THIS
#               they could make a pls that was not positively constrained
#      this would also remove the start, lower and upper options
#           from the formals...

# if we are setting start and lower
#     start = lower if start is missing might be safer...
#        (see code in sp_pls_profile)

#needs to allow more constraint
#     currently not passing forward the args...

#mod <- readRDS("C:/Users/trakradmin/OneDrive - University of Leeds/Documents/pkg/respeciate/test/mod1.RDS")
#dat <- readRDS("C:/Users/trakradmin/OneDrive - University of Leeds/Documents/pkg/respeciate/test/uk.metals.aurn.2b.rds")

#pls_rebuild(mod, subset(dat, SPECIES_NAME=="[avg.AURN] O3"), power=2, as.marker=T)


pls_rebuild <- function(pls, species, power=1,
                         refit.profile=TRUE,
                         as.marker=FALSE,
                         drop.missing=FALSE,
                         ...){

  x.args <- list(...)
  #hiding model args
  #also like to hide power

  .out <- pls_report(pls)

  #cheat
  #########################
  .cheat <- character()
  .cheat2 <- character()

  #########################
  #standardise inputs
  #########################
  if(is.character(species)){
    #assuming this is SPECIES_NAME of the species to be fit
    #and species was in modelled data when pls was built...
    if(!species[1] %in% .out$SPECIES_NAME){
      stop("RSP_PLS> 'species' not in PLS, please check",
           call. = FALSE)
    }
    .add <- subset(.out, SPECIES_NAME == species[1])
    .out <- subset(.out, SPECIES_NAME != species[1])

  } else {
    #assuming this is respeciate object/data.frame of right structure
    .add <- species
  }

  ###################################
  #get and check species name and id
  ###################################
  sp.nm <- unique(.add$SPECIES_NAME)
  sp.id <- unique(.add$SPECIES_ID)
  #both need to be 1 element
  if(length(sp.nm) !=1 || length (sp.id) != 1){
    stop("RSP_PLS> 'species' not unique, either missing or multiple",
         call. = FALSE)
  }

  #if as.marker is character
  #   use it as marker profile name and reset as.marker to TRUE
  #   else use species_name as profile name
  #        wondering if this should be more unique
  if(is.character(as.marker)){
    .mrk.nm <- as.marker
    as.marker <- TRUE
  } else {
    .mrk.nm <- sp.nm
  }

  #####################
  #as.marker T/F handling
  #####################
  if(as.marker){
    #treat species as marker
    for(i in names(pls)){
      if(i %in% unique(.add$PROFILE_CODE) & !is.null(pls[[i]])){
        #remark off all print when happy with method
        #print(i)
        #########################
        #can simplify a lot below
        #########################
        x <- pls[[i]]
        .da <- subset(x$args$data, SPECIES_NAME != sp.nm)
        .da[.mrk.nm] <- 0
        #.cht <- rev(unique(c("test", rev(names(.da)))))
        #.da <- .da[.cht]
        .da <- .da[rev(unique(c("test", rev(names(.da)))))]
        .mn.df <- .da[1,]
        #.mn.df[,1] <- sp.id
        #.mn.df[,2] <- sp.nm
        .mn.df[,c(1,2)] <- c(sp.id, sp.nm)
        .mn.df[,3:(ncol(.da)-2)] <- 0
        ##############################
        #below might want to be something other than 1
        #    e.g. median other others excluding zero's???
        .mn.df[,ncol(.da)-1] <- 1
        #######################################
        #might need to add a jitter to next???
        #######################################
        #print("hi")
        #print(.add)
        #print(i)
        #print(.add[.add$PROFILE_CODE==i,])
        .mn.df[,ncol(.da)] <- .add[.add$PROFILE_CODE==i, ".value"]
        if(!is.na(.mn.df[,ncol(.da)])){


        ##################################
        #a lot below needs more generalising
        ###################################
        pls[[i]]$args$data <- rbind(.da, .mn.df)
        pls[[i]]$args$weights <- (1/pls[[i]]$args$data$test)^power
        if(any(!grepl(.mrk.nm, pls[[i]]$args$formula))){
          #update formula
          .for <- as.character(pls[[i]]$args$formula)
          .for[3] <- paste(.for[3], "+ (`m_", .mrk.nm,
                           "` * `", .mrk.nm, "`)",
                           sep="")
          pls[[i]]$args$formula <- as.formula(paste(.for[2], .for[1],
                                                    .for[3], sep=""))
        }
        if("start" %in% names(pls[[i]]$args)){
          if(!paste("m_", .mrk.nm, sep="") %in% names(pls[[i]]$args$start)){
            #print("adding m_ start")
            .arg <- pls[[i]]$args$start
            .arg[[paste("m_", .mrk.nm, sep="")]] <-0
            pls[[i]]$args$start <- .arg
          }
        }
        if("lower" %in% names(pls[[i]]$args)){
          if(!paste("m_", .mrk.nm, sep="") %in% names(pls[[i]]$args$lower)){
            #print("adding m_ lower")
            .arg <- pls[[i]]$args$lower
            .arg[[paste("m_", .mrk.nm, sep="")]] <-0
            pls[[i]]$args$lower <- .arg
          }
        }
        if("upper" %in% names(pls[[i]]$args)){
          if(!paste("m_", .mrk.nm, sep="") %in% names(pls[[i]]$args$upper)){
            #print("adding m_ upper")
            .arg <- pls[[i]]$args$upper
            .arg[[paste("m_", .mrk.nm, sep="")]] <- Inf
            pls[[i]]$args$upper <- .arg
          }
        }

        #print(pls[[i]]$args$data)
        #print(pls[[i]]$args$formula)
        #print(pls[[i]]$args$weights)
        ######################
        #nls model do.call might need a try wrapper
        ########################
        .cheat2 <- c(.cheat2, i)

        pls[[i]]$mod <- do.call(nls, pls[[i]]$args)
      } #stop it trying entry is NA
      } else {
        #can't build this model update, so drop it!
        #either no marker or no previous model
        #############################
        #might want to change this to
        #leave them alone???
        #   just might never get the o3 profile included
        #   or make the as.marker = FALSE drop the case it
        #       can't model...?
        if(drop.missing){
          .cheat <- c(.cheat, i)
          pls[i] <- list(NULL)
        }
      }
    }
    #print("doing these [mrk]")
    #print(.cheat2)
    #print("dropping these [mrk]")
    #print(.cheat)
  } else {
    ######################################
    #species not a marker
    ######################################
    #distribute across existing sources
    ######################################

    ###############################
    #remark prints when happy with method
    ###############################

    #########################
    #like to first better way of doing following
    #########################

    #need to build a unique data set of previous m matrix predictions
    ##################
    #.test <- .out[.out$pred>0,]
    # (had to exclude pred = 0 because these were not yet modelled)
    #.out <- subset(.out, SPECIES_ID == unique(.test$SPECIES_ID)[1])
    # (replacing with following because above dropped models if first species
    #  was missing from those profile_code)
    #
    .test <- .out[.out$pred>0,]
    .out <- .test[!duplicated(.test$PROFILE_CODE),]

    .test <- c("PROFILE_CODE", ".value", "WEIGHT_PERCENT")
    .test <- names(.add)[names(.add) %in% .test]
    .data <- .add[.test]

    names(.data)[2] <- "refit"
    .data <- merge(.out, .data[c(1:2)])

    #########################
    #note
    #if checking .data species may not be unique
    # just after a unique (all profile_code) m matrix
    # for the added
    #print(.data)

    .ms <- names(.data)[grepl("^m_", names(.data))]
    .for <- paste("(`", .ms, "`*`", gsub("^m_", "n_", .ms), "`)",
                  sep="", collapse = "+")
    .for <- as.formula(paste("refit~", .for))

    .ns <- .ms
    names(.ns) <- gsub("^m_", "n_", .ms)

    #note
    ##################
    #model handling temp update
    #lower, start and upper
    lower <- if("lower" %in% names(x.args)){
      x.args$lower
    } else {
      0
    }
    start <- if("start" %in% names(x.args)){
      x.args$start
    } else {
      lower
    }
    upper <- if("upper" %in% names(x.args)){
      x.args$upper
    } else {
      Inf
    }
    .ls <- lapply(.ns, function(x){start})
    .ls2 <- lapply(.ns, function(x){lower})
    .ls3 <- lapply(.ns, function(x){upper})

    control <- if("control" %in% names(x.args)){
      x.args$control
    } else {
      nls.control(tol=1e-5)
    }

    #print(.data)
    #print(.for)

    mod <- nls(.for, data=.data,
               #weights = (1/.out$test)^power,
               #no weighting currently because species are all the same here!
               start=.ls,
               lower=.ls2,
               upper=.ls3,
               algorithm="port",
               control=control #think about tolerance
    )
    #check.names TRUE was applying make.names
    #     so turned off when building data.frames for pls model outputs
    .ans <- data.frame(
      PROFILE_CODE = .data$PROFILE_CODE,
      SPECIES_ID = .add$SPECIES_ID[1],
      SPECIES_NAME = .add$SPECIES_NAME[1],
      t(coefficients(mod)),
      test = .data$refit,
      check.names=FALSE
    )
    names(.ans) <- gsub("^n_", "", names(.ans))

    #print("doing these")
    #print(.ans$PROFILE_CODE)

    #for each build model, put new models in pls
    ###################################
    #need to move this to working directly from models
    for(i in unique(.ans$PROFILE_CODE)){
      .ii <- subset(.ans, PROFILE_CODE==i)
      .ii <- .ii[names(.ii) != "PROFILE_CODE"]
      .nn <- pls[[i]]$args$data
      .nn <- subset(.nn, !SPECIES_NAME %in% unique(.ii$SPECIES_NAME))
      ###########
      #cheat
      #############
      #print(.nn)
      #print(.ii)
      .ii <- .ii[names(.ii) %in% names(.nn)]
      ########################

      pls[[i]]$args$data <- rbind(.nn, .ii)
      #rebuild model
      .for <- as.character(formula(pls[[i]]$mod))
      .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
      .ms <- names(pls[[i]]$args$data)
      .ms <- .ms[!.ms %in% c("SPECIES_ID", "SPECIES_NAME", "test")]
      .ls <- lapply(.ms, function(x){0})
      names(.ls) <- paste("m_", .ms, sep="")
      .da <- pls[[i]]$args$data
      pls[[i]]$args$weights <- (1/pls[[i]]$args$data$test)^power
      pls[[i]]$args$control <- control
      #################
      #can these go now..?
      #################
      if("start" %in% names(pls[[i]]$args)){
        if(!paste("m_", .mrk.nm, sep="") %in% names(pls[[i]]$args$start)){
          #print("adding m_ start")
          .arg <- pls[[i]]$args$start
          #.arg[[paste("m_", .mrk.nm, sep="")]] <-0
          pls[[i]]$args$start <- .arg
        }
      }
      if("lower" %in% names(pls[[i]]$args)){
        if(!paste("m_", .mrk.nm, sep="") %in% names(pls[[i]]$args$lower)){
          #print("adding m_ lower")
          .arg <- pls[[i]]$args$lower
          #.arg[[paste("m_", .mrk.nm, sep="")]] <-0
          pls[[i]]$args$lower <- .arg
        }
      }
      if("upper" %in% names(pls[[i]]$args)){
        if(!paste("m_", .mrk.nm, sep="") %in% names(pls[[i]]$args$upper)){
          #print("adding m_ upper")
          .arg <- pls[[i]]$args$upper
          #.arg[[paste("m_", .mrk.nm, sep="")]] <- Inf
          pls[[i]]$args$upper <- .arg
        }
      }

    }
    if(drop.missing){
      ##########################################
      #if we are dropping cases were species was
      #     not available, we need to drop the
      #         models that were not (re)fit...
      #print("dropping these!")
      .test <- names(pls)[!names(pls) %in% unique(.ans$PROFILE_CODE)]
      #print(.test)
      if(length(.test)>0){
        for(i in .test){
          pls[i] <- list(NULL)
        }
      }
    }
  }

  ################
  #refit.profiles
  ################
  #this might be a little redundant now

  if(refit.profile){
    for(i in names(pls)){
      if(!is.null(pls[[i]])){
        #print(i)
        #print(pls[[i]]$args$data)
        #print(pls[[i]]$args$formula)

        pls[[i]]$mod <- do.call(nls, pls[[i]]$args)
        #pls[[i]]$mod <- nls(.for, data=.da,
        #                    weights = (1/.da$test)^power, # think about weighting
        #                    start=.ls, lower=.ls,
        #                    algorithm="port",
        #                    control=nls.control(tol=1e-5) #think about tolerance
        #)
        #.for <- as.character(formula(pls[[i]]$mod))
        #.for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
        #.da <- pls[[i]]$args$data
        #.ls <- pls[[i]]$args$lower
        #print(.da)
        #print(.ls)
        #print((1/.da$test)^power)
        #pls[[i]]$mod <- nls(.for, data=.da,
        #                    weights = (1/.da$test)^power, # think about weighting
        #                    start=.ls, lower=.ls,
        #                    algorithm="port",
        #                    control=nls.control(tol=1e-5) #think about tolerance
        #)
        #print("refit.profile")
      }
    }
  }
  ################
  #output
  ################
  pls
}

#################




#fix if nulls are an issue
############################

#mod3 <- mod3[unlist(lapply(mod3, function(x) !is.null(x)))]

#test code
####################

#inc <- readRDS("C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\_projects\\marylebone03\\.tmp.increment.rds")
#inc$PROFILE_CODE <- as.character(inc$`Start Date`)
#inc$PROFILE_NAME <- as.character(inc$`Start Date`)
#inc <- sp_build_rsp_x(inc, value=".value.inc")

#sp_match_profile(inc, spq_pm(), matches=20)

#aa <- sp_profile(c("3157", "4330310", "3941", "4027", "3961"))
#inc.metals <- subset(inc, !grepl("[[]avg.AURN[]]", SPECIES_NAME))

#moda <- sp_pls_profile(inc.metals, aa)
#modb <- sp_pls_profile(inc, aa)

#moda2 <- pls_fit_parent(moda, subset(inc, SPECIES_NAME=="[avg.AURN] PM2.5"))

#moda2i <- pls_fit_species(moda, subset(inc, SPECIES_NAME=="[avg.AURN] PM2.5"))



############################
#next steps
############################

#note

# this is rebuild version 2
#    first version currently pls_rebuild.old (unexported)

#tidy code
#    go through and tidy messy code
#    NB: data.frame names might be getting changed in some functions
#        seemed to be happening in multiple refits....
#            looked like make.name(BAD-NAME)
#    think about models with missing input
#        leave in or drop??
#           or option to do both... ???
#    think about power and other nls arguments
#        need to be handling these better...
#        currently re-calaculating on rebuild
#             BUT might need to be able to work with user input???
#    update the documents

#    have hidden perfect fit error in pls_report
#       think that kills it anywhere
#           but should check pls_plot...
#       also could add a jigger when fitting marker in rebuild?





####################################
###################################
## pls_plots
###################################
###################################

#these are all draft


####################################
####################################
## pls_plot
####################################
####################################


#' @rdname sp.pls
#' @export

##   now imports via data.table::
##        need this to kill the as.data.table load message
##   #' @import data.table
##

#############################
#this needs a lot of work
#############################

# this uses unexported rsp_profile_pie function below...
#     both pls_plot and rsp_profile_pie need work...


pls_plot <- function (pls, n, type = 1, ...){

  #current using lattice/latticeExtra for the panelling/layers...

  #basic plots finished but...
  #    currently not passing arguments generally
  #    the par setting seem to be dropped when using plot(p)
  #         ahead of end of function

  ############################
  # nags
  ############################

  # type = 1
  ############################

  # note sure about the layer naming
  # zero is not bottom of barchart...

  # type = 2
  ############################

  # the label positioning is messy (see not about nudge)

  # cex setting too small if only one panel...

  #wondering about
  #    https://latticeextra.r-forge.r-project.org/man/postdoc.html
  #    as an alternative to type=2
  #    (but 100 percent measured rather than proportion...)

  #################
  #setup
  #################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- rsp_profile_code_order(dat)
  .sp.ref <- unique(dat$SPECIES_NAME)
  #species
  #  now defaulting to all plots
  species <- if (missing(n)) {
    species <- .sp.ref
  }
  else {
    n
  }
  if (is.numeric(species)) {
    if (all(species == -1)) {
      species <- .sp.ref
    }
    else {
      species <- .sp.ref[species]
    }
  }
  if (!any(species %in% .sp.ref)) {
    stop("RSP_PLS> unknown species, please check", call. = FALSE)
  }
  ################################
  #note:
  #  could condition here BUT currently
  #  holding on to everything until just before plot
  #  might not need to do this....
  #################################

  .sp.ord <- unique(dat$SPECIES_ID)
  .sp.m.pro <- names(dat)[grep("^m_", names(dat))]
  .sp.pro <- gsub("^m_", "", .sp.m.pro)

  #line col....
  .col <- lattice::trellis.par.get("superpose.line")$col[1]

  #bar cols
  .cols <- if ("col" %in% names(.x.args)) {
    #could include if you supply a function..?
    #could use col.regions?
    .cols <- .x.args$col
  }
  else {
    .cols <- heat.colors(n = length(.sp.m.pro))
  }
  if (length(.cols) != length(.sp.m.pro)) {
    stop("pls_plot> halted; expecting ", length(.sp.m.pro),
         "colours; given ", length(.cols), sep = "", call. = FALSE)
  }

  ######################
  # build x_[profile]
  ######################
  for (i in .sp.pro) {
    dat[, paste("x_", i, sep = "")] <- dat[, paste("m_",
                                                   i, sep = "")] * dat[, i]
  }
  .sp.x.pro <- names(dat)[grep("^x_", names(dat))]
  .rep <- dat[c("SPECIES_NAME", "SPECIES_ID", "PROFILE_CODE",
                .sp.x.pro)]
  .rep <- data.table::melt(data.table::as.data.table(.rep),
                           id = c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE"))
  .tot <- data.table::as.data.table(dat)
  .cs <- c(".value", "pred", .sp.x.pro)
  .tot <- .tot[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
               .SDcols = .cs, by = c("SPECIES_ID", "SPECIES_NAME")]

  ###########################
  # now plotting as panels
  # using
  ###########################

  ######################################################
  # now using rsp_ function to track all pls model cases
  # previous method only tracked valid cases for the plotted data
  #    so no gaps where models dropped/not built...
  #########################################################
  .rep$.index <- as.numeric(factor(.rep$PROFILE_CODE, levels = .ord.pro.c,
                                   ordered = TRUE))
  dat$.index <- as.numeric(factor(dat$PROFILE_CODE, levels = .ord.pro.c,
                                  ordered = TRUE))
  .tmp <- dat[c("SPECIES_ID", "PROFILE_CODE", ".index", ".value", "pred")]
  .rep <- data.table::merge.data.table(.rep, .tmp)

  .rep$variable <- gsub("^x_", "", .rep$variable)

  #print(names(.rep))
  #return(dat)

  .rep <- subset(as.data.frame(.rep), SPECIES_NAME %in% species)

  if (1 %in% type) {

    #lattice sets panel order based
    .sp <- if(is.factor(.rep$SPECIES_NAME)){
      levels(.rep$SPECIES_NAME)
    } else {
      sort(unique(.rep$SPECIES_NAME))
    }
    .sp <- .sp[.sp %in% .rep$SPECIES_NAME]
    #.y.scale <- lapply(unique(.rep$SPECIES_NAME), function(x){
    .y.scale <- lapply(.sp, function(x){
      .tmp <- .rep[.rep$SPECIES_NAME==x,]
      c(0, max(c(.tmp$.value, .tmp$pred), na.rm=TRUE))
    })
    ###############################################
    #use loa method to generalise this?
    ###############################################


    p2 <- lattice::xyplot(.value ~ .index | SPECIES_NAME, .rep,
                          panel=lattice::panel.xyplot,
                          type="l", xlab="Sample [index]",
                          ylab="Measurement",
                          scales=list(relation="free"),
                          ylim=.y.scale)

    p <- lattice::barchart(value ~ factor(.index) | SPECIES_NAME, .rep,
                           groups=.rep$variable, stack=TRUE,
                           panel=function(x, y, col, groups, ..., subscripts){
                             #grid control like loa
                             rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                          lattice::panel.grid, ...)
                             lattice::panel.barchart(x=x, y=y, col=col,
                                                     groups=groups,
                                                     subscripts=subscripts, ...)
                             .y <- .rep$.value[subscripts]
                             #col needs to be from option[1]
                             lattice::panel.xyplot(x=x, y=.y,
                                                   col=.col,
                                                   type="l",
                                                   subscripts=subscripts,...)
                           },
                           scales=list(relation="free"),
                           #auto.key=list(space="top", columns=2,
                           #               col.line=.cols,
                           #              points=FALSE, rectangles=TRUE),
                           ylim=.y.scale,
                           col=.cols,
                           border=NA,
                           #par.settings = list(superpose.polygon = list(col = .cols,
                           #                          pch =c (15, 15)),
                           #superpose.symbol = list(fill = .cols)),
                           auto.key=list(space="top", columns = 3,
                                         cex=0.8,
                                         points=FALSE,
                                         rectangles=TRUE)) #,
    #xscale.components = function(lim,...){
    #  lim <- as.numeric(as.character(lim))
    #  ans <- lattice::xscale.components.default(lim=lim,...)
    #  print(ans)
    #  ans
    #})
    plot(update(latticeExtra::doubleYScale(p2, p, add.axis = FALSE),
                par.settings = list(superpose.polygon = list(col = .cols),
                                    superpose.symbol = list(fill = .cols))))

    #p2 <- lattice::xyplot(.value ~ factor(.index) | SPECIES_NAME, dat,
    #                      type="l", scales=list(relation="free"))
    #plot(cheat(p, latticeExtra::as.layer(p2)))

    #plot(latticeExtra::doubleYScale(p, p2, add.axis=FALSE, add.ylab2=FALSE))
  }
  if (2 %in% type) {


    p <- lattice::xyplot(value ~ .index | SPECIES_NAME, .rep,
                         groups=.rep$variable,
                         totals=.rep$.value,
                         scales=list(relation="free",
                                     draw=FALSE),
                         ylab="", xlab="",
                         col = .cols,
                         auto.key=list(space="top", columns = 3,
                                       cex=0.8,
                                       points=FALSE,
                                       rectangles=TRUE),
                         ylim=c(-2,2), xlim=c(-2,2),
                         between = list(x = 0.2, y = 0.2),
                         panel=rsp_panel.pie,
                         par.settings = list(superpose.polygon = list(col = .cols),
                                             axis.line = list(col = 'transparent'),
                                             superpose.symbol = list(fill = .cols))
    )
    plot(p)

  }
  invisible(.rep)
}



#test <- "C:/Users/trakradmin/OneDrive - University of Leeds/Documents/pkg/respeciate"
#mod <- readRDS(paste(test, "mod2.RDS", sep="/"))
#pls_plot(mod)






####################################
####################################
## pls_plot_species
####################################
####################################


#' @rdname sp.pls
#' @export

##   now imports from xxx.r
##   #' @import data.table

#############################
#this needs a lot of work
#############################


pls_plot_species <- function (pls, n, type = 1, ...)
{
  ###########################
  # setup
  ###########################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- rsp_profile_code_order(dat)
  .sp.ref <- unique(dat$SPECIES_NAME)
  species <- if (missing(n)) {
    .sp.ref
    #default option (print the lot...)
    ############################
    #possibly a warning if lots of species to plot
    ##################
  } else {
    n
  }
  if (is.numeric(species)) {
    if (all(species == -1)) {
      species <- .sp.ref
    }
    else {
      species <- .sp.ref[species]
    }
  }
  if (!any(species %in% .sp.ref)) {
    stop("RSP_PLS> unknown species, please check", call. = FALSE)
  }
  ############################
  #if not earlier, then here?
  #possibly a warning if lots of species to plot
  ##################

  #########################
  #could drop a lot of this??
  #########################
  .xlb <- if ("xlab" %in% names(.x.args)) {
    .x.args$xlab
  } else {
    "Measurement"
  }
  .ylb <- if ("ylab" %in% names(.x.args)) {
    .x.args$ylab
  } else {
    "Model"
  }
  .bc <- if ("col" %in% names(.x.args)) {
    .x.args$col
  } else {
    par("col")
  }
  .mc <- if ("mod.col" %in% names(.x.args)) {
    .x.args$mod.col
  } else {
    "red"
  }
  dat <- subset(dat, SPECIES_NAME %in% species)
  #    lims <- range(c(d2$.value, d2$pred), na.rm = TRUE, finite = TRUE)
  #    mod <- lm(pred ~ 0 + .value, d2)
  #    .sum <- paste("y = ", signif(summary(mod)$coefficients[1,
  #        1], 3), "x (adj.R2 = ", signif(summary(mod)$adj.r.squared,
  #        3), ")", sep = "")
  .lims <- lapply(species, function(x){
    .d <- subset(dat, SPECIES_NAME==x)
    range(c(.d$pred, .d$.value), finite=TRUE, na.rm=TRUE)
  })
  if (1 %in% type) {
    p1.ls <- list(x=pred~.value | SPECIES_NAME, data=dat,
                  #prepanel forces x and y lims to same range
                  prepanel=function(...){
                    .tmp <- prepanel.default.xyplot(...)
                    .tmp$xlim <- range(c(.tmp$xlim, .tmp$ylim))
                    .tmp$ylim <- .tmp$xlim
                    .tmp
                  },
                  panel= function(x, y, xlim, ylim, ...){
                    #user control of grid - like loa...
                    rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                 lattice::panel.grid, ...)
                    #TO DO
                    #user control of y=x
                    panel.ablineq(a = 0, b = 1, adj = c(0,1),
                                  col.line ="grey", lty=2, label="")
                    #user control of main plotted data via
                    # standard lattice
                    panel.xyplot(x=x, y=y,  ...)
                    #CURRENTLY JUST col via mod.col
                    #user control of model
                    panel.ablineq(lm(y ~ x + 0), cex = 0.8,
                                  x = min(c(x, y), na.rm=TRUE),
                                  y = max(c(x, y), na.rm=TRUE),
                                  r.squared = TRUE, adj = c(0,1),
                                  sep = " (", sep.end = ")",
                                  offset=0, varStyle = NULL,
                                  col.line = .mc, col.text = .mc, digits = 2)
                  },
                  xlab="Measurement", ylab="model",
                  scales=list(y=list(relation="free",
                                     rot=90),
                              x=list(relation="free")))
    p1.ls <- modifyList(p1.ls, .x.args)
    p <- do.call(xyplot, p1.ls)
    plot(p)

    #      plot(d2$.value, d2$pred, type = "n", main = i, col = .bc,
    #          xlab = .xlb, ylab = .ylb, xlim = lims, ylim = lims)
    #      grid()
    #      abline(a = 0, b = 1, col = "grey")
    #      points(d2$.value, d2$pred)
    #      abline(mod, col = .mc, lty = 2)
    #      text(lims[1], lims[2], .sum, adj = c(0, 1), cex = 0.75)
  }
  if (2 %in% type) {
    #xlab
    if(!"xlab" %in% names(.x.args)){
      .x.args$xlab <- "Sample [index]"
    }
    if(!"ylab" %in% names(.x.args)){
      .x.args$ylab <- "Measurement, Model"
    } else {
      if(length(.x.args$ylab)>1){
        if(!"key.text" %in% names(.x.args)){
          .x.args$key.text <- .x.args$ylab[1:2]
        }
        .x.args$ylab <- paste(.x.args$ylab[1], .x.args$ylab[2], sep =", ")
      }
    }
    if(!"key.text" %in% names(.x.args)){
      .x.args$key.text <- c("Measurement", "Model")
    }
    if(!"col" %in% names(.x.args)){
      .x.args$col <- trellis.par.get("superpose.line")$col[1:2]
    }
    if("mod.col" %in% names(.x.args)){
      .x.args$col <- c(.x.args$col[1], .x.args$mod.col)
    }


    #ylab
    #can to two terms for

    #if("ylab" %in% names(.x.args)){
    #  if(length(.x.args$ylab)>1){
    #    if(!"key.text" %in% names(.x.args)){
    #      .x.args$key.text <- .x.args$ylab[1:2]
    #    }
    #    .x.args$ylab <- paste(.x.args$ylab[1], .x.args$ylab[2], sep =", ")
    #  } else {
    #    if(!"key.text" %in% names(.x.args)){
    #      .x.args$key.text <- c("Measurement", "Model")
    #    }
    #  }
    #} else {
    #  if(!"key.text" %in% names(.x.args)){
    #    .x.args$key.text <- c("Measurement", "Model")
    #  }
    #  .x.args$ylab <- "Measurement, Model"
    #}




    #########################
    #previous code
    #########################
    #plot(d2$.value, type = "n", main = i, col = .bc,
    #    ylab = .ylb, xlab = .xlb, ylim = lims)
    #lines(d2$.value)
    #lines(d2$pred, col = .mc)
    ########################
    #using standardised index
    #make 'ordered profile codes' at top
    #      before any subsetting...
    #      .ord.pro.c <- rsp_profile_code_order(dat)
    dat$.index <- as.numeric(factor(dat$PROFILE_CODE, levels=.ord.pro.c,
                                    ordered = TRUE))
    p2.ls <- list(x= .value + pred ~ .index | SPECIES_NAME, data=dat,
                  auto.key = list(text=.x.args$key.text,
                                  space="top", columns=2),
                  type="l",
                  panel= function(...){
                    rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                 panel.grid, ...)
                    lattice::panel.xyplot(...)
                  },
                  scale=list(relation="free"),
                  par.settings = simpleTheme(col=.x.args$col))
    p2.ls <- modifyList(p2.ls, .x.args)
    p <- do.call(xyplot, p2.ls)
    plot(p)
    ######################

    #    or any with any missing are plot on different x scale
    #    maybe find longest, take range for that
    #xyplot(.value + .pred ~ )
  }

  invisible(dat)
}



####################################
####################################
## pls_plot_profile
####################################
####################################


#' @rdname sp.pls
#' @export

##   now imports from xxx.r
##   #' @import data.table


#############################
#this needs a lot of work
#############################

pls_plot_profile <- function (pls, n, log = FALSE, ...)
{
  #########################
  #previous plot used base r graphics
  #this moved to lattice/latticeExtra
  #so we can panel outputs
  #########################

  #setup
  .x.args <- list(...)
  .plt.args <- .x.args[names(.x.args %in% c())]
  dat <- pls_report(pls)
  .sp.ord <- unique(dat$SPECIES_ID)
  .sp.m.pro <- names(dat)[grep("^m_", names(dat))]
  .sp.pro <- gsub("^m_", "", .sp.m.pro)
  #defaulting n to all profiles as one plot
  profile <- if (missing(n)) {
    profile <- .sp.pro
  } else {
    n
  }
  if (is.numeric(profile)) {
    if (all(profile == -1)) {
      profile <- .sp.pro
    }
    else {
      profile <- .sp.pro[profile]
    }
  }
  if (!any(profile %in% .sp.pro)) {
    stop("RSP_PLS> unknown profile(s), please check", call. = FALSE)
  }

  #########################
  #build x_[profile]
  #########################
  m_profile <- paste("m_", profile, sep = "")
  dat <- dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE",
               profile, m_profile, "pred", ".value")]
  for (i in profile) {
    dat[, paste("x_", i, sep = "")] <- dat[, paste("m_",
                                                   i, sep = "")] * dat[, i]
  }
  .rep <- data.table::as.data.table(dat)
  .cols <- c(".value", "pred", paste("x_", profile, sep = ""))
  .rep <- .rep[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
               .SDcols = .cols, by = c("SPECIES_ID", "SPECIES_NAME")]
  .rep <- as.data.frame(.rep)

  #########################
  # y2 setup
  #########################
  # by default this is .value
  #   but might want mod prediction
  if ("y2" %in% names(.x.args) && .x.args$y2 == "pred") {
    for (i in profile) {
      .rep[, paste("pc_", i, sep = "")] <-
        (.rep[, paste("x_", i, sep = "")]/.rep$pred) * 100
    }
  }
  else {
    for (i in profile) {
      .rep[, paste("pc_", i, sep = "")] <-
        (.rep[, paste("x_", i, sep = "")]/.rep$.value) * 100
    }
  }
  #might not need all of following now we
  #we are not pulling apart to plot one at time...
  dat <- dat[!duplicated(dat$SPECIES_NAME), ]
  dat$PROFILE_NAME <- dat$PROFILE_NAME[1]
  dat$PROFILE_CODE <- dat$PROFILE_CODE[1]
  dat <- merge(.rep, dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE",
                           profile)], )
  dat <- dat[order(ordered(dat$SPECIES_ID, levels = .sp.ord)), ]

  ################################
  # build pc_[profile]
  ################################
  rownames(dat) <- 1:nrow(dat)
  .ref <- names(dat)[grep("pc_", names(dat))]
  .oth <- c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE", ".value", "pred")
  .temp <- data.table::as.data.table(dat[c(.oth, gsub("pc_", "", .ref))])
  .d1 <- data.table::melt(.temp, measure.vars = gsub("pc_", "", .ref),
              variable.name = "pls_profile", value.name = "loading")
  .temp <- data.table::as.data.table(dat[c(.oth, .ref)])
  .d2 <- data.table::melt(.temp, measure.vars = .ref,
              variable.name = "pls_profile", value.name = "percent_contr")
  .d2$pls_profile <- gsub("pc_", "", .d2$pls_profile)
  dat <- as.data.frame(merge(.d1, .d2, all=T))
  #############################

  ############################
  #now using lattice to handle logs
  ###############
  #.dat <- dat
  #don't need local version of dat because not changing data ahead of plot
  #if(log){
  #  .dat$loading <- log10(.dat$loading)
  #  .ylim <- lapply(profile, function(x){
  #    .temp <- subset(.dat, pls_profile==x)
  #    .temp <- range(.temp$loading, na.rm=TRUE, finite=TRUE)
  #    if(.temp[1] == .temp[2]){
  #      .temp <- c(.temp[1]-1, .temp[1]+1)
  #    }
  #    range(c(floor(.temp), ceiling(.temp)))
  #  })
  #} else {
  #  .ylim <- lapply(profile, function(x){
  #    .temp <- subset(.dat, pls_profile==x)
  #    .temp <- range(.temp$loading, na.rm=TRUE, finite=TRUE)
  #    range(pretty(.temp))
  #  })
  #}


  ######################
  #plot
  ######################
  #now using lattice/latticeExtra
  ##
  #think there is more here that can be generalized...
  p1.ls <- list(x = loading~SPECIES_NAME | pls_profile,
                data=dat, ylab="Source Loading",
                panel = function(...){
                  rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                               panel.grid, ...)
                  panel.barchart(...)
                },
                between=list(y=.2),
                scales=list(x=list(rot=90),
                            y=list(rot=c(0,90),
                                   relation="free")),
                layout=c(1,length(profile)))
  if(log){
    p1.ls$scales$y$log=10
    p1.ls$yscale.components <- rsp_yscale.component.log10
  }
  p1.ls <- modifyList(p1.ls, .x.args)
  if(!"col" %in% names(p1.ls)){
    p1.ls$col <- trellis.par.get("superpose.line")$col[1]
  }
  p1 <- do.call(barchart, p1.ls)
  if("mod" %in% names(.x.args) && !.x.args$mod){
    #if mod FALSE just plot 1
    plot(p1)
  } else {
    #add mod layer (total contributions) as y2
    .col2 <- if("mod.col" %in% names(.x.args)){
      .x.args$mod.col
    } else {
      trellis.par.get("superpose.line")$col[2]
    }
    p2.ls <- list(x = percent_contr ~ factor(SPECIES_NAME) | pls_profile,
                  pch=16, type=c("h", "p"), col= c(.col2, .col2),
                  ylab="Total Contribution (%)",
                  data=dat)
    .tmp <- .x.args[grepl("^mod[.]", names(.x.args))]
    if(length(.tmp)>0){
      names(.tmp) <- gsub("^mod[.]", "", names(.tmp))
      p2.ls <- modifyList(p2.ls, .tmp)
    }
    p2 <- do.call(xyplot, p2.ls)
    plot(update(doubleYScale(p1, p2, add.ylab2 = TRUE),
                par.settings = simpleTheme(col=c(p1.ls$col[1], .col2))))
  }

  ############
  #output
  ############
  #could pass plot and data as list???
  return(invisible(dat))
}


















################
################
## unexported
################
################

# profile code order
# get profile order in case you need it latter...

rsp_profile_code_order <- function(data){
  .tmp <-  data.table::as.data.table(data)[, .(ans=length(unique(PROFILE_CODE))),by="SPECIES_NAME"]
  .tmp <- subset(.tmp, ans == max(.tmp$ans, na.rm=TRUE))$SPECIES_NAME
  .tmp <- subset(data, SPECIES_NAME %in% .tmp)
  unique(.tmp$PROFILE_CODE)
}


#log axis hander
#based on lattice text book method

#issues??
#   could be problem with y padding when log=T and .value range is wide...

rsp_yscale.component.log10 <- function(lim, ...) {
  ans <- yscale.components.default(lim = lim, ...)
  tick.at <- pretty(lim)
  tick.at <- tick.at[tick.at == floor(tick.at)]
  tick.at <- tick.at[tick.at < max(lim, na.rm=TRUE) & tick.at > min(lim, na.rm=TRUE)]
  ans$left$ticks$at <- tick.at
  ans$left$labels$at <- tick.at
  ans$left$labels$labels <- c(format(10^(tick.at),
                                     drop0trailing = TRUE,
                                     scientific = FALSE))
  #print(ans$left$labels$labels)
  #######################
  #need to sort of right labeling
  #   dropped for now...
  #ans$right <- ans$left
  ans
}


#lattice panel pal
#based on panel handler in loa

rsp_panelPal <- function(.name, .ls, .panel, ...){
  .x.args <- list(...)
  if(!.name %in% names(.x.args) || !is.logical(.x.args[[.name]]) ||
     .x.args[[.name]]){
    .name2 <- paste("^", .name, "[.]", sep="")
    if(.name %in% names(.x.args) && is.list(.x.args[[.name]])){
      .tmp <- .x.args[[.name]]
      if(length(.tmp)>0){
        names(.tmp) <- paste(.name, names(.tmp), sep=".")
        .x.args <- modifyList(.tmp, .x.args)
      }
    }
    .x.args <- .x.args[grepl(.name2, names(.x.args))]
    if(length(.x.args)>0){
      names(.x.args) <- gsub(.name2, "", names(.x.args))
      .ls <- modifyList(.ls, .x.args)
    }
    do.call(.panel, .ls)
  }
}



# could move this into the function...

rsp_panel.pie <-
  function (x, y=NULL, groups=NULL, subscripts, totals=NULL,
            labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45,
            col = NULL, border = 1, lty = NULL, main = NULL, ...)
  {

    #this is graphics::pie with a couple of modifications...
    #many thanks to...
    #R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation
    #for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.

    #if (!is.numeric(x) || any(is.na(x) | x < 0))
    #    stop("'x' values must be positive.")

    #########################
    #measurement totals
    .y <- totals[subscripts]
    ref <- sapply(unique(groups), function(g){
      sum(.y[groups==g], na.rm=TRUE)
    })
    .total <- mean(ref, na.rm=TRUE)

    ##########################
    #profile contributions to model
    # as percentage of measurements
    ans <- sapply(unique(groups), function(g){
      sum(y[groups==g], na.rm=TRUE)
    })
    ans <- (ans / .total) * 100

    #####################
    #cheat because following comes from
    #pie function in base r...
    x <- ans

    if (is.null(labels))
      labels <- as.character(unique(groups))
    else labels <- as.graphicsAnnot(labels)
    labels = paste(labels, " (",
                   round(ans, digits=1), "%)", sep = "")

    if (any(x == 0)) {
      labels <- labels[x != 0]
      col <- col[x != 0]
      x <- x[x != 0]
    }
    my.tot <- sum(x, na.rm=TRUE)
    ########################
    #this adds extra void area
    #  if does not account for
    #  99 percent of the
    #  measurements
    if (my.tot < 99) {
      x <- c(x, 100 - my.tot)
      labels <- c(labels, "[hide]")
      col <- c(col, NA)
      init.angle <- init.angle + (((100 - my.tot)/200) * 360)
    }
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)

    ######################
    #????
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim

    ########################
    #col setting
    #   this needs generalising like
    #   other pls_plot
    if (is.null(col))
      col <- if (is.null(density))
        c("white", "lightblue", "mistyrose", "lightcyan",
          "lavender", "cornsilk")
    else par("fg")

    ########################
    #border setting
    #   needs generalising...
    if (!is.null(border))
      border <- rep_len(border, nx)

    ##############
    #lty
    #   needs generalising...
    if (!is.null(lty))
      lty <- rep_len(lty, nx)

    ##############
    #angle of segment
    angle <- rep(angle, nx)
    if (!is.null(density))
      density <- rep_len(density, nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    ###########################
    #like to nudge these if percent before and
    #  this one are both small
    #  (making labels close)

    for (i in 1L:nx) {
      if (!as.character(labels[i]) == "[hide]") {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        lattice::lpolygon(c(P$x, rev(P$x * 0.5)), c(P$y, rev(P$y *
                                                               0.5)), density = density[i], angle = angle[i],
                          border = border[1], col = col[i], lty = lty[i])
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
          lattice::llines(c(1, 1.2) * P$x, c(1, 1.2) * P$y)
          lattice::ltext(1.3 * P$x, 1.3 * P$y, labels[i], xpd = TRUE,
                         cex=0.7, adj = ifelse(P$x < 0, 1, 0), ...)
        }
      }
    }
    lattice::ltext(0, 0, label = paste("sum\n", signif(my.tot, 3), "%",
                                       sep = ""), cex=0.7)
  }





#think about
#######################################
# printing amount missing as a segment
# adding plot arg control like in plot.respeciate
# adding args to change the displacement of labels

rsp_profile_pie <- function (x, labels = names(x), edges = 200, radius = 0.8,
                             clockwise = FALSE,
                             init.angle = if (clockwise) 90 else 0,
                             density = NULL, angle = 45, col = NULL,
                             border = NULL, lty = NULL, main = NULL, ...)
{
  #this is graphics::pie with a couple of modifications...
  #many thanks to...
  #R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation
  #for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.

  #print(labels)
  #print(col)

  if (!is.numeric(x) || any(is.na(x) | x < 0))
    stop("'x' values must be positive.")
  if (is.null(labels))
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)

  #added to remove any source with a zero contribution
  #but hold labels and col alignment
  if(any(x==0)){
    labels <- labels[x!=0]
    col <- col[x!=0]
    x <- x[x!=0]
  }
  my.tot <- sum(x)
  if(my.tot < 99){
    x <- c(x, 100-my.tot)
    labels <- c(labels, "[hide]")
    col <- c(col, NA)
    init.angle <- init.angle + (((100-my.tot)/200)*360)
  }

  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L])
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col))
    col <- if (is.null(density))
      c("white", "lightblue", "mistyrose", "lightcyan",
        "lavender", "cornsilk")
  else par("fg")
#  if (!is.null(col))
#    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  if (!is.null(lty))
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density))
    density <- rep_len(density, nx)
  twopi <- if (clockwise)
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }

  for (i in 1L:nx) {

    if(!as.character(labels[i]) == "[hide]"){
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
      #changed shape to include hole
      polygon(c(P$x, rev(P$x*0.5)), c(P$y, rev(P$y*0.5)),
              density = density[i], angle = angle[i],
              border = border[i], col = col[i], lty = lty[i])
      P <- t2xy(mean(x[i + 0:1]))
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        # 1.2 and 1.3 are the extenders when moving labels way from
        # the pie plot itself
        lines(c(1, 1.2) * P$x, c(1, 1.2) * P$y)
        text(1.3 * P$x, 1.3 * P$y, labels[i], xpd = TRUE,
             adj = ifelse(P$x < 0, 1, 0), ...)
      }
    }
  }

  text(0,0, label=paste("sum\n",signif(my.tot, 3), "%", sep=""))
  title(main = main, ...)
  invisible(NULL)
}



###########################
###########################
## pls_refit_species
###########################
###########################


# superseded by pls_fit_species
# now not exported

# need to update the model handling so it is like sp_pls_profile
#     this would sort power issue above
#          also means the user can change setting themselves
#          THINK ABOUT THIS
#               they could make a pls that was not positively constrained


rsp_pls_refit_species <- function(pls, name, power=1,
                              ...){
  .xx <- pls_report(pls)
  #name might want to be case-non-sensitive at some point
  #think about how to do this one...
  .data <- .xx[.xx$SPECIES_NAME==name,]
  #get and hold all the m_ values
  #update profile contributions for named species
  .ms <- names(.data)[grepl("^m_", names(.xx))]
  .xs <- gsub("^m_", "", .ms)
  .for <- paste("(`", .ms, "`*`", .xs, "`)",
                sep="", collapse = "+")
  .for <- as.formula(paste("test~", .for))
  .da <- .data[!names(.data) %in% .xs]


  .ls <- lapply(.xs, function(x){0})
  names(.ls) <- .xs

  #################
  #user might want to set this???

  .ls2 <- lapply(.xs, function(x){.data[1, x]})
  names(.ls2) <- .xs

  mod <- nls(.for, data=.da,
             #weights = 1/(.out$test^push), # think about weighting
             start=.ls2, lower=.ls,
             algorithm="port",
             control=nls.control(tol=1e-5) #think about tolerance
  )

  .data[.xs] <- data.frame(t(coefficients(mod)))

  #lazy
  .ans <- .data

  for(i in .ans$PROFILE_CODE){
    .ii <- subset(.ans, PROFILE_CODE==i)
    .ii <- .ii[names(.ii) %in% names(pls[[i]]$args$data)]
    .sp.ord <- unique(pls[[i]]$args$data$SPECIES_ID)
    pls[[i]]$args$data <- subset(pls[[i]]$args$data, SPECIES_NAME!=name)
    pls[[i]]$args$data <- rbind(pls[[i]]$args$data, .ii)
    #put back in right order
    pls[[i]]$args$data <-
      pls[[i]]$args$data[order(ordered(pls[[i]]$args$data$SPECIES_ID,
                                       levels=.sp.ord)),]
    #rebuild model
    .for <- as.character(formula(pls[[i]]$mod))
    .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
    .ms <- names(pls[[i]]$args$data)
    .ms <- .ms[!.ms %in% c("SPECIES_ID", "SPECIES_NAME", "test")]
    .ls <- lapply(.ms, function(x){0})
    names(.ls) <- paste("m_", .ms, sep="")
    .da <- pls[[i]]$args$data

    pls[[i]]$mod <- nls(.for, data=.da,
                        weights = (1/.da$test)^power, # think about weighting
                        start=.ls, lower=.ls,
                        algorithm="port",
                        control=nls.control(tol=1e-5,
                                            warnOnly = TRUE) #think about tolerance
    )
  }

  invisible(pls)

}



####################################
####################################
## pls_fit_parent
####################################
####################################

# superseded by pls_fit_species
# now now exported

# (like pls_refit_species)
# like to drop power from formals
#   maybe ignore or pass overwrites via ...?

# need to update the model handling so it is like sp_pls_profile
#     this would sort power issue above
#          also means the user can change setting themselves
#          THINK ABOUT THIS
#               they could make a pls that was not positively constrained
#      this would also remove the start, lower and upper options
#           from the formals...

# parent could already be in x
#    then parent could just be the name of parent???

# also a case for using this to add a non-parent to x
#    e.g. pls_fit_unknown_species...
#    to fit a species to the existing model as a source apportion of
#        that unknown...
#    in which case maybe this should just be a wrapper for that
#        with the start, lower and upper like below

# if we are setting start and lower
#     start = lower if start is missing might be safer...


rsp_pls_fit_parent <- function(pls, parent, power=1,
                           start=100,
                           lower=50, upper=200, ...){

  .out <- pls_report(pls)
  #parent should only have one species
  #and have same profiles as pls model data
  #and its contribution to all sources is set by .value

  .out <- subset(.out, SPECIES_ID == unique(.out$SPECIES_ID)[1])
  .test <- c("PROFILE_CODE", ".value", "WEIGHT_PERCENT")
  .test <- names(parent)[names(parent) %in% .test]
  .data <- parent[.test]
  names(.data)[2] <- "parent"
  .data <- merge(.out, .data[c(1:2)])

  #formula
  .ms <- names(.data)[grepl("^m_", names(.out))]
  .for <- paste("(`", .ms, "`*`", gsub("^m_", "n_", .ms), "`)",
                sep="", collapse = "+")
  .for <- as.formula(paste("parent~", .for))

  .ns <- .ms
  names(.ns) <- gsub("^m_", "n_", .ms)
  .ls <- lapply(.ns, function(x){start})
  .ls2 <- lapply(.ns, function(x){lower})
  .ls3 <- lapply(.ns, function(x){upper})

  mod <- nls(.for, data=.data,
             #weights = (1/.out$test)^power, # think about weighting
             start=.ls,
             lower=.ls2,
             upper=.ls3,
             algorithm="port",
             control=nls.control(tol=1e-5) #think about tolerance
  )
  .ans <- data.frame(
    PROFILE_CODE = .data$PROFILE_CODE,
    SPECIES_ID = parent$SPECIES_ID[1],
    SPECIES_NAME = parent$SPECIES_NAME[1],
    t(coefficients(mod)),
    test = .data$parent
  )
  names(.ans) <- gsub("^n_", "", names(.ans))
  for(i in .ans$PROFILE_CODE){
    .ii <- subset(.ans, PROFILE_CODE==i)
    .ii <- .ii[names(.ii) != "PROFILE_CODE"]
    pls[[i]]$args$data <-
      rbind(pls[[i]]$args$data, .ii)
    #rebuild model
    .for <- as.character(formula(pls[[i]]$mod))
    .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
    .ms <- names(pls[[i]]$args$data)
    .ms <- .ms[!.ms %in% c("SPECIES_ID", "SPECIES_NAME", "test")]
    .ls <- lapply(.ms, function(x){0})
    names(.ls) <- paste("m_", .ms, sep="")
    .da <- pls[[i]]$args$data

    pls[[i]]$mod <- nls(.for, data=.da,
                        weights = (1/.da$test)^power, # think about weighting
                        start=.ls, lower=.ls,
                        algorithm="port",
                        control=nls.control(tol=1e-5) #think about tolerance
    )
  }

  pls

}



#previous version of rebuild...


rsp_pls_rebuild.old <- function(pls, species, power=1,
                        refit.profile=TRUE, ...){

  x.args <- list(...)
  #hiding model args

  .out <- pls_report(pls)
  #species / parent should only have one species
  #   note: parent is name from previous function
  #         maybe change now???
  #and have same profiles as pls model data
  #and its contribution to all sources is set by .value

  #note
  ################################
  #following just done quickly to replace
  #    two previous functions pls_fit_parent and pls_refit_species

  if(is.character(species)){
    #assuming this is SPECIES_NAME of the species to be fit
    #and species was in modelled data when pls was built...
    if(!species[1] %in% .out$SPECIES_NAME){
      stop("RSP_PLS> 'species' not in PLS, please check",
           call. = FALSE)
    }
    parent <- subset(.out, SPECIES_NAME == species[1])
    .out <- subset(.out, SPECIES_NAME != species[1])

  } else {
    #assuming this is respeciate object data.frame of right structure
    parent <- species
  }
  #get a 'safe' profile
  #not sure this will work if any sources are not fit to first species
  .test <- .out[.out$pred>0,]
  .out <- subset(.out, SPECIES_ID == unique(.test$SPECIES_ID)[1])
  .test <- c("PROFILE_CODE", ".value", "WEIGHT_PERCENT")
  .test <- names(parent)[names(parent) %in% .test]
  .data <- parent[.test]
  names(.data)[2] <- "parent"
  .data <- merge(.out, .data[c(1:2)])

  ######################
  #for trace
  #add parent to this as m_dummy?
  #that should fit as n_dummy = 1...

  ###########
  #cheat
  #############

  #print(.data)

  #formula
  #changed .out to .data in next line
  .ms <- names(.data)[grepl("^m_", names(.data))]
  .for <- paste("(`", .ms, "`*`", gsub("^m_", "n_", .ms), "`)",
                sep="", collapse = "+")
  .for <- as.formula(paste("parent~", .for))

  .ns <- .ms
  names(.ns) <- gsub("^m_", "n_", .ms)

  #note
  ##################
  #model handling temp update
  #lower, start and upper
  lower <- if("lower" %in% names(x.args)){
    x.args$lower
  } else {
    0
  }
  start <- if("start" %in% names(x.args)){
    x.args$start
  } else {
    lower
  }
  upper <- if("upper" %in% names(x.args)){
    x.args$upper
  } else {
    Inf
  }

  .ls <- lapply(.ns, function(x){start})
  .ls2 <- lapply(.ns, function(x){lower})
  .ls3 <- lapply(.ns, function(x){upper})

  #print(.data)
  #print(.for)

  mod <- nls(.for, data=.data,
             #weights = (1/.out$test)^power,
             #no weighting currently because species are all the same here!
             start=.ls,
             lower=.ls2,
             upper=.ls3,
             algorithm="port",
             control=nls.control(tol=1e-5) #think about tolerance
  )
  .ans <- data.frame(
    PROFILE_CODE = .data$PROFILE_CODE,
    SPECIES_ID = parent$SPECIES_ID[1],
    SPECIES_NAME = parent$SPECIES_NAME[1],
    t(coefficients(mod)),
    test = .data$parent
  )
  names(.ans) <- gsub("^n_", "", names(.ans))

  #print(.ans)

  for(i in .ans$PROFILE_CODE){
    .ii <- subset(.ans, PROFILE_CODE==i)
    .ii <- .ii[names(.ii) != "PROFILE_CODE"]
    .nn <- pls[[i]]$args$data
    .nn <- subset(.nn, !SPECIES_NAME %in% unique(.ii$SPECIES_NAME))
    ###########
    #cheat
    #############
    #print(i)
    #print(names(.nn))
    #print(names(.ii))
    ########################

    pls[[i]]$args$data <- rbind(.nn, .ii)
    #rebuild model
    .for <- as.character(formula(pls[[i]]$mod))
    .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
    .ms <- names(pls[[i]]$args$data)
    .ms <- .ms[!.ms %in% c("SPECIES_ID", "SPECIES_NAME", "test")]
    .ls <- lapply(.ms, function(x){0})
    names(.ls) <- paste("m_", .ms, sep="")
    .da <- pls[[i]]$args$data


    #print(.for)
    #note
    #############################
    # option to not do this refit?
    if(refit.profile){
      pls[[i]]$mod <- nls(.for, data=.da,
                          weights = (1/.da$test)^power, # think about weighting
                          start=.ls, lower=.ls,
                          algorithm="port",
                          control=control #think about tolerance
      )
    }
  }

  pls

}



