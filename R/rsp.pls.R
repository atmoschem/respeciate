#' @name rsp.pls
#' @title Positive Least Squares models
#' @aliases rsp_pls_x pls_report pls_test pls_fit_species
#' pls_refit_species pls_rebuild

#' @description Functions for Positive Least Squares (PSL) fitting of
#' respeciate profiles

#' @description
#' \code{rsp_pls_x} builds PSL models for supplied profile(s) using
#' the \code{\link{nls}} function, the 'port' algorithm and a lower
#' limit of zero for all model outputs to enforce the positive fits. The
#' modeled profiles are typically from an external source, e.g. a
#' measurement campaign, and are fit as a linear additive series of reference
#' profiles, here typically from \code{respeciate}, to provide a measure of
#' source apportionment based on the assumption that the profiles in the
#' reference set are representative of the mix that make up the modeled
#' sample. The \code{pls_} functions work with \code{rsp_pls_x}
#' outputs, and are intended to be used when refining and analyzing
#' these PLS models. See also \code{pls_plot}s for PLS model plots.

#' @param x A \code{respeciate} object, a \code{data.frame} of
#' profiles in standard long form, intended for PLS modelling.
#' @param m A \code{respeciate} object, a \code{data.frame} of
#' profiles also in standard long form, used as the set of candidate
#' source profiles when fitting \code{x}.
#' @param power A numeric, an additional factor to be added to
#' weightings when fitting the PLS model. This is applied in the form
#' \code{weight^power}, and increasing this, increases the relative
#' weighting of the more heavily weighted measurements. Values in the
#' range \code{1 - 2.5} are sometimes helpful.
#' @param ... additional arguments, typically ignored or passed on to
#' \code{\link{nls}}.
#' @param pls A \code{rsp_pls_x} output, intended for use with
#' \code{pls_} functions.
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
#' model is then refit using the revised \code{m} source profile to generate
#' a PLS model based on the revised source profiles (i.e., m + new species
#' or m + refit species). However, this second step can be omitted using
#' \code{refit.profile=FALSE} if you want to use the supplied \code{species}
#' as an indicator rather than a standard member of the apportionment model.
#' @param as.marker for \code{pls_rebuild}, \code{pls_fit_species} and
#' \code{pls_refit_species}, \code{logical}, default \code{FALSE}, when
#' fitting (or refitting) a species, treat it as source marker.
#' @param drop.missing for \code{pls_rebuild}, \code{pls_fit_species} and
#' \code{pls_refit_species}, \code{logical}, default \code{FALSE}, when
#' building or rebuilding a PLS model, discard cases where \code{species}
#' is missing.

################################
# to do...
################################
# link above to pls plot help page?
# document methods and references


#' @return \code{rsp_pls_x} returns a list of nls models, one per
#' profile/measurement set in \code{x}. The \code{pls_} functions work with
#' these outputs. \code{pls_report} generates a \code{data.frame} of
#' model outputs, and is used of several of the other \code{pls_}
#' functions. \code{pls_fit_species}, \code{pls_refit_species} and
#' \code{pls_fit_parent} return the supplied \code{rsp_pls_profile} output,
#' updated on the basis of the \code{pls_} function action.
#' \code{pls_plot}s (documented separately) produce various plots
#' commonly used in source apportionment studies.


#' @note This implementation of PLS applies the following modeling constraints:
#'
#' 1. It generates a model of \code{x} that is positively constrained linear
#' product of the profiles in \code{m}, so outputs can only be
#' zero or more.  Although the model is generated using \code{\link{nls}},
#' which is a Nonlinear Least Squares (NLS) model, the fitting term applied
#' in this case is linear.
#'
#' 2. The model is fit in the form:
#'
#'  \eqn{X_{i,j} = \sum\limits_{k=1}^{K}{N_{i,k} * M_{k,j}  + e_{i,j}}}
#'
#'  Where X is the data set of measurements, input \code{x} in \code{rsp_pls_x},
#'  M (\code{m}) is data set of reference profiles,  and N is the data set of
#'  source contributions, the source apportion solution, to be solved by
#'  minimising e, the error terms.
#'
#' 3. The number of species in \code{x} must be more that the number of
#' profiles in \code{m} to reduce the likelihood of over-fitting.
#'


# GENERAL NOTES

# TO DO
# link to CMB as crude form of CMB and reference?

# these all need code tidying

# check individual function notes


############################
############################
## rsp_pls_profile
############################
############################

##   now importing locally where possible
##   data.table::[function]
##   #' @import data.table

#This is version 2

#version 1 combined version2 and pls_report
#now separated because it simplified pls_ model reworking

#currently keeping the function args
#   might not need to do this BUT
#   model does not seem to be tracking them ...

# check power handling is right

#########################
#think about ?
#########################

# should first arg be rsp.x rather than x or rsp ???

# maybe get formula into docs ???

# maybe split this into rsp.pls and then separate pls. documents ???

#' @rdname rsp.pls
#' @export

rsp_pls_x <- function(x, m, power = 1,
                      ...){

  ##################
  #quick tidy for now
  ##################
  #x <- rsp
  ref <- m
  ######################
  # SPECIEUROPE data
  #can this go???
  ######################
  if("rsp_eu" %in% class(x)){
    x <- .rsp_eu2us(x)
  }
  #######################

  ##################
  #from rough code
  ##################

  ########################
  #only allowing profiles < species
  if(length(unique(ref$.profile.id)) >= length(unique(x$.species.id))){
    stop("rsp_pls: need n.species > n.profiles, more species or less profiles?",
         call. = FALSE)
  }

  x.args <- list(...)

  ####################
  #make sure we only have one species / profile
  ####################
  #tidying
  .pr.cd <- unique(x$.profile.id)
  ##  .xx <- respeciate:::rsp_tidy_profile(x)
  .xx <- lapply(.pr.cd, function(y){
    .x <- x[x$.profile.id==y,]
    .x <- rsp_average_profile(.x, y, .x$.profile[1])
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
  .pr.cd <- unique(.xx$.profile.id)

  ####################
  #reduce ref to just species in x
  ###################
  #no point to look at any species not in x
  ref <- subset(ref, .species.id %in% unique(.xx$.species.id))

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
      .x <- as.data.frame(.xx[.xx$.profile.id==y,])
      .x <- rsp_average_profile(.x, "test", "1_test")

      #might not need one of this-and-same-above
      #might be better doing it here...
      .tmp <- subset(ref, ref$.species.id %in% unique(.x$.species.id))

      #could change this with rbindlist version??
      .ref <- intersect(names(.x), names(.tmp))
      .out <- rbind(.x[.ref], .tmp[.ref])
      .out <- rsp_dcast_profile(.out)

      #build formula and model args
      .tmp <- names(.out)
      .tmp <- .tmp[!.tmp %in% c(".species.id", ".species", "test")]
      names(.out)[names(.out) %in% .tmp] <- paste(".m_", names(.out)[names(.out) %in% .tmp],
                                                  sep="")
      #zero cases for port function
      .ls <- paste(".n_", .tmp, sep="")
      .ls2 <- lapply(.ls, function(x){0})
      names(.ls2) <- .ls
      .for <- paste("(`.n_", .tmp, "`*`.m_", .tmp, "`)", sep="", collapse = "+")
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
      #also switch m_[profile] to n_[profile]
      #   so we have commonly notation...

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

#' @rdname rsp.pls
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


#test
#devtools::load_all()
#d1 <- readRDS("C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\test\\my.working.rds")
#ref <- rsp(c("4868", "4914", "8948", "91155", "91163", "95441", "95529"))
#mod <- rsp_pls_profile(d1, ref, power=2)

pls_report <- function(pls){

  ans <- lapply(names(pls), function(x){
    .xx <- pls[[x]]
    if(!is.null(.xx)){
      .out <- .xx$args$data
      .tmp <- summary(.xx$mod)$coefficients
      .p.mod <- .tmp[,4]
      names(.p.mod) <- gsub(".n_", ".p_", names(.p.mod))
      .out <- data.frame(.profile.id = x,
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
  #working on
  #####################
  #    added x_[profile] (.n_[profile] * .m_[profile]) calculations here
  #       was done on fly in older plots...
  #    also changed m_[profile] to n_[profile] and [profile] to m_[profile]
  #       so annotation was consistent with equation in documentation...
  #    must be a better way of doing this...

  .tmp <- names(ans)
  .tmp <- .tmp[grep("^.m_", .tmp)]

  ans <- as.data.frame(ans)
  for(i in .tmp){
    ans[,gsub("^.m_", ".x_", i)] <- ans[,gsub("^.m_", ".n_", i)] * ans[,i]
  }
  ans <- data.table::as.data.table(ans)
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
  #by-species calculate stats
  #    guessing this could be done in data.table???
  .sp.ref <- unique(ans$.species)
  .sp.ref <- .sp.ref[2:length(.sp.ref)]


  .tmp <- lapply(.sp.ref, function(x){
    .tmp <- subset(ans, .species==x)
    #################
    # note
    #################
    #    was previouslys pred ~ .value
    #         and reported intercept and intercept p
    #
    #print(summary(as.data.frame(.tmp)$.value))
    .mod <- lm(pred ~ 0 + .value, data = as.data.frame(.tmp))
    ###########
    #(also noted in rsp_pls_profile)
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
    if(nrow(.s.mod$coefficients)==0){
      .s.mod$coefficients <- data.frame(NA, NA, NA, NA)
    }
    #to catch cases when no model,
    #      e.g. because no entry in any of the supplied profiles...
    data.frame(.species = x,
               adj.r.sq = .s.mod$adj.r.squared,
               slope = .s.mod$coefficients[1, 1],
               p.slope = .s.mod$coefficients[1, 4],
               AIC = AIC(.mod)
    )
  })
  .tmp <- data.table::rbindlist(.tmp)
  ans <- merge(ans, .tmp, by=".species")

  as.data.frame(ans)
}




#############################
#############################
## pls_test
#############################
#############################

#' @rdname rsp.pls
#' @export

##   now imports from xxx.r
##   #' @import data.table

# this is the model tests
# this builds from pls_report

pls_test <- function(pls){
  .rp <- pls_report(pls)
  #species
  .tmp<- lapply(unique(.rp$.species), function(i){
    .ans <- subset(.rp, .species==i)
    data.frame(.species = i,
               adj.r.sq = .ans$adj.r.sq[1],
               slope=.ans$slope[1],
               p.slope=.ans$p.slope[1],
               AIC = .ans$AIC[1])
  })
  .sp <- data.table::rbindlist(.tmp)

  #pls
  ######################
  # not sure if we should focus on 'good' or 'bad' p-score here...
  .pn <- names(.rp)[grepl("^.p_", names(.rp))]
  .ans <- data.table::as.data.table(.rp)[, lapply(.SD,
                                                  function(x){length(x[x>0.05])/length(x)}),
                                         .SDcols = .pn]
  .ans <- as.data.frame(.ans)
  .ans <- (1 - .ans)*100
  names(.ans) <- gsub("^.p_", "gp_", names(.ans))

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


#' @rdname rsp.pls
#' @export

pls_fit_species <- function(pls, species, power=1,
                            refit.profile=TRUE,
                            as.marker=FALSE,
                            drop.missing=FALSE,
                            ...){
  #wrapper for multiple fits of new data to a pls model
  .id <- unique(species$.species)
  for(i in .id){
    .sub.sp <- subset(species, .species==i)
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



#' @rdname rsp.pls
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



#' @rdname rsp.pls
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
    if(!species[1] %in% .out$.species){
      stop("RSP_PLS> 'species' not in PLS, please check",
           call. = FALSE)
    }
    .add <- subset(.out, .species == species[1])
    .out <- subset(.out, .species != species[1])

  } else {
    #assuming this is respeciate object/data.frame of right structure
    .add <- species
  }

  ###################################
  #get and check species name and id
  ###################################
  sp.nm <- unique(.add$.species)
  sp.id <- unique(.add$.species.id)
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
      if(i %in% unique(.add$.profile.id) & !is.null(pls[[i]])){
        #remark off all print when happy with method
        #print(i)
        #########################
        #can simplify a lot below
        #########################
        x <- pls[[i]]
        .da <- subset(x$args$data, .species != sp.nm)
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
        .mn.df[,ncol(.da)] <- .add[.add$.profile.id==i, ".value"]
        if(!is.na(.mn.df[,ncol(.da)])){


        ##################################
        #a lot below needs more generalising
        ###################################
        pls[[i]]$args$data <- rbind(.da, .mn.df)
        pls[[i]]$args$weights <- (1/pls[[i]]$args$data$test)^power
        if(any(!grepl(.mrk.nm, pls[[i]]$args$formula))){
          #update formula
          .for <- as.character(pls[[i]]$args$formula)
          .for[3] <- paste(.for[3], "+ (`.m_", .mrk.nm,
                           "` * `.n_", .mrk.nm, "`)",
                           sep="")
          pls[[i]]$args$formula <- as.formula(paste(.for[2], .for[1],
                                                    .for[3], sep=""))
        }
        if("start" %in% names(pls[[i]]$args)){
          if(!paste(".n_", .mrk.nm, sep="") %in% names(pls[[i]]$args$start)){
            #print("adding .n_ start")
            .arg <- pls[[i]]$args$start
            .arg[[paste(".m_", .mrk.nm, sep="")]] <-0
            pls[[i]]$args$start <- .arg
          }
        }
        if("lower" %in% names(pls[[i]]$args)){
          if(!paste(".n_", .mrk.nm, sep="") %in% names(pls[[i]]$args$lower)){
            #print("adding .n_ lower")
            .arg <- pls[[i]]$args$lower
            .arg[[paste(".n_", .mrk.nm, sep="")]] <-0
            pls[[i]]$args$lower <- .arg
          }
        }
        if("upper" %in% names(pls[[i]]$args)){
          if(!paste(".n_", .mrk.nm, sep="") %in% names(pls[[i]]$args$upper)){
            #print("adding .n_ upper")
            .arg <- pls[[i]]$args$upper
            .arg[[paste(".n_", .mrk.nm, sep="")]] <- Inf
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
    .out <- .test[!duplicated(.test$.profile.id),]

    .test <- c(".profile.id", ".value", ".pc.weight")
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

    .ms <- names(.data)[grepl("^.n_", names(.data))]
    .for <- paste("(`", .ms, "`*`", gsub("^.n_", ".m_", .ms), "`)",
                  sep="", collapse = "+")
    .for <- as.formula(paste("refit~", .for))

    .ns <- .ms
    names(.ns) <- gsub("^.n_", ".m_", .ms)

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
      .profile.id = .data$.profile.id,
      .species.id = .add$.species.id[1],
      .species.id = .add$.species[1],
      t(coefficients(mod)),
      test = .data$refit,
      check.names=FALSE
    )
    names(.ans) <- gsub("^.n_", "", names(.ans))

    #print("doing these")
    #print(.ans$PROFILE_CODE)

    #for each build model, put new models in pls
    ###################################
    #need to move this to working directly from models
    for(i in unique(.ans$.profile.id)){
      .ii <- subset(.ans, .profile.id==i)
      .ii <- .ii[names(.ii) != ".profile.id"]
      .nn <- pls[[i]]$args$data
      .nn <- subset(.nn, !.species %in% unique(.ii$.species))
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
      .ms <- .ms[!.ms %in% c(".species.id", ".species", "test")]
      .ls <- lapply(.ms, function(x){0})
      names(.ls) <- paste(".n_", .ms, sep="")
      .da <- pls[[i]]$args$data
      pls[[i]]$args$weights <- (1/pls[[i]]$args$data$test)^power
      pls[[i]]$args$control <- control
      #################
      #can these go now..?
      #################
      if("start" %in% names(pls[[i]]$args)){
        if(!paste(".n_", .mrk.nm, sep="") %in% names(pls[[i]]$args$start)){
          #print("adding .n_ start")
          .arg <- pls[[i]]$args$start
          #.arg[[paste(".n_", .mrk.nm, sep="")]] <-0
          pls[[i]]$args$start <- .arg
        }
      }
      if("lower" %in% names(pls[[i]]$args)){
        if(!paste(".n_", .mrk.nm, sep="") %in% names(pls[[i]]$args$lower)){
          #print("adding .n_ lower")
          .arg <- pls[[i]]$args$lower
          #.arg[[paste("m_", .mrk.nm, sep="")]] <-0
          pls[[i]]$args$lower <- .arg
        }
      }
      if("upper" %in% names(pls[[i]]$args)){
        if(!paste(".n_", .mrk.nm, sep="") %in% names(pls[[i]]$args$upper)){
          #print("adding .n_ upper")
          .arg <- pls[[i]]$args$upper
          #.arg[[paste(".n_", .mrk.nm, sep="")]] <- Inf
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
      .test <- names(pls)[!names(pls) %in% unique(.ans$.profile.id)]
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





