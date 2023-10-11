#' @name sp.pls
#' @title (re)SPECIATE profile Positive Least Squares
#' @aliases sp_pls_profile pls_report pls_fit_species
#' pls_plot pls_plot_species pls_plot_profile

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
#' @param species (for \code{pls_fit_species} only) a data.frame of
#' measurements of an additional species to be fitted to the existing
#' PLS model, or the (character class) name of a species already included
#' in the model to be refit.
#' @param refit.profile (for \code{pls_fit_species} only) logical.
#' When fitting a new \code{species} (or refitted an existing \code{species}),
#' all other species in the reference profiles are held 'as is' and
#' \code{species} is fit to the source contribution time-series of the
#' previous PLS model. By default, the full PLS model is then refit
#' using the revised \code{ref} source profile to generate a PLS model
#' based on the revised source profiles (i.e., ref + new species or ref +
#' refit species). However, this second step can be omitted using
#' \code{refit.profile=FALSE} if you want to use the supplied \code{species}
#' as an indicator rather than a standard member of the apportionment model.
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
  #ans$pred[is.na(ans$pred)] <- 0   #this about this..
  ######################
  #thinking about
  ######################
  #    forcing this to y=x??
  #       if so, use code in pls_plot_species
  #            so consistent....
  #    also, moving the lm to the lapply
  #       so lm stats are calculated by-species??
  mod2 <- lm(pred~.value, data=ans)
  ans$adj.r.sq <- summary(mod2)$adj.r.squared
  ans$slope <- summary(mod2)$coefficients[2,1]
  ans$p.slope <- summary(mod2)$coefficients[2,4]
  ans$intercept <- summary(mod2)$coefficients[1,1]
  ans$p.intercept <- summary(mod2)$coefficients[1,4]
  ###########
  #(also noted in sp_pls_profile)
  #if we need to calculate it on a case-by-case basis...
  #need to read this:
  #https://stackoverflow.com/questions/39999456/aic-on-nls-on-r
  #see stats:::logLik.nls for AIC calc...
  ans$AIC <- AIC(mod2)

  as.data.frame(ans)
}






####################################
####################################
## pls_fit_species
####################################
####################################


#' @rdname sp.pls
#' @export

##   now imports from xxx.r
##   #' @import data.table

#############################
#this needs a lot of work
#############################

# thinking about dropping the pls_refit_species and pls_fit_parent
#      (or making them wrappers of this)

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


pls_fit_species <- function(pls, species, power=1,
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
  .test <- .out[.out$pred>0,]
  .out <- subset(.out, SPECIES_ID == unique(.test$SPECIES_ID)[1])
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

  for(i in .ans$PROFILE_CODE){
    .ii <- subset(.ans, PROFILE_CODE==i)
    .ii <- .ii[names(.ii) != "PROFILE_CODE"]
    .nn <- pls[[i]]$args$data
    .nn <- subset(.nn, !SPECIES_NAME %in% unique(.ii$SPECIES_NAME))
    pls[[i]]$args$data <- rbind(.nn, .ii)
    #rebuild model
    .for <- as.character(formula(pls[[i]]$mod))
    .for <- as.formula(paste(.for[2], .for[1], .for[3], sep=""))
    .ms <- names(pls[[i]]$args$data)
    .ms <- .ms[!.ms %in% c("SPECIES_ID", "SPECIES_NAME", "test")]
    .ls <- lapply(.ms, function(x){0})
    names(.ls) <- paste("m_", .ms, sep="")
    .da <- pls[[i]]$args$data

    #note
    #############################
    # option to not do this refit?
    if(refit.profile){
      pls[[i]]$mod <- nls(.for, data=.da,
                          weights = (1/.da$test)^power, # think about weighting
                          start=.ls, lower=.ls,
                          algorithm="port",
                          control=nls.control(tol=1e-5) #think about tolerance
      )
    }
  }

  pls

}


#fix if nulls are an issue
############################

#mod3 <- mod3[unlist(lapply(mod3, function(x) !is.null(x)))]

#test code
####################

#inc <- readRDS("C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\_projects\\marylebone03\\.tmp.increment.rds")
#inc$.value <- inc$.value.inc
#inc$WEIGHT_PERCENT <- inc$.value.inc
#inc$PROFILE_CODE <- as.character(inc$`Start Date`)
#inc$PROFILE_NAME <- as.character(inc$`Start Date`)

#inc <- sp_pad(inc, "species")
#inc <- subset(inc, `Start Date` > "2021-01-01")

#sp_match_profile(inc, spq_pm(), matches=20)

#aa <- sp_profile(c("3157", "4330310", "3941", "4027", "3961"))

#inc.metals <- subset(inc, !grepl("[[]avg.AURN[]]", SPECIES_NAME))

#moda <- sp_pls_profile(inc.metals, aa)
#modb <- sp_pls_profile(inc, aa)

#moda2 <- pls_fit_parent(moda, subset(inc, SPECIES_NAME=="[avg.AURN] PM2.5"))

#moda2i <- pls_fit_species(moda, subset(inc, SPECIES_NAME=="[avg.AURN] PM2.5"))














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


pls_plot <- function(pls, n=1, type=1,...){

  #main summary plot
  #require(data.table)

  #to do
  ############################

  #general

  #tidy code;
  #    this was put together fast...
  #    lots of code can be tidied/simplified...

  #needs external plot control...
  #    arg passing to barplot in ...???
  #    at least col control???

  #type 1
  ############################
  #like x axes to look like a conventionally numeric
  #like option for x axes to be mapped onto another
  #       measurement
  #       currently an index based on PROFILE_CODE
  #            maybe as.x or map.x, etc...


  #type =2 for pie plot...
  #############################
  #using modification of pie
  #      unexported/below
  #      see notes there...


  species <- n

  dat <- pls_report(pls)
  .sp.ref <- unique(dat$SPECIES_NAME)
  if(missing(species)){
    species <- .sp.ref[1]
  }
  if(is.numeric(species)){
    if(all(species == -1)){
      species <- .sp.ref
    } else {
      species <- .sp.ref[species]
    }
  }
  if(!any(species %in% .sp.ref)){
    stop("RSP_PLS> unknown species, please check",
         call.=FALSE)
  }
  .sp.ord <- unique(dat$SPECIES_ID) #only need this if I
  .sp.m.pro <- names(dat)[grep("^m_", names(dat))]
  .sp.pro <- gsub("^m_", "", .sp.m.pro)

  #could put this is pls_report???
  for(i in .sp.pro){
    dat[, paste("x_", i, sep="")] <- dat[, paste("m_", i, sep="")] * dat[,i]
  }

  .sp.x.pro <- names(dat)[grep("^x_", names(dat))]

  .rep <- dat[c("SPECIES_NAME", "SPECIES_ID", "PROFILE_CODE", .sp.x.pro)]
  .rep <- data.table::melt(data.table::as.data.table(.rep),
               id=c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE"))

  .tot <- data.table::as.data.table(dat)
  .cs <- c(".value", "pred", .sp.x.pro)
  .tot <- .tot[, lapply(.SD,
                        function(x) sum(x, na.rm=TRUE)),
               .SDcols= .cs,
               by=c("SPECIES_ID", "SPECIES_NAME")]

  for(i in species){
    .rep2 <- as.data.frame(subset(.rep, SPECIES_NAME==i))
    .rep2$.index <- as.character(.rep2$PROFILE_CODE)
    .tot2 <- as.data.frame(subset(.tot, SPECIES_NAME==i))
    .dat <- subset(dat, SPECIES_NAME==i)
    if(1 %in% type){
      if(all(is.na(.dat$pred)) | all(.dat$pred==0)){
        warning(paste("pls_plot: no type 1 ", i, " model", sep=""),
                call. = FALSE)
      } else {
        .scale <- c(.dat$.value, .dat$pred)
        .scale <- .scale[is.finite((.scale))]
        .scale <- max(pretty(.scale))
        .rep2$.index <- as.numeric(ordered(.rep2$.index, levels=unique(.rep2$.index)))
        .rep2$.index <- ordered(.rep2$.index, levels=unique(.rep2$.index))
        #.cols <- palette.colors(n = length(.sp.x.pro), palette = "Okabe-Ito",
        #                        recycle = FALSE)
        .cols <- heat.colors(n=length(.sp.x.pro))
        .ncol <- ceiling(length(.sp.x.pro)/3)
        .leg.text <- gsub("^x_", "", .sp.x.pro)
        .bar <- barplot(value~variable + .index,  .rep2, col=.cols,
                        main = i, ylab="Measurement",
                        xlab="Sample [index]",
                        ylim=c(0, .scale),   #testing
                        legend.text=.leg.text,
                        args.legend=list(cex=0.8,
                                         ncol=.ncol))
        .dat <- subset(dat, SPECIES_NAME==i)

        lines(.bar, .dat$.value)
      }
    }
    if(2 %in% type){
        .cols <- heat.colors(n=length(.sp.x.pro))
        if(nrow(.tot2)==0){
          warning(paste("pls_plot: no type 2 ", i, " model", sep=""),
                  call. = FALSE)
        } else {
          .vals <- unlist((.tot2[,.sp.x.pro]/.tot2$.value) * 100)
          if(all(.vals==0)){
            warning(paste("pls_plot: no type 2 ", i, " model", sep=""),
                    call. = FALSE)
          } else {
            .labs=paste(gsub("x_", "", .sp.x.pro), " (", signif(.vals, 3) , "%)", sep="")
            rsp_profile_pie(.vals, col=.cols, edges=1000,
                            labels=.labs, main=i)
          }
        }
      }
  }

  #output
  #not sure if dat, .rep or output is most useful??
  invisible(.rep)
}


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


pls_plot_species <- function(pls, n, type=1, ...){

  #various pls model plots for species

  #general
  ######################################

  #tidy code;
  #    this was put together fast...
  #    lots of code can be tidied/simplified...

  #needs external plot control...
  #    arg passing to barplot in ...???
  #    at least col control???

  species <- n

  #get (and work from) report
  dat <- pls_report(pls)

  #species handling/erroring
  .sp.ref <- unique(dat$SPECIES_NAME)
  if(missing(species)){
    species <- .sp.ref[1]
  }
  if(is.numeric(species)){
    if(all(species == -1)){
      species <- .sp.ref
    } else {
      species <- .sp.ref[species]
    }
  }
  if(!any(species %in% .sp.ref)){
    stop("RSP_PLS> unknown species, please check",
         call.=FALSE)
  }
  for(i in species){
    d2 <- subset(dat, SPECIES_NAME==i)
    lims <- range(c(d2$.value, d2$pred))
    mod <- lm(pred~0+.value, d2)
    .sum <- paste("y = ", signif(summary(mod)$coefficients[1,1], 3),
                  "x (adj.R2 = ", signif(summary(mod)$adj.r.squared, 3),
                  ")", sep="")
    if(1 %in% type){
      plot(d2$.value, d2$pred, type="n",
           main=i,
           xlab="Measurement",
           ylab="Model",
           xlim=lims, ylim=lims)
      grid()
      abline(a=0, b=1, col="grey")
      points(d2$.value, d2$pred)
      abline(mod, col="red", lty=2)
      text(lims[1], lims[2], .sum, adj=c(0,1), cex=0.75)
    }
    if(2 %in% type){
      plot(d2$.value, type="n",
           main=i,
           ylab="Measurement",
           xlab="Sample [index]",
           ylim=lims)
      lines(d2$.value)
      lines(d2$pred, col="red")
    }
  }
  invisible(NULL)
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


pls_plot_profile <- function(pls, n, log=FALSE, ...){

  #general

  #tidy code;
  #    this was put together fast...
  #    lots of code can be tidied/simplified...

  #needs external plot control...
  #    arg passing to barplot in ...???
  #    at least col control???

  #type
  ############################
  #(currently only one so not needed...)
  #

  #log logical ONLY
  ############################
  #   very fiddly...
  #

  #require(data.table)

  profile <- n

  dat <- pls_report(pls)
  #need to reset species order after calculating stats
  #  think about making first half of this function a dedicated function??
#print(unique(dat$SPECIES_NAME))
  #need to think this through
  #  there is a lot here that is very painful...

  #could pass plot commands via the ... args

  .sp.ord <- unique(dat$SPECIES_ID)
#print(unique(.sp.ord))
  .sp.m.pro <- names(dat)[grep("^m_", names(dat))]
  .sp.pro <- gsub("^m_", "", .sp.m.pro)
  if(missing(profile)){
    profile <- .sp.pro[1]
  }
  if(is.numeric(profile)){
    if(all(profile == -1)){
      profile <- .sp.pro
    } else {
      profile <- .sp.pro[profile]
    }
  }
  if(!any(profile %in% .sp.pro)){
    stop("RSP_PLS> unknown profile(s), please check",
         call.=FALSE)
  }
  m_profile <- paste("m_", profile, sep="")

  #just profiles asked for
  dat <- dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE",
               profile, m_profile, "pred", ".value")]
  #calculate percentage of species
  for(i in profile){
    dat[, paste("x_", i, sep="")] <- dat[, paste("m_", i, sep="")] * dat[,i]
  }
  .rep <- data.table::as.data.table(dat)
  .cols <- c(".value", "pred", paste("x_", profile, sep=""))
  .rep <- .rep[, lapply(.SD, function(x) sum(x, na.rm=TRUE)),
               .SDcols= .cols, by=c("SPECIES_ID", "SPECIES_NAME")]
  .rep <- as.data.frame(.rep)

  for(i in profile){
    .rep[, paste("pc_", i, sep="")] <- (.rep[, paste("x_", i, sep="")] / .rep$pred) * 100
  }

  #dat <- dat[dat$PROFILE_CODE==unique(dat$PROFILE_CODE)[1],]
  #   replace above because first profile does not contain all species
  dat <- dat[!duplicated(dat$SPECIES_NAME),]
  dat$PROFILE_NAME <- dat$PROFILE_NAME[1]
  dat$PROFILE_CODE <- dat$PROFILE_CODE[1]

  dat <- merge(.rep, dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE", profile)],)
#print(unique(dat$SPECIES_NAME))

  dat <- dat[order(ordered(dat$SPECIES_ID, levels=.sp.ord)), ]
  rownames(dat) <- 1:nrow(dat)

#print(unique(dat$SPECIES_NAME))
  .old.par <- par(no.readonly = TRUE)
  for(i in profile){
    d2 <- dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE",
                i, paste("pc_", i, sep=""))]

    if(log){
      #plotting first axis as logs
      .f.cex = 0.8
      .x <- d2$SPECIES_NAME
      .m.x <- max(nchar(.x), na.rm=T)/1.3*.f.cex
      .y <- d2[,i]
      .min <- min(.y[.y>0], na.rm=TRUE)/2
      .y[.y<.min] <- .min
      .y[is.na(.y)] <- .min
      .y <- log10(.y)
      .y1.at <- pretty(.y)
      .y1.at <- .y1.at[.y1.at == round(.y1.at)]
      #need to go back in and recalculate min as
      #bottom of pretty...
      .y[.y==log10(.min)] <- .y1.at[1]

      #.y1.lb <- as.character(10^(.y1.at))
      .y1.lb <- c(format(10^(.y1.at), drop0trailing = TRUE, scientific=FALSE))
      .y <- .y - min(.y1.at)
      .y1.at <- .y1.at - min(.y1.at)

      .yl <- "Source Loading"
      .y2 <- d2[, paste("pc_", i, sep="")]
      .y2 <- ((.y2/100) * (max(.y1.at) - min(.y1.at))) + min(.y1.at)
      .y2.lb <- seq(0, 100, by=20)
      .y2.at <- ((.y2.lb/100) * (max(.y1.at) - min(.y1.at))) + min(.y1.at)

    } else {
      #plotting first axis as is
      .f.cex = 0.8
      .x <- d2$SPECIES_NAME
      .m.x <- max(nchar(.x), na.rm=T)/1.3*.f.cex
      .y <- d2[,i]
      .y1.at <- pretty(.y)
      .y1.lb <- .y1.at
      .yl <- "Source Loading"
      .y2 <- d2[, paste("pc_", i, sep="")]
      .y2 <- ((.y2/100) * (max(.y1.at) - min(.y1.at))) + min(.y1.at)
      .y2.lb <- seq(0, 100, by=20)
      .y2.at <- ((.y2.lb/100) * (max(.y1.at) - min(.y1.at))) + min(.y1.at)

    }

    #lims <- range(d2[,4], na.rm=TRUE)
    #.min <- min(d2[,4][d2[,4]>0], na.rm=TRUE)


    par(mar = c(.m.x, 4, 4, 4) + 0.3)
    .bar <- barplot(.y,
                    names.arg=.x,
                    ylim = range(.y1.at),
                    main=i,
                    cex.lab=.f.cex,
                    cex.names=.f.cex,
                    axes=FALSE,
                    ylab=.yl,
                    las=2
    )
    points(.bar, .y2, col="red", pch=19)
    axis(2, ylim=range(.y1.at), at=.y1.at, labels=.y1.lb, cex=.f.cex,
         cex.axis=.f.cex, las=2)
    axis(4, ylim=range(0, max(.y2.at)), at=.y2.at, labels=.y2.lb,
         col="red", col.axis="red", cex=.f.cex,
         cex.axis=.f.cex, las=2)
    mtext("Percent of Total Contribution (%)", side=4, line=3, col ="red",
          cex=.f.cex)

  }

  par(.old.par)

  invisible(dat)
}













################
################
## unexported
################
################


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


pls_refit_species <- function(pls, name, power=1,
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


pls_fit_parent <- function(pls, parent, power=1,
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



