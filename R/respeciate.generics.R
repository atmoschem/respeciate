#' @name respeciate.generics
#' @title respeciate.generics
#' @description \code{respeciate} object classes and generic functions.

########################
#might move all the text to top
#    hard to keep style consistent when docs are in between
#    multiple functions

#' @description When supplied a \code{respeciate}
#' object, \code{\link{print}} manages its appearance.
#' @description When supplied a \code{respeciate}
#' object, \code{\link{plot}} provides a basic plot
#' output. This uses base function \code{\link{barplot}};
#' also see note.
#' @param x the \code{respeciate}
#' object to be printed, plotted, etc.
#' @param n when plotting or printing a multi-profile object, the
#' maximum number of profiles to report. (When plotting, \code{n}
#' is ignored if \code{id} is also set.)
#' @param ... any extra arguments, mostly ignored except by
#' \code{plot} which passes them to \code{\link{barplot}}.
#' @param object like \code{x} but for \code{summary}.
#' @param id numeric, indices of profiles to use when
#' plotting (\code{id=1:6} is equivalent to \code{n=6}).
#' @param order logical, order the species in the
#' profile(s) by relative abundance before plotting.
#' @note \code{respeciate} objects revert to
#' \code{data.frame}s when not doing anything
#' package-specific, so you can still
#' use as previously with \code{lattice} or
#' \code{ggplot2}, useful if you are pulling multiple
#' profiles and you exceed the base \code{\link{barplot}}
#' capacity...


#notes
##################################

#loosing the respeciate.ref and respeciate.spcs classes
#    testing merge them into respeciate

#         with different print outputs?
#         only plot for class plot, etc???

#' @rdname respeciate.generics
#' @method print respeciate
#' @export

print.respeciate <- function(x, n = NULL, ...){
  test <- rsp_test_respeciate(x, level = 2, silent = TRUE)
  if(test == "respeciate.profile.ref"){
    if(is.null(n)){
      n <- 100
    }
    return(rsp_print_respeciate_profile(x=x, n=n, ...))
  }
  if(test == "respeciate.species.ref"){
    if(is.null(n)){
      n <- 10
    }
    return(rsp_print_respeciate_species(x=x, n=n, ...))
  }
  if(is.null(n)){
    n <- 10
  }
  rsp_print_respeciate(x=x, n=n, ...)
}

## rsp_print functions unexported
##    further down
##    VVVVVVVVVVVV
##    VVVVVVVVVVVV







#' @rdname respeciate.generics
#' @method plot respeciate
#' @export

##########################
#notes
##########################
#like....
#better handling of factor axis labels
#better handling of axes and legend font sizes
#    (I think previous may have handled this a little better)
#    (but not perfectly...)
#like horiz=total scales to be other way around?
#    could also mean rethinking the legend position for

############################
#added warning/handling for
#  duplicate species in profiles (handling merge/mean)
#  duplicated profile names (handling make unique)
#but might want data.table or dplyr
#  to handle test because it'll really slow
#  plots down


plot.respeciate <-
  function(x, n=NULL, id=NULL, order=TRUE, ...){

    #add .value if not there
    ## don't think .value works
    x <- rsp_tidy_profile(x)

    ##test object type
    test <- rsp_test_respeciate(x, level=2, silent=TRUE)
    if(test != "respeciate"){
      if(test %in% c("respeciate.profile.ref", "respeciate.species.ref")){
        stop("No plot method for respeciate.reference files.")
      } else {
        stop("suspect respeciate object!")
      }
      #don't stop - respeciate profile
    }

    ##test something to plot
    if(nrow(x)==0){
      ######################
      #think about this
      ######################
      #maybe stop() instead???
      #stop("empty respeciate object?")
      return(invisible(NULL))
    }

    #hold extra args
    #  passing to plot
    .xargs <- list(...)

    #test number of profiles
    #and subset x, etc...
    test <- unique(x$PROFILE_CODE)
    if(is.null(n) & is.null(id)){
      id <- 1:length(test)
    } else {
      if(!is.null(n)){
        id <- 1:n
      }
    }
    test <- test[id]
    x <- x[x$PROFILE_CODE %in% test,]
    #above will die if n-th profile not there
    if(length(n)>6){
      warning(paste("\n\t", length(test),
                    " profiles (might be too many; suggest 6 or less...)",
                    "\n", sep=""))
    }

    x <- rsp_test_profile(x)


    if(any(x$.n>1)){
      warning(paste("\n\t",
                    " found duplicate species in profiles (merged and averaged...)",
                    "\n", sep=""))
    }
    x$SPECIES_NAME <- rsp_tidy_species_name(x$SPECIES_NAME)

    ####################################
    #issue profile names are not always unique
    ####################################
    test <- x
    test$SPECIES_ID <- ".default"
    test <- rsp_test_profile(test)
    ###################
    #rep_test
    #can now replace this with data.table version
    #BUT check naming conventions for .n
    ###################

    #does this need a warning?
    if(length(unique(test$PROFILE_NAME))<nrow(test)){
      warning(paste("\n\t",
                    " found profiles with common names (making unique...)",
                    "\n", sep=""))
      test$PROFILE_NAME <- make.unique(test$PROFILE_NAME)
      x <- x[names(x) != "PROFILE_NAME"]
      x <- merge(x, test[c("PROFILE_NAME", "PROFILE_CODE")], by="PROFILE_CODE")
    }


    #x$PROFILE_NAME <- make.unique(x$PROFILE_NAME)

    #order largest to smallest
    #############################
    #like to also be able to order by molecular weight
    ##############################
    if(order){
      ################################
      #bit of a cheat...
      ################################
      test <- x
      test$PROFILE_CODE <- ".default"
      test <- rsp_test_profile(test)
      if("beside" %in% names(.xargs) && .xargs$beside){
        test <- x[order(x$WEIGHT_PERCENT, decreasing = TRUE),]
        xx <- unique(test$SPECIES_NAME)
      } else {
        test <- test[order(test$.total, decreasing = TRUE),]
        xx <- unique(test$SPECIES_NAME)
      }
    } else {
      xx <- unique(x$SPECIES_NAME)
    }
    x <- x[c("WEIGHT_PERCENT", "PROFILE_NAME", "SPECIES_NAME")]

    x$SPECIES_NAME <- factor(x$SPECIES_NAME,
                             levels = xx)

    .xargs$formula <- WEIGHT_PERCENT~PROFILE_NAME+SPECIES_NAME
    .xargs$data <- x
    .xargs$las <- 2
    .xargs$legend <- TRUE
    if(!"xlab" %in% names(.xargs)){
      .xargs$xlab <- ""
    }
    if(!"ylab" %in% names(.xargs)){
      .xargs$ylab <- ""
    }
    #################################
    #would like better control of the
    #factor axis font size
    #and graphical white space
    #################################
    if(!"cex.names" %in% names(.xargs)){
      .xargs$cex.names <- 0.5
    }
    ##################################
    #would like better legend handling
    ##################################
    if(!"args.legend" %in% names(.xargs)){
      .xargs$args.legend <- list()
    }
    if(!"cex" %in% names(.xargs$args.legend)){
      .xargs$args.legend$cex <- 0.5
    }

    #and shuffle so it always leads with formula for right method...
    .xargs <- .xargs[unique(c("formula", names(.xargs)))]

    #plot
    do.call(barplot, .xargs)

  }


##################################
#summary
##################################

#like something to get us to...
#database.summary
#summary.respeciate(sysdata$PROFILES)

#' @rdname respeciate.generics
#' @method summary respeciate
#' @export

summary.respeciate <-
  function(object, ...){
    #v0.1 summary
    n <- object$PROFILE_TYPE
    n <- n[!duplicated(object$PROFILE_CODE)]
    summary(factor(n))
}




#################################
#unexported code
#################################

###################################
#class builds
###################################

#rsp_build_respeciate.spcs <-
#  function(x, ...){
    #build
    #add .value
#    x <- rsp_tidy_profile(x)
#    class(x) <- c("respeciate.spcs", "data.frame")
#    x
#  }

#rsp_build_respeciate.ref <-
#  function(x, ...){
    #build
#    class(x) <- c("respeciate.ref", "data.frame")
#    x
#  }

rsp_build_respeciate <-
  function(x, ...){
    #build
    class(x) <- c("respeciate", "data.frame")
    x
  }


###########################
#split respeciate by profile
###########################

#currently not exported
#quick code assumed CODE is unique to profile

#need to test this

#not sure we are using this any more
#    i think rsp_test, then rsp_test.2 replaced
#    and code in plot.respeciate.old ???

rsp_split_profile <- function(x){
  ref <- unique(x$PROFILE_CODE)
  lapply(ref, function(y) x[x$PROFILE_CODE==y,])
}


########################
#unexported rsp_print
########################

## like to tidy this/these


rsp_print_respeciate <-
  function(x, n=6, ...){
    #shifted from REF_Code to PROFILE_Code
    #   when allowing multipe profiles
    #   because REF_Code not unique
    y <- unique(x$PROFILE_CODE)
    report <- paste("respeciate profile(s): count ",
                    length(y), "\n", sep="")
    if(length(y)==0){
      cat(paste(report,
                "empty (or bad?) respeciate object\n",
                sep=""))
      return(invisible(x))
    }
    yy <- if(length(y)>n) {y[1:n]} else {y}
    for(i in yy){
      report <- paste(report, "  ", i, " (checksum: ",
                      sum(as.numeric(as.character(x[x$PROFILE_CODE==i,]$WEIGHT_PERCENT)),
                          na.rm = T), ")\n", sep="")
    }
    if(length(y)>n){
      report <- paste(report, "  ... not showing last ",
                      length(y)-n, "\n", sep="")
    }
    cat(report, sep="")
    invisible(x)
  }


## #' @description When supplied a \code{respeciate.ref}
# #' object, \code{\link{print}} manages its appearance.
# #' @param x the \code{respeciate} or \code{respeciate.ref}
# #' object to be printed, plotted, etc.
# #' @rdname respeciate.generics
# #' @method print respeciate.ref
# #' @export
rsp_print_respeciate_profile <-
  function(x, n = 100, ...){
    xx <- nrow(x)
    wi <- getOption("width")
    ####################################
    #use of cat might need rethinking?
    ####################################
    cat("respeciate profile reference\n")
    if(n>xx){
      n <- xx
    }
    if(xx>n){
      report <- c(x$PROFILE_CODE[1:n], "...")
      comment <- paste(" profiles [showing first ", n,
                       "]\n", sep = "")
    } else {
      report <- x$PROFILE_CODE
      comment <- " profiles\n"
    }
    if(xx>0) cat(report, fill=wi)
    cat("   > ", xx, comment, sep="")
    invisible(x)
  }


# #' @rdname respeciate.generics
# #' @method print respeciate.spcs
# #' @export
rsp_print_respeciate_species <-
  function(x, n = 10, ...){
    xx <- nrow(x)
    wi <- getOption("width")
    ####################################
    #use of cat might need rethinking?
    ####################################
    cat("respeciate species reference\n")
    if(n>xx){
      n <- xx
    }
    if(xx>n){
      report <- c(x$SPECIES_NAME[1:n], "...")
      comment <- paste(" species [showing first ", n,
                       "]\n", sep="")
    } else {
      report <- x$SPECIES_NAME
      comment <- " species\n"
    }
    if(xx>0) cat(report, fill=wi)
    cat("   > ", xx, comment, sep="")
    invisible(x)
  }












#########################
#plot old
#########################

#alternative to above plot.respeciate
#working on this handle multiple profiles...

#now replacing previous plot.respeciate

plot.respeciate.old <-
  function(x, n=NULL, order=TRUE, ...,
           legend.text=NULL,
           args.legend = NULL){

    #test number of profiles
    #and subset x, etc...
    test <- unique(x$PROFILE_CODE)
    if(is.null(n)) n <- 1:length(test)
    test <- test[n]
    x <- x[x$PROFILE_CODE %in% test,]
    #above will die if n-th profile not there
    if(length(n)>6){
      warning(paste("\n\t", length(test),
                    " profiles (might be too many; suggest 6 or less...)",
                    "\n", sep=""))
    }
    test.names <- make.unique(sapply(test,
                                     function(y) subset(x,
                                                        PROFILE_CODE==y)$PROFILE_NAME[1]))

    #check anything left to work with
    if(length(test)==0){
      stop("empty (or bad) respeciate object?")
    }

    #assuming multiple profiles
    #build common data (could use dplyr)
    x <- x[c("PROFILE_NAME", "PROFILE_CODE",
             "SPECIES_NAME", "SPECIES_ID", "SPEC_MW",
             "WEIGHT_PERCENT")]
    x <- rsp_split_profile(x)
    x <- suppressWarnings(Reduce(function(x, y)
      merge(x=x, y=y,
            by=c("SPECIES_ID", "SPECIES_NAME",
                 "SPEC_MW"),
            all.x=T, all.y=T), x)
    )
    #in case names not unique
    names(x) <- make.names(names(x), unique=TRUE)

    #order largest to smallest
    if(order){
      temp <- names(x)[grep("WEIGHT_PERCENT", names(x))]
      temp <- apply(x[temp], 1,
                    function(y) sum(y, na.rm=TRUE))
      x <-x[rev(order(temp)),]
    }

    #prepare plot
    xx <- rsp_tidy_species_name(x$SPECIES_NAME)
    x <- x[grep("WEIGHT_PERCENT", names(x))]
    x[is.na(x)] <- 0
    #########################
    #above kills log but seems to be needed
    #or we loose all records of one species if any are NA
    b <- as.matrix(t(x))

    #below now handled later
    #if("beside" %in% names(list(...)) &&
    #        list(...)$beside){
    #  #need to replace this with something nicer
    #  temp <- rep(NA, length(xx) * length(n))
    #  temp[(1:length(xx))*length(n)] <- xx
    #  xx <- temp
    #}

    #plot legend handling
    #could simplify this
    if(is.null(legend.text)){
      legend.text <- test.names
    }
    if(is.null(args.legend)){
      args.legend <- list()
    }
    if(!"cex" %in% names(args.legend)){
      args.legend$cex <- 0.5
    }
    if(!"x" %in% names(args.legend)){
      args.legend$x <- "topright"
    }

    #need to do plot differently if horiz(ontal)
    if("horiz" %in% names(list(...)) &&
       list(...)$horiz){
      #set up y annotation
      ref <- max(nchar(xx), na.rm=TRUE) * 0.25
      if(ref>10) ref <- 10 #stop it getting silly with x names
      op <- par(mar=c(2,ref,4,2))
      #plot standard
      b <- barplot(b, yaxt="n", #space=0.5,
                   legend.text=legend.text,
                   args.legend =args.legend,
                   ...)
      if(is.matrix(b)){
        b <- apply(b, 2, function(x) mean(x, na.rm=T))
      }
      axis(2, at=b, labels=xx, las=2, tick=FALSE, cex.axis=0.5)
      rm(op)
    } else {
      #set up x annotation
      ref <- max(nchar(xx), na.rm=TRUE) * 0.25
      if(ref>10) ref <- 10 #stop it getting silly with x names
      op <- par(mar=c(ref,4,4,2))
      #plot standard
      b <- barplot(b, xaxt="n", #space=0.5,
                   legend.text=legend.text,
                   args.legend = args.legend,
                   ...)
      if(is.matrix(b)){
        b <- apply(b, 2, function(x) mean(x, na.rm=T))
      }
      axis(1, at=b, labels=xx, las=2, tick=FALSE, cex.axis=0.5)
      rm(op)
    }
  }





