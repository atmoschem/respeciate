#' @name respeciate.generics
#' @title respeciate.generics
#' @importFrom graphics axis barplot par

#' @description respeciate object classes and generic functions

#reversed order of documentation,
#started with the find function...


#' @description When supplied a \code{respeciate}
#' object, \code{\link{print}} manages its appearance.
#' @param x the \code{respeciate}
#' object to be printed, plotted, etc.
#' @param n when plotting or printing a multi-profile, the
#' number(s) of profile(s) to report.
#' @param ... any extra arguments, mostly ignored except by
#' \code{plot} which passes them to \code{\link{barplot}}.
#' @rdname respeciate.generics
#' @method print respeciate
#' @export

print.respeciate <-
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

#' @description When supplied a \code{respeciate}
#' object, \code{\link{plot}} provides a basic plot
#' output. This uses base function \code{\link{barplot}};
#' also see note.
#' @note \code{respeciate} objects revert to
#' \code{data.frame}s when not doing anything
#' package-specific, so you can still
#' use as previously with \code{lattice} or
#' \code{ggplot2}, useful if you are pulling multiple
#' profiles and you exceed the base \code{\link{barplot}}
#' capacity...
#' @param id numeric, indices of profiles to use when
#' plotting (nb: \code{n=6} is equivalent to code{id=1:6}).
#' @param order logical, order the species in the
#' profile(s) by relative abundance before plotting.
#' @rdname respeciate.generics
#' @method plot respeciate
#' @export

plot.respeciate <-
  function(x, n=NULL, id=NULL, order=TRUE, ...){

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

    x <- rsp_test(x)
    if(any(x$COUNT>1)){
      warning(paste("\n\t",
                    " found duplicate profiles (merged and averaged...)",
                    "\n", sep=""))
    }

    x$SPECIES_NAME <- rsp_tidy_species_name(x$SPECIES_NAME)

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
      test <- rsp_test(test)
      if("beside" %in% names(.xargs) && .xargs$beside){
        test <- x[order(x$WEIGHT_PERCENT, decreasing = TRUE),]
        xx <- unique(test$SPECIES_NAME)
      } else {
        test <- test[order(test$TOTAL, decreasing = TRUE),]
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






#' @description When supplied a \code{respeciate.ref}
#' object, \code{\link{print}} manages its appearance.
#' @param x the \code{respeciate} or \code{respeciate.ref}
#' object to be printed, plotted, etc.
#' @rdname respeciate.generics
#' @method print respeciate.ref
#' @export
print.respeciate.ref <-
  function(x, ...){
    xx <- nrow(x)
    wi <- getOption("width")
    ####################################
    #use of cat might need rethinking?
    ####################################
    cat("respeciate profile reference\n")
    if(xx>100){
      report <- c(x$PROFILE_CODE[1:100], "...")
      comment <- " profiles [showing first 100]\n"
    } else {
      report <- x$PROFILE_CODE
      comment <- " profiles\n"
    }
    if(xx>0) cat(report, fill=wi)
    cat("   > ", xx, comment, sep="")
    invisible(x)
  }


#' @rdname respeciate.generics
#' @method print respeciate.spcs
#' @export
print.respeciate.spcs <-
  function(x, ...){
    xx <- nrow(x)
    wi <- getOption("width")
    ####################################
    #use of cat might need rethinking?
    ####################################
    cat("respeciate species\n")
    if(xx>10){
      report <- c(x$SPECIES_NAME[1:10], "...")
      comment <- " species [showing first 10]\n"
    } else {
      report <- x$SPECIES_NAME
      comment <- " species\n"
    }
    if(xx>0) cat(report, fill=wi)
    cat("   > ", xx, comment, sep="")
    invisible(x)
  }




#################################
#unexported code
#################################

###################################
#class builds
###################################

rsp_build_respeciate.spcs <-
  function(x, ...){
    #build
    class(x) <- c("respeciate.spcs", "data.frame")
    x
  }

rsp_build_respeciate.ref <-
  function(x, ...){
    #build
    class(x) <- c("respeciate.ref", "data.frame")
    x
  }

rsp_build_respeciate <-
  function(x, ...){
    #build
    class(x) <- c("respeciate", "data.frame")
    x
  }

###########################
#tidy names
###########################

#currently not exported
#quick code to tidy species names

#note: not fully tested

rsp_tidy_species_name <- function(x){

  #attempts shorten names by remove other versions
  #names seem to be in format a (or b || c)
  #where (guessing) a is main name and
  #         b and c are alternatives.

  #not fully tested,
  #   might still be more cases this dies on

  #gsub("[(].*","", x) failed if name a includes brackets
  #example:#"(2-methylpropyl)benzene (or isobutylbenzene)"

  #sub("[(][^(]or+$", "", x) failed if b or c includes brackets
  #also left space at end so needed sub("^\\s+|\\s+$", "", x)

  #sometimes it is "( or "
  x <- gsub(" [(] or ", " (or ", x)
  #next removes from last "(or" onwards
  x <- gsub("[(]or .*","", x)
  sub("^\\s+|\\s+$", "", x)
}


###########################
#split respeciate by profile
###########################

#currently not exported
#quick code assumed CODE is unique to profile

rsp_split_profile <- function(x){
  ref <- unique(x$PROFILE_CODE)
  lapply(ref, function(y) x[x$PROFILE_CODE==y,])
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

rsp_test <- function(x){
  .prf <- unique(x$PROFILE_CODE)
  ans <- lapply(.prf, function(y){
    temp <- subset(x, PROFILE_CODE==y)
    .spc <- unique(temp$SPECIES_ID)
    ans <- lapply(.spc, function(z){
      temp2 <- subset(temp, SPECIES_ID==z)
      data.frame(PROFILE_CODE = y,
                 PROFILE_NAME = temp2$PROFILE_NAME[1],
                 SPECIES_ID = z,
                 SPECIES_NAME = temp2$SPECIES_NAME[1],
                 COUNT = length(temp2$WEIGHT_PERCENT[!is.na(temp2$WEIGHT_PERCENT)]),
                 TOTAL = sum(temp2$WEIGHT_PERCENT[!is.na(temp2$WEIGHT_PERCENT)]),
                 SPEC_MW = temp2$SPEC_MW[1],
                 WEIGHT_PERCENT=mean(temp2$WEIGHT_PERCENT, na.rm=TRUE))
    })
    do.call(rbind, ans)
  })
  do.call(rbind, ans)
}




