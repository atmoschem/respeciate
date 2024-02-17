#' @name respeciate.generics
#' @title respeciate.generics
#' @description \code{respeciate} object classes and generic functions.

########################
#might move all the text to top
#    hard to keep style consistent when docs are in between
#    multiple functions

#' @description When supplied a \code{respeciate}
#' object or similar, \code{\link{print}} manages its appearance.
#' @description When supplied a \code{respeciate}
#' object, \code{\link{plot}} provides a basic plot
#' output. This uses base function \code{\link{barchart}};
#' also see note.
#' @param x the \code{respeciate}
#' object to be printed, plotted, etc.
#' @param n when plotting or printing a multi-profile object, the
#' maximum number of profiles to report.
#' @param ... any extra arguments, mostly ignored except by
#' \code{plot} which passes them to \code{\link{sp_plot_profile}}.
#' @param object like \code{x} but for \code{summary}.
#' @note \code{respeciate} objects revert to
#' \code{data.frame}s when not doing anything
#' package-specific, so you can still
#' use as previously with \code{lattice} or
#' \code{ggplot2}, useful if you are pulling multiple
#' profiles and you exceed the base \code{\link{barchart}}
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



#############################################
#############################################
## print.rsp_pls
#############################################
#############################################

#' @rdname respeciate.generics
#' @method print rsp_pls
#' @export

# notes
############################

#currently just a print tidier
#    so user not catching this does not get a
#        screen dump of model lists

#needs work if we want it to actually be useful...

print.rsp_pls <- function(x, n = NULL, ...){
  #expecting list of nls models
  report <- "respeciate pls model:"
  if(!is.list(x)){
    report <- paste(report, "\n   Suspect!\n", sep="")
  } else{
    temp <- unlist(lapply(x, function(x) !is.null(x)))
    temp <- length(temp[temp])
    report <- paste(report, "\n   list of ", length(x), " profile models",
                    "\n   (", temp, " good)\n", sep="")
  }
  cat(report)
}





#' @rdname respeciate.generics
#' @method plot respeciate
#' @export

##########################
#notes
##########################
#like....
#better handling of factor axis labels
#better handling of axes and legend font sizes
#    (I think previous code may have handled this a little better)
#    (but not perfectly...)
#like horiz=total scales to be other way around?
#    could also mean rethinking the legend position for this?

############################
#added warning/handling for
#  duplicate species in profiles (handling merge/mean)
#  duplicated profile names (handling make unique)

#test is now set up to use data.table

#this is now sp_plot_profile

plot.respeciate <- function(x, ...){
  sp_plot_profile(x, ...)
}



#########################
#to do
#########################

#check below and then remove???

rsp_plot.respeciate.old <-
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



##############################
#plot.respeciate using lattice
##############################

#to do
#####################

#layout ???
#n > 6 warning not appearing !!!
#option to have col as a function ???

#decide what to do about stacking
#log / bad.log???

#say no to stack logs!

#would like it to handle logs force origin to 0 for standard
#    and minimum for logs ???

#strip label font size???

#key? to reorder the auto.key test and rectangles???
# key=list(space="right",adj=0,title="Legends",
#    points=list(pch=1,
#            col=trellis.par.get("superpose.symbol")$col[1:length(labels)]),
# text=list(labels))

#plot types???

#

#test
#my <- "C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\test\\uk.metals.aurn.2b.rds"
#my <- sp_build_rsp_x(readRDS(my))
#rsp_plot(my)


#########################
#next
##########################

#now very messy...
#what can we rationalise???
#profile name shortening
#profile name to code option???
#species name to species id option???

rsp_plot <-
  function(x, id, order=TRUE,
           log=FALSE, ...){

    #setup
    ##################
    #add .value if not there
    x <- rsp_tidy_profile(x)
    #others refs
    .x.args <- list(...)
    .sp.ord <- unique(x$SPECIES_ID)
    .sp.pro <- unique(x$PROFILE_NAME)
    #n/profile handling
    profile <- if (missing(id)) {
      profile <- .sp.pro
    } else {
      id
    }
    if (is.numeric(profile)) {
      if (all(profile == -1)) {
        profile <- .sp.pro
      }
      else {
        profile <- .sp.pro[profile]
      }
    }
    if (!any(profile %in% .sp.pro) | any(is.na(profile))) {
      stop("RSP> unknown profile(s) or missing ids, please check", call. = FALSE)
    }

    if(length(profile)>8 & missing(id)){
      warning("RSP> ", length(profile), " profiles... ",
              "plot foreshorten to 8 to reduce cluttering",
              "\n\t (maybe use id to force larger range if sure)",
              sep="", call.=FALSE)
      profile <- profile[1:8]
    }
    x <- x[x$PROFILE_NAME %in% profile,]

    ##test object type
    test <- rsp_test_respeciate(x, level=2, silent=TRUE)
    if(test != "respeciate"){
      if(test %in% c("respeciate.profile.ref", "respeciate.species.ref")){
        stop("RSP> No plot method for respeciate.reference files.",
             call. = FALSE)
      } else {
        stop("RSP> suspect respeciate object!",
             call. = FALSE)
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
      #maybe warning() aw well??
      return(invisible(NULL))
    }

    x <- rsp_test_profile(x)

    if(any(x$.n>1)){
      warning(paste("RSP> found duplicate species in profiles (merged and averaged...)",
                    sep=""), call.=FALSE)
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
      warning(paste("RSP> found profiles with common names (making unique...)",
                    sep=""), call. = FALSE)
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
      #previous barplot had bedside
      if("stack" %in% names(.x.args) && .x.args$stack){
        test <- test[order(test$.total, decreasing = TRUE),]
        xx <- unique(test$SPECIES_NAME)
      } else {
        test <- x[order(x$WEIGHT_PERCENT, decreasing = TRUE),]
        xx <- unique(test$SPECIES_NAME)
      }
    } else {
      xx <- unique(x$SPECIES_NAME)
    }
    x <- x[c("WEIGHT_PERCENT", "PROFILE_NAME", "SPECIES_NAME")]

    x$SPECIES_NAME <- factor(x$SPECIES_NAME,
                             levels = xx)

    ##################
    #profile bar chart
    ##################
    p1.ls <- list(x= WEIGHT_PERCENT~SPECIES_NAME,
                  data=x, ylab="Profile Loading", xlab="",
                  #NB: prepanel seemed to break ylim when stacking
                  panel = function(x, y, origin, ylim, ...){
                    rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                 panel.grid, ...)
                    if(missing(origin)){
                      origin <- if(min(y, na.rm=TRUE) < 0 ) {
                        min(y, na.rm=TRUE) - 0.02
                      } else {
                        0
                      }
                    }
                    panel.barchart(x=x, y=y, origin=origin, ylim=ylim, ...)
                  },
                  between=list(y=.2),
                  scales=list(x=list(rot=90,
                                     cex=0.7,
                                     alternating=1),
                              y=list(rot=c(0,90),
                                     cex=0.7,
                                     alternating=3,
                                     relation="free"))
                  )
                  #,
                  #auto.key=list(space="right", columns = 1,
                  #              cex=0.7,
                  #              points=FALSE,
                  #              rectangles=TRUE))
    #################
    #this may need refining...

    #####################
    #this is involved...

    if("col" %in% names(.x.args)){
      if(is.function(.x.args$col)){
        .x.args$col <- .x.args$col(length(profile))
      }
    }

    if(length(profile)>1){
      #panel or group profiles?
      if("panel.profiles" %in% names(.x.args)){
        p1.ls$x <- WEIGHT_PERCENT~SPECIES_NAME | PROFILE_NAME
      } else {
        p1.ls$groups <- x$PROFILE_NAME
        if(!"col" %in% names(p1.ls)){
          p1.ls$col <- rep(trellis.par.get("superpose.polygon")$col,
                           length.out=length(profile))
        }
      }
    }

    if(log){
      if("stack" %in% names(.x.args) && .x.args$stack){
        stop("RSP> sorry currently don't stack logs...",
        call. = FALSE)
      }
      #previous
      p1.ls$scales$y$log <- 10
      p1.ls$yscale.components <- rsp_yscale.component.log10
    }
    p1.ls <- modifyList(p1.ls, .x.args)
    if("groups" %in% names(p1.ls) & length(profile)>1){
      #add key... if auto.key not there
      .tmp <- if("col" %in% names(p1.ls)){
        rep(p1.ls$col, length.out = length(profile))
      } else {
        rep(trellis.par.get("superpose.polygon")$col,
            length.out=length(profile))
      }
      p1.ls$key <- list(space="right",
                        #title="Legends",
                        rectangles=list(col=.tmp),
                        text = list(profile, cex=0.7))
    }
    if("key" %in% names(.x.args)){
      p1.ls$key <- modifyList(p1.ls$key, .x.args$key)
    }
    if("col" %in% names(p1.ls)){
      p1.ls$par.settings = list(superpose.polygon = list(col = p1.ls$col),
                              superpose.symbol = list(fill = p1.ls$col))
    }
    p1 <- do.call(barchart, p1.ls)
    return(p1)
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


# see below about alternative summary output
#   including check sum?

#   maybe table
#   profile (code); (name); type; n.species (count); checksum; comments
#   just show some but send all

# replacing previous

summary.respeciate <-
  function(object, ...){
    #v0.1 summary
    #n <- object$PROFILE_TYPE
    #n <- n[!duplicated(object$PROFILE_CODE)]
    #summary(factor(n))

    #v0.3 summary

    xx <- data.table::as.data.table(object)


    #########################
    #disabling print/silent for now
    ##############################
    #messy when summary caught
    #  remarking .xargs, .max.n and .silent

    #.xargs <- list(...)

    #.max.n <- if("max.n" %in% names(.xargs)){
    #  .xargs$max.n
    #} else {
    #  10
    #}

    #.silent <- if("silent" %in% names(.xargs)){
    #  .xargs$silent
    #} else {
    #  FALSE
    #}

    #check what we have
    test <- c("WEIGHT_PERCENT","PROFILE_NAME","PROFILE_TYPE",
              "SPECIES_ID")
    test <- test[ test %in% colnames(xx)]
    if(!"PROFILE_CODE" %in% colnames(xx)){
      xx$PROFILE_CODE <- "{{ICK}}"
    }

    out <- xx[,
              .(#SPECIES_NAME = SPECIES_NAME[1],
                #SPEC_MW = SPEC_MW[1],
                .checksum = if("WEIGHT_PERCENT" %in% names(xx)){
                  sum(WEIGHT_PERCENT, na.rm = TRUE)
                } else {
                  NA
                },
                .checkname = if("PROFILE_NAME" %in% names(xx)){
                  length(unique(PROFILE_NAME))}
                else {
                  NA
                },
                .name = if("PROFILE_NAME" %in% names(xx)){
                  PROFILE_NAME[1]
                } else {
                  NA
                },
                .type = if("PROFILE_TYPE" %in% names(xx)){
                  PROFILE_TYPE[1]
                } else {
                  NA
                },
                .nspecies = if("SPECIES_ID" %in% names(xx)){
                  length(unique(SPECIES_ID))
                } else {
                  NA
                }
              ),
              by=.(PROFILE_CODE)]

    #out <- merge(xx, out, by="PROFILE_CODE", all.x=TRUE, all.y=FALSE,
    #             allow.cartesian=TRUE)

    out$PROFILE_CODE[out$PROFILE_CODE=="{{ICK}}"] <- NA
    out <- as.data.frame(out)
    #if(!.silent){
    #  if(nrow(out) > .max.n){
    #    print(head(out[c(1,2,5,6)], n = .max.n))
    #    cat("  [forestortened - showing ", .max.n, " of ", nrow(out), "]\n",
    #        sep="")
    #  } else {
    #    print(out[c(1,2,5,6)])
    #  }
    #  return(invisible(out))
    #}
    out
  }






#################################
#unexported code
#################################


# like to do something like this for summary
#     but code very messy
#     AND run time for 100+ profiles is too slow...

#     try with data.table...


#rsp_summary_v3 <- function(object, ...){

#  xx <- as.data.table(object)

#  .xargs <- list(...)
#  .max.n <- if("max.n" %in% names(.xargs)){
#    .xargs$max.n
#  } else {
#    10
#  }
#  .silent <- if("silent" %in% names(.xargs)){
#    .xargs$silent
#  } else {
#    FALSE
#  }


  #check what we have
#  test <- c("WEIGHT_PERCENT","PROFILE_NAME","PROFILE_TYPE",
#            "SPECIES_ID")
#  test <- test[ test %in% colnames(xx)]
#  if(!"PROFILE_CODE" %in% colnames(xx)){
#    xx$PROFILE_CODE <- "{{ICK}}"
#  }

#  out <- xx[,
#            .(#SPECIES_NAME = SPECIES_NAME[1],
#              #SPEC_MW = SPEC_MW[1],
#              .checksum = if("WEIGHT_PERCENT" %in% names(xx)){
#                sum(WEIGHT_PERCENT, na.rm = TRUE)
#              } else {
#                NA
#              },
#              .checkname = if("PROFILE_NAME" %in% names(xx)){
#                length(unique(PROFILE_NAME))}
#              else {
#                NA
#              },
#              .name = if("PROFILE_NAME" %in% names(xx)){
#                PROFILE_NAME[1]
#              } else {
#                NA
#              },
#              .type = if("PROFILE_TYPE" %in% names(xx)){
#                PROFILE_TYPE[1]
#              } else {
#                NA
#              },
#              .nspecies = if("SPECIES_ID" %in% names(xx)){
#                length(unique(SPECIES_ID))
#              } else {
#                NA
#              }
#            ),
#            by=.(PROFILE_CODE)]

  #out <- merge(xx, out, by="PROFILE_CODE", all.x=TRUE, all.y=FALSE,
  #             allow.cartesian=TRUE)

#  out$PROFILE_CODE[out$PROFILE_CODE=="{{ICK}}"] <- NA
#  out <- as.data.frame(out)
#  if(!.silent){
#    if(nrow(out) > .max.n){
#      print(head(out[c(1,2,5,6)], n = .max.n))
#      cat("  [forestortened - showing ", .max.n, " of ", nrow(out), "]",
#          sep="")
#    } else {
#      print(out[c(1,2,5,6)])
#    }
#  }
#  invisible(out)

#}

#rsp_summary_v2 <-
#  function(object, ...){
#    #v0.2 summary
#    if(!"PROFILE_CODE" %in% names(object)){
#      object$PROFILE_CODE <- "{{NA}}"
#    }
#    ref <- unique(object$PROFILE_CODE)
#    if(length(ref)>10){
#      ref <- ref[1:100]
#    }
#    .out <- lapply(ref, function(x){
#      .tmp <- subset(object, PROFILE_CODE==x)
#      .x <- if(x=="{{NA}}") {NA} else {x}
#      .pt <- if("PROFILE_TYPE" %in% names(.tmp)){
#        .tmp$PROFILE_TYPE[1]
#      } else {
#        NA
#      }
#      .pn <- if("PROFILE_NAME" %in% names(.tmp)){
#        .tmp$PROFILE_NAME[1]
#      } else {
#        NA
#      }
#      .ns <- if("SPECIES_ID" %in% names(.tmp)){
#        length(unique(.tmp$SPECIES_ID))
#      } else {
#        NA
#      }
#      .cs <- if("WEIGHT_PERCENT" %in% names(.tmp)){
#        sum(.tmp$WEIGHT_PERCENT, na.rm=TRUE)
#      } else {
#        NA
#      }
#      data.frame(profile=.x,
#                 type=.pt,
#                 name=.pn,
#                 n.species=.ns,
#                 checksum=.cs)
#    })
#    .out <- do.call(rbind, .out)
#    print(.out[c("profile", "type", "n.species", "checksum")], max=40)
#    return(invisible(.out))
#  }




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

# respeciate profile(s)
#    [profile_code] [check sum] [profile_name] < width limited
#    ... showing n

# added profile_name to output

# could move check sum to summary and
#      replace with species count???

# doing this... previous
###.msg <- paste("  ", i, " (checksum: ",
##round(sum(as.numeric(as.character(x[x$PROFILE_CODE==i,]$WEIGHT_PERCENT)),
##          na.rm = T), digits=2),
##") ", i2, "\n", sep="")

# could make other respeciate print outputs
#      look like this?

rsp_print_respeciate <-
  function(x, n=6, ...){
    #profile_code is (I think) only term unique to a profile
    y <- unique(x$PROFILE_CODE)
    report <- paste("respeciate profile(s): count ",
                    length(y), "\n", sep="")
    if(length(y)==0){
      cat(paste(report,
                "empty (or bad?) respeciate object\n",
                sep=""))
      return(invisible(x))
    }
    .tmp <- getOption("width")
    yy <- if(length(y)>n) {y[1:n]} else {y}
    for(i in yy){
      if("PROFILE_NAME" %in% names(x)){
        i2 <- x$PROFILE_NAME[x$PROFILE_CODE==i][1]
      } else {
        i2 <- "[unknown]"
      }
      if("SPECIES_ID" %in% names(x)){
        .spe <- length(unique(x$SPECIES_ID[x$PROFILE_CODE==i]))
      } else {
        .spe <- "0!"
      }
      .msg <- paste("  ", i, " (", .spe, " species) ",
                    i2, "\n", sep="")
      if(nchar(.msg)>.tmp){
        .msg <- paste(substring(.msg, 1, .tmp-3), "...\n")
      }
      report <- paste(report, .msg, sep="")
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





