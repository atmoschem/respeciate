#' @name respeciate.0.2
#' @title respeciate.0.2
#' @aliases sp_find_profile sp_profile
#' @importFrom graphics axis barplot par

#' @description This is respeciate but with object classes...

#reversed order of documentation,
#started with the find function...

#' @description \code{\link{sp_find_profile}} finds
#' the codes of speciate profiles in the local speciate
#' archive using supplied search terms.
#' @param term character, the search term to use
#' when searching archive.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'}.
#' @return \code{sp_find_profile} returns a object of
#' \code{respeciate.ref} class, a \code{data.frame} of
#' profile information.
#' @rdname respeciate.0.2
#' @export
#' @examples \dontrun{
#' profile <- "Ethanol"
#' dt <- sp_find_profile(profile)
#' }
sp_find_profile <- function(term, by = "Keywords") {
  #extract profile info from archive
  PROFILES <- sysdata$PROFILES
  out <- PROFILES[grep(term, PROFILES[[by]]), ]
  out <- rsp_build_respeciate.ref(out)
  return(out)
}

#' @description \code{\link{sp_profile}} extracts a
#' speciate profile from the local speciate archive.
#' @param code character or numeric, the speciate code
#' of the required source profile (EPA SPECIATE term PROFILE_CODE).
#' @return \code{sp_profile} returns a object of
#' \code{respeciate} class, a \code{data.frame} containing a
#' speciate profile.
#' @rdname respeciate.0.2
#' @export
#' @references
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#' @examples \dontrun{
#' x <- sp_profile(c(8833, 8850))
#' plot(x)
#' }

#NOTE

#get_profile allows you to get multiple profiles
#not sure this is staying

sp_profile <- function(code) {

  #handle numerics/characters
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  #handle multiple codes
  df <- lapply(code, function(x){
    df <- PROFILES[PROFILES$PROFILE_CODE == x, ]
    df <- merge(df, SPECIES, by = "PROFILE_CODE")
    df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID")
    df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE")
    df <- merge(df, REFERENCES, by = "REF_Code")
    df
  })
  #build
  df <- do.call(rbind, df)
  df <- rsp_build_respeciate(df)
  return(df)
}

#' @description When supplied a \code{respeciate}
#' object, \code{\link{print}} manages its appearance.
#' @rdname respeciate.0.2
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
#' \code{ggplot2}, useful it you are pulling multiple
#' profiles and you exceed the base \code{\link{barplot}}
#' capacity...
#' @param x the \code{respeciate}
#' object to be printed, plotted, etc.
#' @param n when plotting or printing a multi-profile, the
#' number(s) of profile(s) to report.
#' @param order logical, order the species in the
#' profile(s) by molecular weight before plotting.
#' @param legend.text,args.legend \code{\link{barplot}}
#' arguments.
#' @param ... any extra arguments, mostly ignored except by
#' \code{plot} which passes them to \code{\link{barplot}}.
#' @rdname respeciate.0.2
#' @method plot respeciate
#' @export
plot.respeciate <-
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






#' @description When supplied a \code{respeciate.ref}
#' object, \code{\link{print}} manages its appearance.
#' @param x the \code{respeciate} or \code{respeciate.ref}
#' object to be printed, plotted, etc.
#' @rdname respeciate.0.2
#' @method print respeciate.ref
#' @export
print.respeciate.ref <-
  function(x, ...){
    xx <- nrow(x)
    wi <- getOption("width")
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





#################################
#unexported code
#################################

###################################
#class builds
###################################

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
#plot 2
#########################

#alternative to above plot.respeciate
#working on this handle multiple profiles...

#now replacing previous plot.respeciate

