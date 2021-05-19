#' @name respeciate
#' @title respeciate
#' @aliases find_profile_code profile

#' @description respeciate but with object classes...

#reversed order of documentation

#' @description \code{\link{find_profile_code}} finds
#' the codes of speciate profiles in the local speciate
#' archive using supplied search terms.
#' @param term character, the search term to use
#' when searching archive.
#' @param by character, the section of the archive to
#' search, by default \code{'keywords'}.
#' @return \code{find_profile_code} returns a object of
#' \code{respeciate.ref} class, a \code{data.frame} of
#' profile information.
#' @rdname respeciate
#' @export
#' @examples \dontrun{
#' profile <- "Ethanol"
#' dt <- find_profile_code(profile)
#' }
find_profile_code <- function(term, by = "Keywords") {
  #extract profile info from archive
  PROFILES <- sysdata$PROFILES
  out <- PROFILES[grep(term, PROFILES[[by]]), ]
  out <- sp_build_respeciate.ref(out)
  return(out)
}

#' @description \code{\link{get_profile}} extracts a
#' speciate profile from the local speciate archive.
#' @param code character or numeric, the speciate code
#' of the required source profile (EPA SPECIATE PROFILE_CODE).
#' @return \code{get_profile} returns a object of
#' \code{respeciate} class, a \code{data.frame} containing a
#' speciate profile.
#' @rdname respeciate
#' @export
#' @references
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#' @examples \dontrun{
#' code <- "8855"
#' x <- spec(code)
#' }

#NOTE

#get_profile allows you to get multiple profiles
#not sure this is staying

get_profile <- function(code) {

  #handle numerics
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  df <- lapply(code, function(x){
    df <- PROFILES[PROFILES$PROFILE_CODE == x, ]
    df <- merge(df, SPECIES, by = "PROFILE_CODE")
    df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID")
    df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE")
    df <- merge(df, REFERENCES, by = "REF_Code")
    df
  })
  #build
  df <- do.call(rbind,df)
  df <- sp_build_respeciate(df)
  return(df)
}

#' @description When supplied a \code{respeciate}
#' object, \code{\link{print}} manages its appearance.
#' @rdname respeciate
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
#' output. The objects is a data.frame so you can still
#' use it with as normal with \code{lattice} or
#' \code{ggplot2}...
#' @param x the \code{respeciate} or \code{respeciate}
#' object to be printed, plotted, etc.
#' @param ... any extra arguments, currently ignored.
#' @rdname respeciate
#' @method plot respeciate
#' @export
plot.respeciate <-
  function(x, profile=NULL, ...){
    #test single or multiple profile
    test <- unique(x$PROFILE_CODE)
    if(length(test)>1){
      if(is.null(profile)) {
        profile <- 1
        cat("(multi-profile object; showing first)\n")
      } else {
        if(length(profile)>1){
          warning("Multiple profiles requested; show first")
          profile <- profile[1]
        }
      }
      if(any(profile>length(test))){
        profile <- 1
        warning(paste("Requested profile not present; showing first\n",
          "  (NB: only ", length(test), " available)\n",
                      sep=""))
      }
      x <- x[x$PROFILE_CODE==test[profile],]
      test <- test[profile]
    }

    if(length(test)==0){
      stop("empty (or bad) respeciate object?")
    }

    #single profile
    #x <- x[rev(order(x$WEIGHT_PERCENT)),]
    b <- x$WEIGHT_PERCENT
    xx <- sp_tidy_species_name(x$SPECIES_NAME)

    #set up x annotation
    ref <- max(nchar(xx), na.rm=TRUE) * 0.25
    if(ref>10) ref <- 10 #stop it getting silly with x names
    op <- par(mar=c(ref,4,4,2))

    #notes:
    #currently doesn't like horiz =TRUE
      #need to rethink par and axis to make this work...

    b <- barplot(b, xaxt="n", space=0.5,
                 ...)
    axis(1, at=b, labels=xx, las=2, tick=FALSE, cex.axis=0.5)
    rm(op)
  }






#' @description When supplied a \code{respeciate.ref}
#' object, \code{\link{print}} manages its appearance.
#' @param x the \code{respeciate.ref} object to be printed.
#' @param ... any extra arguments, currently ignored.
#' @rdname respeciate
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

sp_build_respeciate.ref <-
  function(x, ...){
    #build
    class(x) <- c("respeciate.ref", "data.frame")
    x
  }

sp_build_respeciate <-
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

sp_tidy_species_name <- function(x){

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

sp_split_profile <- function(x){
  ref <- unique(x$PROFILE_CODE)
  lapply(ref, function(y) x[x$PROFILE_CODE==y,])
}



#########################
#plot 2
#########################

#alternative to above plot.respeciate
#working on this handle multiple profiles...

#not exporting

plot.v2.respeciate <-
  function(x, profile=1:6, ...){
    #test single or multiple profile
    test <- unique(x$PROFILE_CODE)
    if(length(test)>6){
      warning(paste(length(test),
            " profiles; might be too many profiles for plot",
            "\n  (suggest plotting 6 or less)",
            "\n", sep=""))
    }

    test.names <- make.unique(sapply(test,
                                     function(y) subset(x,
                                                        PROFILE_CODE==y)$PROFILE_NAME[1]))
    if(length(test)==0){
      stop("empty (or bad) respeciate object?")
    }

    #assuming multiple profile
    #build common data (could use dplyr)
    x <- x[c("PROFILE_NAME", "PROFILE_CODE",
               "SPECIES_NAME", "SPECIES_ID", "SPEC_MW",
               "WEIGHT_PERCENT")]
    x <- sp_split_profile(x)
    x <- suppressWarnings(Reduce(function(x, y)
            merge(x=x, y=y,
                  by=c("SPECIES_ID", "SPECIES_NAME",
                      "SPEC_MW"),
                  all.x=T, all.y=T), x)
            )
    names(x) <- make.names(names(x), unique=TRUE)

    #order largest to smallest
    temp <- names(x)[grep("WEIGHT_PERCENT", names(x))]
    temp <- apply(x[temp], 1,
                  function(y) sum(y, na.rm=TRUE))
    x <-x[rev(order(temp)),]

    #prepare plot
    xx <- sp_tidy_species_name(x$SPECIES_NAME)
    x <- x[grep("WEIGHT_PERCENT", names(x))]
    x[is.na(x)] <- 0
    b <- t(as.matrix(x))

    #set up x annotation
    ref <- max(nchar(xx), na.rm=TRUE) * 0.25
    if(ref>10) ref <- 10 #stop it getting silly with x names
    op <- par(mar=c(ref,4,4,2))

    #notes:
    #currently doesn't like horiz =TRUE
    #need to rethink par and axis to make this work...
    #currently doesn't like beside=TRUE

    b <- barplot(b, xaxt="n", space=0.5,
                 legend.text=test.names,
                 args.legend = list(cex=0.5, x="topright"),
                 ...)
    axis(1, at=b, labels=xx, las=2, tick=FALSE, cex.axis=0.5)
    rm(op)
  }
