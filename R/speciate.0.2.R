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
get_profile <- function(code) {

  #handle numerics
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  df <- PROFILES[PROFILES$PROFILE_CODE == code, ]
  df <- merge(df, SPECIES, by = "PROFILE_CODE")
  df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID")
  df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE")
  df <- merge(df, REFERENCES, by = "REF_Code")
  df <- sp_build_respeciate(df)
  return(df)
}

#' @description When supplied a \code{respeciate}
#' object, \code{\link{print}} manages its appearance.
#' @rdname respeciate
#' @method print respeciate
#' @export
print.respeciate <-
  function(x, ...){
    xx <- paste(unique(x$REF_Code), collapse = "; ")
    cat("respeciate profile: ", xx, "\n checksum: ",
        sum(as.numeric(as.character(x$WEIGHT_PERCENT)),
            na.rm = T),
        "\n")
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
  function(x, ...){
    xx <- sp_tidy_species_name(x$SPECIES_NAME)
    ref <- max(nchar(xx), na.rm=TRUE) * 0.25
    if(ref>10) ref <- 10 #stop it getting silly
    op <- par(mar=c(ref,4,4,2))

    b <- barplot(x$WEIGHT_PERCENT,
                 xaxt="n", space=0.5, ...)
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

sp_tidy_species_name <- function(x){
  #not fully tested, might be cases this dies on

  #shorten names by remove other versions
  #names seem to be in format a (b or c)
  gsub("[(].*","", x)
}


