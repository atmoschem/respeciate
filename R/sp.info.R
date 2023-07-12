#' @name sp.info
#' @title re(SPECIATE) information
#' @aliases sp_info

###########################
#keep think about the names
###########################
#  wondering if find_profile_info, sp_find_profile_info or
#    find_sp_profile_info would be better?

#like to include summary(factor(sysdata$PROFILES$PROFILE_TYPE))
#  or something like that

#' @description \code{sp_info} provides (re)SPECIATE
#' source information...
#' @return \code{sp_info} provide brief source information report on the
#' currently installed (re)SPECIATE data sets.

# tidy output???
#    little messy???

# this is not currently catchable!!!!
# a <- sp_info() #a = NULL

#' @rdname sp.info
#' @export

sp_info <- function() {
  #extract profile info from archive
  .ver <- "source: SPECIATE 5.2\n\t[in (re)SPECIATE since 0.2.0]"
  .pro <- length(unique(sysdata$PROFILES$PROFILE_CODE))
  .spc <- length(unique(sysdata$SPECIES_PROPERTIES$SPECIES_ID))
  cat(.ver, "\n\tProfiles: ", .pro, "\n\tSpecies: ", .spc, sep = "")

}

