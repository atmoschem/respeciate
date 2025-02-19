#' @name rsp.info
#' @title Information about data sets currently in respeciate
#' @aliases rsp_info

#######################
# recent changes:
#######################

# in 0.3.1

# separated rsp_info and rsp_find.../rsp_..._info functions
#      also dropped forms of functions...


#########################
#to think about
#########################

# testing idea to remove need to set source...
#    following seems messy
#        rsp(rsp_find_profile("diesel", source="eu"), source="eu")
#    using profile_code = US:... and EU:... for source used
#        now need to set source when you want to be specific...
#    notes:
#        seems to be working BUT...
#    rsp_find_species might have issue
#        because species info can be coming from either
#        SPECIATE or SPECIEUROPE...
#    data.table::rbindlist seems to be forcing WEIGHT_PERCENT to character
#        when bind is list(us, NULL)...

#' @description Functions that provide respeciate
#' source information.
#' \code{rsp_info} generates a brief version report for the currently installed
#' respeciate data sets.

#' @return \code{rsp_info} provides a brief version information report on the
#' currently installed respeciate archive.

#' @seealso \code{\link{SPECIATE}} and \code{\link{SPECIEUROPE}}
#' @references
#' For SPECIATE:
#'
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#'
#' For SPECIEUROPE:
#'
#' Pernigotti, D., Belis, C.A., Spano, L., 2016. SPECIEUROPE: The
#' European data base for PM source profiles. Atmospheric Pollution Research,
#' 7(2), pp.307-314. DOI: https://doi.org/10.1016/j.apr.2015.10.007
#'


#' @examples \dontrun{
#' rsp_info()
#' }
#'


#######################
#rsp_info
#######################

# tidy output???
#    little messy???

#like to include summary(factor(SPECIATE$PROFILES$PROFILE_TYPE))
#       or summary.respeciate(sysdata$PROFILES)
#       BUT for SPECIATE and SPECIEUROPE
#  ??? database.summary

# this is not currently catchable!!!!
# a <- rsp_info() #a = NULL

#' @rdname rsp.info
#' @export

rsp_info <- function() {

  #extract profile info dircetly from archive
  #    could use ..rsp outputs in future but
  #         either is probably fine...
  SPECIATE <- respeciate::SPECIATE
  SPECIEUROPE <- respeciate::SPECIEUROPE
  .epa <- paste("source: SPECIATE 5.2\n\t[in respeciate since 0.2.0]",
                "\n\tProfiles: ", length(unique(SPECIATE$PROFILES$PROFILE_CODE)),
                "; species: ", length(unique(SPECIATE$SPECIES_PROPERTIES$SPECIES_ID)),
                sep="")
  .eu <- paste("source: SPECIEUROPE 2.0\n\t[in respeciate since 0.3.1]",
               "\n\tProfiles: ", length(unique(SPECIEUROPE$source$Id)),
               "; species: ", length(unique(SPECIEUROPE$source$Specie.Id)),
               sep="")
  .sts <- paste("respeciate: ", packageVersion("respeciate"), "", sep="")
  cat(.sts, "\n", .epa, "\n", .eu, "\n", sep = "")
}


