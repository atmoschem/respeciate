#' Access to the SPECIATE 5.1 US/EPA Tool
#'
#' @description \code{\link{spec}} Return a speciate data.frame
#'
#' @param code Character, PROFILE CODE required by EPA/Speciate
#' @return a data.frame with full information for the desired code (PROFILE_CODE)
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
spec <- function(code) {

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  df <- PROFILES[PROFILES$PROFILE_CODE == code, ]
  df <- merge(df,
              SPECIES,
              by = "PROFILE_CODE")
  cat("Sum WEIGHT_PERCENT: ",
      sum(as.numeric(as.character(df$WEIGHT_PERCENT)), na.rm = T),
      "\n")

  df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID")

  df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE")

  df <- merge(df, REFERENCES, by = "REF_Code")

return(df)

}


#' Find PROFILE_CODE
#'
#' @description \code{\link{find_code}} Return a data.frame with profile codes
#'
#' @param profile Character, to search PROFILE CODE
#' @param by Character, to search code. eg: "Keywords", "PROFILE_NOTES", "PROFILE_TYPE"
#' or other name of PROFILES
#' @return a data.frame with with profile codes
#' @export
#' @references
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#' @examples \dontrun{
#' profile <- "Ethanol"
#' dt <- find_code(profile)
#' }
find_code <- function(profile, by = "Keywords") {

  PROFILES <- sysdata$PROFILES

  return(PROFILES[grep(profile, PROFILES[[by]]), ])
}
