#' @name sp
#' @title sp_ functions
#' @aliases sp_profile


#' @description sp function to get profiles from the R (re)SPECIATE archive

#' @description \code{\link{sp_profile}} extracts a
#' SPECIATE profile from the local (re)SPECIATE archive.
#' @param code character or numeric, the SPECIATE code
#' of the required profile (EPA SPECIATE term PROFILE_CODE).
#' @return \code{sp_profile} returns a object of
#' \code{respeciate} class, a \code{data.frame} containing a
#' speciate profile.
#' @references
#' Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
#' Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
#' The development and uses of EPA SPECIATE database.
#' Atmospheric Pollution Research, 1(4), pp.196-206.
#' @examples \dontrun{
#' x <- sp_profile(c(8833, 8850))
#' plot(x)}

#NOTES
#######################

#to think about
#######################

#add two functions

## sp_build_profile to make a profile locally
## sp_import_profile to import a profile from an external source


#' @rdname sp
#' @export
##     (now importing via xxx.r)
##     #' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

#####################
#to think about
#####################
# not sure but I think something in the main build:
#    PROFILES>>SPECIES>>SPECIES_PROPERTIES>>PROFILE_REFERENCE>>REFERENCES
# is replicating profiles.
#

#v 0.2
#   based on previous sp_profile but using data.table
#   (0.1 version currently unexported sp_profile.old)

sp_profile <- function(code) {

  # code currently handles:
  # respeciate.ref, numerics and characters characters

  #######################
  #could replace code with ...???
  ######################

  if(class(code)[1] == "respeciate" && "PROFILE_CODE" %in% names(code)){
    code <- unique(code$PROFILE_CODE)
  }
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- as.data.table(sysdata$PROFILES)
  SPECIES <- as.data.table(sysdata$SPECIES)
  SPECIES_PROPERTIES <- as.data.table(sysdata$SPECIES_PROPERTIES)
  PROFILE_REFERENCE <- as.data.table(sysdata$PROFILE_REFERENCE)
  REFERENCES <- as.data.table(sysdata$REFERENCES)

  ##########################
  #testing tolower below
  #   as a fix for code arg case sensitivity
  ##########################
  df <- PROFILES[tolower(PROFILES$PROFILE_CODE) %in% tolower(code),]
  df <- merge(df, SPECIES, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE,
              allow.cartesian=TRUE)
  df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID", all.y=FALSE,
              all.x=TRUE, allow.cartesian=TRUE)
  df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE", all.y=FALSE,
              all.x=TRUE, allow.cartesian=TRUE)
  df <- merge(df, REFERENCES, by = "REF_Code", all.y=FALSE, all.x=TRUE,
              allow.cartesian=TRUE)
  df <- df[order(df$PROFILE_CODE, decreasing = FALSE),]

  #build
  #note: currently adding .value in rsp_build_respeciate
  #      could do it here?
  df <- rsp_build_respeciate(as.data.frame(df))
  return(df)
}












#############################
#unexported & previous code
#############################

#sp_profile v 0.1
#now unexported

rsp_profile.old <- function(code) {
  #handle numerics/characters
  #######################
  #could replace code with ...???
  ######################
  if(class(code)[1] == "respeciate" && "PROFILE_CODE" %in% names(code)){
    code <- unique(code$PROFILE_CODE)
  }
  if(is.numeric(code)) code <- as.character(code)
  if(!is.character(code)) stop("unexpected code class")

  PROFILES <- sysdata$PROFILES
  SPECIES <- sysdata$SPECIES
  SPECIES_PROPERTIES <- sysdata$SPECIES_PROPERTIES
  PROFILE_REFERENCE <- sysdata$PROFILE_REFERENCE
  REFERENCES <- sysdata$REFERENCES

  #handle multiple codes
  ############################
  #replace previous lapply with a direct %in%
  ##  df <- lapply(code, function(x){
  ##    df <- PROFILES[PROFILES$PROFILE_CODE == x, ]
  ##    ...
  ##  })
  ##  df <- do.call(rbind, df)
  #testing as sp_profile.2
  #faster with data.table
  ############################
    df <- PROFILES[PROFILES$PROFILE_CODE %in% code,]
    df <- merge(df, SPECIES, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE)
    df <- merge(df, SPECIES_PROPERTIES, by = "SPECIES_ID", all.y=FALSE, all.x=TRUE)
    df <- merge(df, PROFILE_REFERENCE, by = "PROFILE_CODE", all.y=FALSE, all.x=TRUE)
    df <- merge(df, REFERENCES, by = "REF_Code", all.y=FALSE, all.x=TRUE)
    df <- df[order(df$PROFILE_CODE, decreasing = FALSE),]
##  })
  #build
  df <- rsp_build_respeciate(df)
  return(df)
}


