#' @name sp.average
#' @title speciate data averaging functions
#' @aliases sp_average_profile


#' @description Functions to build composite (re)SPECIATE profiles


#' @description \code{sp_average_profile} generates an average composite
#' of a supplied multi-profile \code{respeciate} object.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param code required character, the unique profile code to assign to the
#' average profile.
#' @param name character, the profile name to assign to the average
#' profile. If not supplied, this defaults to a collapsed list of the codes
#' of all the profiles averaged.
#' @param method numeric, the averaging method to apply: Currently only 1 (default)
#' \code{mean(x)}.
#' @param ... additional arguments, currently ignored
#' @return \code{sp_average_profile} returns a single profile average
#' version of the supplied \code{respeciate} profile.
#' @note In development function; arguments and outputs likely to be subject to
#' change.
#'
#' This is one of the very few \code{respeciate} functions that modifies the
#' \code{WEIGHT_PERCENT} column of the \code{respectiate} \code{data.frame}.


#NOTE

#' @rdname sp.average
#' @export

## #' @import data.table (in xxx.r)
# may need to set data.table specifically??
#      data.table::as.data.table, etc??

######################
#average data
######################

## in development

## to do

##     padding output - see in function notes

##     plot and output options

## to think about

##     currently averaging using mean
##          do we need to rescale before averaging because not all
##          WEIGHT_PERCENT add up to 100 ??

##     mean.respeciate
#           could be a no plot, method = mean version of this function ??


##########################
#issue
############################

###########################
#test
###########################

#aa <- sp_profile(sp_find_profile("ae8", by="profile_type"))
#sp_average_profile(aa)


sp_average_profile <- function(x, code = NULL, name = NULL, method = 1,
                               ...){

  #################################
  #check x is a respeciate object??

  #check it has .value
  x <- rsp_tidy_profile(x)

  #save class to return as is..
  #    thinking about this
  tmp <- class(x)
  xx <- data.table::as.data.table(x)

  #save profiles
  test <- unique(x$PROFILE_CODE)

  #extra.args - not sure if we are using these
  .xargs <- list(...)

  #get profile terms if supplied
  if(is.null(code)){
    stop("need a new profile code")
  }
  xx$PROFILE_CODE <- code

  xx$PROFILE_NAME <- if(is.null(name)){
    if(length(test)>10){
      "average of multiple cases"
    } else {
      paste("average of:", paste(test, collapse = ","), sep="")
    }
  } else {
    name
  }

  out <- xx[,
            .(PROFILE_NAME = PROFILE_NAME[1],
              SPECIES_NAME = SPECIES_NAME[1],
  ##########################
  # testing
              #SPEC_MW = SPEC_MW[1],
  ##########################
              .total = sum(.value, na.rm = TRUE),
              .value = mean(.value, na.rm = TRUE),
              .n = length(.value[!is.na(.value)]),
              .sd = sd(.value, na.rm = TRUE)
            ),
            by=.(PROFILE_CODE, SPECIES_ID)]
  #I said we would NOT do this...
  out$WEIGHT_PERCENT <- out$.value

  #########################
  #pad missing info
  #    check method in sp_profile
  #    species info
  #    ignore profile and ref info
  #         because the user is builder...
  #########################

  #output

  ################
  #plot, report, etc
  ###################

  out <- as.data.frame(out)
  class(out) <- tmp
  out

}




#####################################
#sp_species_calc
#####################################

sp_species_calc <- function(x, calc = NULL,
                            id = NULL, name = NULL,
                               ...){
  #x is an rsp object
  #calc is the calculation to apply to species in x

  .temp <- x
  #test we can use this..?
  print(calc)
  .temp <- sp_dcast_species(.temp)
  if(length(grep("=", calc)) > 0){
    print("is equals")
  } else {
    print("no equals")
  }

  #out
  return(NULL)
}
