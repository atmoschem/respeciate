#' @name sp.rescale
#' @title sp_profile data rescaling functions
#' @aliases sp_rescale_profile

#' @description Functions for rescaling (re)SPECIATE profiles

#' @description \code{\link{sp_rescale_profile}} rescales the percentage
#' weight information in a supplied (re)SPECIATE profile.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param method numeric, the rescaling method to apply:
#'   1 (default) \code{x/mean(x)};
#'   2 \code{x-min(x)/max(x)-min(x)};
#'   3 \code{x-mean(x)/sd(x)};
#'   4 \code{x/max(x)}.
#' The alternative 0 returns the records to their original
#' values.
#' @return \code{sp_rescale_profile} returns the \code{respeciate} profile
#' with the percentage weight records rescaled using the requested method.
#' @note Data sometimes needs to be normalised, e.g. when applying some
#' statistical analyses. Rather than modify the EPA records in the
#' \code{WEIGHT_PERCENT} column, \code{respeciate} creates a duplicate column
#' \code{.value} which is modified by operations like \code{sp_profile_rescale}.
#' This means rescaling is always applied to the source information, rather
#' than rescaling an already rescaled value, the EPA records are retained
#' unaffected and the original source information can be easily recovered.
#' @references
#'   Dowle M, Srinivasan A (2023). data.table: Extension of `data.frame`.
#'   R package version 1.14.8, \url{https://CRAN.R-project.org/package=data.table}.

#NOTE

#' @rdname sp.rescale
#' @export
#' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

# may need to think about additional local scaling
#      e.g. within in profile [species conc]/[sum of all species concs]

######################
#rescale data
######################

##########################
#issues
############################



sp_rescale_profile <- function(x, method = 1){

  #################################
  #check x is a respeciate object??

  #check it has .value
  x <- rsp_tidy_profile(x)

  #save to return as is..
  #    thinking about this
  tmp <- class(x)

  #################################
  #calculate stats
  xx <- as.data.table(x)
  #remove stats if there...
  test <- c(".min",".max",".total", ".mean", ".na", ".n", ".sd")
  test <- test[ test %in% colnames(xx)]
  if(length(test)>0){
    xx <- xx[, (test) := NULL]
  }
  #and recalculate
  out <- xx[,
            .(#SPECIES_NAME = SPECIES_NAME[1],
              #SPEC_MW = SPEC_MW[1],
              .min = min(WEIGHT_PERCENT, na.rm = TRUE),
              .max = max(WEIGHT_PERCENT, na.rm = TRUE),
              .total = sum(WEIGHT_PERCENT, na.rm = TRUE),
              .mean = mean(WEIGHT_PERCENT, na.rm = TRUE),
              .na = length(WEIGHT_PERCENT[is.na(WEIGHT_PERCENT)]),
              .n = length(WEIGHT_PERCENT[!is.na(WEIGHT_PERCENT)]),
              .sd = sd(WEIGHT_PERCENT, na.rm = TRUE)
            ),
            by=.(SPECIES_ID)]

  out <- merge(xx, out, by="SPECIES_ID", all.x=TRUE, all.y=FALSE,
               allow.cartesian=TRUE)

  #################
  #need to decide how to handle this
  ###################
  #this is a little messy

  #rather leave weight-percent untouched
  #   make a local .value column if not there
  #       then have rescale reset that
  #       using weight-percent as source

  #not sure of a best plan for method arg
  #     how to handling...?
  #     how much user control...?

  #other options
  #     no rescale
  #     x/sum(x) so all out of 1?
  #         that might want to only for unique profile codes???

  if(!method %in% c(0:4)){
    stop("unknown method")
  }
  if(method==0){
    out$.value<- out$WEIGHT_PERCENT
  }
  if(method==1){
    out$.value<- out$WEIGHT_PERCENT /out$.mean
  }
  if(method==2){
    #might be an issue if only one value
    out$.value <- (out$WEIGHT_PERCENT - out$.min) / (out$.max - out$.min)
  }
  if(method==3){
    #might be an issue if only one value
    out$.value <- (out$WEIGHT_PERCENT - out$.mean) / out$.sd
  }
  if(method==4){
    out$.value<- out$WEIGHT_PERCENT / out$.max
  }

  #output
  #
  out <- as.data.frame(out)
  class(out) <- tmp
  out
}


