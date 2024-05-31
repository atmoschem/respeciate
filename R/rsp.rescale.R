#' @name rsp.rescale
#' @title rescaling respeciate profiles
#' @aliases rsp_rescale rsp_rescale_profile rsp_rescale_species

#' @description Functions for rescaling respeciate data sets

#' @description \code{rsp_rescale} rescales the percentage weight records in
#' a supplied respeciate profile data set. This can be by profile or species
#' subsets, and \code{rsp_rescale_profile} and \code{rsp_rescale_species} provide
#' short-cuts to these options.
#' @param rsp A \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles.
#' @param method numeric, the rescaling method to apply:
#'   1 \code{x/total(x)};
#'   2 \code{x/mean(x)};
#'   3 \code{x-min(x)/max(x)-min(x)};
#'   4 \code{x-mean(x)/sd(x)};
#'   5 \code{x/max(x)}.
#' The alternative 0 returns the records to their original
#' values.
#' @param by character, when rescaling \code{x} with
#' \code{\link{rsp_rescale}}, the data type to group and rescale,
#' currently \code{'species'} (default) or \code{'profile'}.
#' @return \code{rsp_rescale} and \code{rsp_rescale} return the
#' \code{respeciate} profile with the percentage weight records rescaled using
#' the requested method. See Note.
#' @note Data sometimes needs to be normalised, e.g. when applying some
#' statistical analyses. Rather than modify the EPA records in the
#' \code{WEIGHT_PERCENT} column, \code{respeciate} creates a duplicate column
#' \code{.value} which is modified by operations like \code{sp_rescale_profile}
#' and \code{sp_rescale_species}. This means rescaling is always applied to
#' the source information, rather than rescaling an already rescaled value,
#' and the EPA records are retained unaffected. So, the original source
#' information can be easily recovered.
#' @references
#'   Dowle M, Srinivasan A (2023). data.table: Extension of `data.frame`.
#'   R package version 1.14.8, \url{https://CRAN.R-project.org/package=data.table}.

#NOTE

#' @rdname rsp.rescale
#' @export
## #' @import data.table
#   now done in xxx.r

# may need to set data.table specifically??
#      data.table::as.data.table, etc??


######################
#rescale by profile
######################

##########################
#issues
############################

#think about...

# maybe rethink options for sp_rescale() when user sets by but NOT method...

# not actually sure anyone would want anything but method 1 for profiles...



rsp_rescale <- function(rsp, method = 2, by = "species"){

  #################################
  #check x is a respeciate object??

  ######################
  # SPECIEUROPE data
  ######################
  x <- rsp
  if("rsp_eu" %in% class(x)){
    x <- .rsp_eu2us(x)
  }
  #######################

  #check it has .value
  x <- .rsp_tidy_profile(x)

  #save to return as is..
  #    thinking about this
  tmp <- class(x)

  #was testing/thinking about
  #################################
  #backdoor
  #################################
  #could save species and profile identifiers, and
  #weight_percent then overwrite weight_percent with .value here
  #then copy back weight_percent after calculations
  #    then you could do multiple rescales without touching
  #    weight_percent???


  #################################
  #calculate stats
  xx <- data.table::as.data.table(x)
  #remove stats if there...
  test <- c(".min",".max",".total", ".mean", ".na", ".n", ".sd")
  test <- test[ test %in% colnames(xx)]
  if(length(test)>0){
    xx <- xx[, (test) := NULL]
  }

  #and recalculate

  #stop if by option not known.
  if(!by %in% c("species", "profile")){
    stop("unknown 'by' option",
         call.= FALSE)
  }

  if(by == "profile"){
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
              by=.(PROFILE_CODE)]

    out <- merge(xx, out, by="PROFILE_CODE", all.x=TRUE, all.y=FALSE,
                 allow.cartesian=TRUE)
  }
  if(by == "species"){
    out <- xx[,
              .(.min = min(WEIGHT_PERCENT, na.rm = TRUE),
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
  }


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
  #     any more???

  #might need an option to get all WEIGHT_PERCENT back from database...

  if(!method %in% c(0:5)){
    stop("unknown method")
  }
  if(method==0){
    out$.value<- out$WEIGHT_PERCENT
  }
  if(method==1){
    out$.value<- out$WEIGHT_PERCENT / out$.total
  }
  if(method==2){
    out$.value<- out$WEIGHT_PERCENT /out$.mean
  }
  if(method==3){
    #might be an issue if only one value
    out$.value <- (out$WEIGHT_PERCENT - out$.min) / (out$.max - out$.min)
  }
  if(method==4){
    #might be an issue if only one value
    out$.value <- (out$WEIGHT_PERCENT - out$.mean) / out$.sd
  }
  if(method==5){
    out$.value<- out$WEIGHT_PERCENT / out$.max
  }

  ############################
  #backdoor end
  ############################
  #this would need careful thinking about
  #     handling for method = 0 might be an issue
  #     maybe a sp_reset() would be better
  #         remove weight_percent and apply sp_pad(out, "weight")?

  #output
  #
  out <- as.data.frame(out)
  class(out) <- tmp
  out
}


#' @rdname rsp.rescale
#' @export

rsp_rescale_profile <- function(rsp, method = 1, by ="profile"){
  rsp_rescale(rsp=rsp, method=method, by=by)
}

#' @rdname rsp.rescale
#' @export

rsp_rescale_species <- function(rsp, method = 2, by ="species"){
  rsp_rescale(rsp=rsp, method=method, by=by)
}




###################
#unexported
##################


######################
#old.rescale data by species
######################

##########################
#issues
############################

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

# may need to think about additional local scaling
#      e.g. within in profile [species conc]/[sum of all species concs]

.rsp_rescale_species <- function(x, method = 2){

  #################################
  #check x is a respeciate object??

  #check it has .value
  x <- .rsp_tidy_profile(x)

  #save to return as is..
  #    thinking about this
  tmp <- class(x)

  #################################
  #calculate stats
  xx <- data.table::as.data.table(x)
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

  if(!method %in% c(0:5)){
    stop("unknown method")
  }
  if(method==0){
    out$.value<- out$WEIGHT_PERCENT
  }
  if(method==1){
    out$.value<- out$WEIGHT_PERCENT / out$.total
  }
  if(method==2){
    out$.value<- out$WEIGHT_PERCENT /out$.mean
  }
  if(method==3){
    #might be an issue if only one value
    out$.value <- (out$WEIGHT_PERCENT - out$.min) / (out$.max - out$.min)
  }
  if(method==4){
    #might be an issue if only one value
    out$.value <- (out$WEIGHT_PERCENT - out$.mean) / out$.sd
  }
  if(method==5){
    out$.value<- out$WEIGHT_PERCENT / out$.max
  }

  #output
  #
  out <- as.data.frame(out)
  class(out) <- tmp
  out
}








