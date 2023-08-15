#' @name sp.match
#' @title Find nearest matches from reference set of profiles
#' @aliases sp_match_profile

#' @description sp_profile functions identify nearest matches in a reference
#' set of (re)SPECIATE profiles for a supplied species profile.

#' @description \code{sp_match_profile} attempts to identify the nearest
#' matches to a supplied profile from a reference set of (re)SPECIATE profiles
#' on the basis of correlation coefficient.
#' @param x A \code{respeciate} object, a \code{data.frame} containing a
#' species profile to be compared with profiles in \code{ref}.
#' @param ref A \code{respeciate} object, a \code{data.frame} containing a
#' multiple species profiles, to be used as reference library when identifying
#' matches with \code{x}.
#' @param n Numeric (default 10), the maximum number of profile matches to
#' report.
#' @param rescale Numeric (default 4), the data scaling method to apply before
#' comparing \code{x} and profiles in \code{ref}: options 0 to 4 handled by
#' \code{\link{sp_rescale_profile}}.
#' @return \code{sp_match_profile} returns a fit report: a \code{data.frame} of
#' up to \code{n} fit reports for the nearest matches to \code{x} from the
#' reference profile data set, \code{ref}.

#NOTE

#' @rdname sp.match
#' @export

#  using data.table for dcast

######################
#match
#find ref profile 'most similar to x
######################

#in development

#to do
#########################



##aa <- sp_profile(sp_find_profile("composite", by="profile_name"))
##sp_match_profile(subset(rsp_("41220C")), aa)
##assuming 41220C exists

##NOTE sp_profile code is case sensitive
##   not sure if we can fix this??


#to think about
#########################

#default for ref
#    using sp_profile(sp_find_profile("composite", by="profile_name"))
#        in example.

#could add error if x is more than one profile
#    could use sp_profile_mean when written if we want to force to one
#         one profile [?? nb: that function name not decided yet]

#do we want an output option?
#    have a basic report table but could

#option to exclude test? from report

#how can users make x if not from (re)SPECIATE archive
#    currently using rsp_ [code after this function]
#         to anonymise a speciate profile
#    suggestion:
#         identify needed columns, formats and names
#         maybe can build from at least:
#             species_name and species_id
#             weight_percent or .value


sp_match_profile <- function(x, ref, n=10, rescale=4){

  #######################
  #if ref missing
  ##################
  #to do
  #   using sp_profile(sp_find_profile("composite", by="profile_name"))
  #   looked promising

  #add .value if not there
  x <- rsp_tidy_profile(x)

  x <- as.data.table(x)
  ref <- as.data.table(ref)
  .tmp <- rbindlist(list(x, ref), fill=TRUE)

  #################
  #think about this
  #################

  #no normalisation option is method = 0 (via sp_profile_rescale)

  #note: think about calling this rescale because it is the rescale method
  #      (not match method) we are setting

  #rescale data
  ################################
  #note: currently outputting data.frame
  #      (idea is we are not restricting the user to data.table)
  #      (but it means we have to reset and I am guessing there is
  #       a time/memory penalty for that??)

  #might handle sp_profile_rescale in/output
  #   either returns object in class it is given
  #      or errors if not respeciate
  #      or errors if not respeciate unless forced
  #          but then maybe need to check requires
  #          cols are there???

  .tmp <- as.data.table(sp_rescale_profile(.tmp, method=rescale))

  ###################
  #keep species names and ids for renaming
  ###################
  .tmp.pr.nm <- .tmp$PROFILE_NAME
  .tmp.pr.cd <- .tmp$PROFILE_CODE

  ##################
  #dcast to reshape for search
  #    cols (profiles), rows(species)
  ##################

  #testing: using sp_profile_dcast
  #         code is set up for sp_profile_dcast(.tmp, wide="profile")

  ##previous code
  #.tmp <- dcast(.tmp,
  #              SPECIES_ID + SPECIES_NAME ~PROFILE_CODE,
  #              mean,
  #              na.rm=TRUE,
  #              value.var = ".value")

  .tmp <- as.data.table(sp_dcast_profile(.tmp, wide="profile"))

  #nb: need the as.data.table() because sp_profile_dcast
  #    currently returns data.frame
  #    we are using data.table but I am trying to avoid forcing
  #    users into doing the same...

  #to think about:
  #    an as.is option in functions to made outputs same as inputs?
  #    disable the 'as is' assumption in previous code
  #        BUT now not sure why...

  #ignore first two columns
  #    species_id and species_name
  .cols <- names(.tmp)[3:(ncol(.tmp))]

  #get x back as a test case...
  .test <- .tmp[, "test"]

  ###########################
  #fit term
  #    currently using conventional regression coefficient

  #nb: we see a warning
  #     think it is just cases when no variance in the xy data
  #          so only one pair or (all xs same and all ys sames)???

  #########################
  #min bin
  #########################
  #to do
  #   currently hard coded in f
  #   as 6

  f <- function(x) {
    if(length(x[!is.na(x) & !is.na(.test)])>6){
      suppressWarnings(cor(x, .test, use ="pairwise.complete.obs"))
    } else {
      NA
    }
  }
  .out <- .tmp[, (.cols) := lapply(.SD, f), .SDcols = .cols]

  ##########################
  #chop down to build report
  ##########################

  #wonder if above is non-ideal??
  #    rows are replicates
  #    might be a better way of doing this???

  .out <- as.data.frame(.out[1, -1:-2])
  .out <- sort(unlist(.out), decreasing = TRUE)
  if(length(.out) > n){
    .out <- .out[1:n]
  }

  #could be better way to do next bit
  #just making a lookup for profile info

  #this is a pain because some profile names are replicated
  #    (several profile codes seem to have same profile name)

  .tmp <- names(.out)
  for(i in 1:length(.tmp)){
    if(.tmp[i] %in% .tmp.pr.cd){
      .tmp[i] <- .tmp.pr.nm[.tmp.pr.cd == names(.out)[i]][1]
    }
  }

  #row.names fulled form .out if you don't overwrite

  .out <- data.frame(PROFILE_CODE=names(.out),
                     PROFILE_NAME=.tmp,
                     fit=.out,
                     row.names = 1:length(.out))

  #######################
  #output
  #######################

  #think about options?
  return(as.data.frame(.out))
}





#need something to replace this that helps users build local profiles

rsp_ <- function(x){
  .o <- sp_profile(x)
  .o$PROFILE_NAME <- paste("test", .o$PROFILE_NAME, sep=">")
  .o$PROFILE_CODE <- "test"
  .o
}




