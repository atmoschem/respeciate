#' @name sp.match
#' @title Find nearest matches from reference set of profiles
#' @aliases sp_match_profile

#' @description \code{sp_match_profile} compares a supplied species
#' (re)SPECIATE profile and a reference set of supplied profiles and
#' attempt to identify nearest matches on the basis of correlation
#' coefficient.
#' @param x A \code{respeciate} object or similar \code{data.frame} containing
#' a species profile to be compared with profiles in \code{ref}. If \code{x}
#' contains more than one profile, these are averaged (using
#' \code{\link{sp_average_profile}}), and the average compared.
#' @param ref A \code{respeciate} object, a \code{data.frame} containing a
#' multiple species profiles, to be used as reference library when identifying
#' nearest matches for \code{x}.
#' @param matches Numeric (default 10), the maximum number of profile matches to
#' report.
#' @param rescale Numeric (default 5), the data scaling method to apply before
#' comparing \code{x} and profiles in \code{ref}: options 0 to 5 handled by
#' \code{\link{sp_rescale}}.
#' @param min.n \code{numeric} (default 6), the minimum number of paired
#' species measurements in two profiles required for a match to be assessed.
#' See also \code{\link{sp_species_cor}}.
#' @param test.x Logical (default FALSE). The match process self-tests by adding
#' \code{x} to \code{ref}, which should generate a perfect fit=1 score. Setting
#' \code{test.x} to \code{TRUE} retains this as an extra record.
#' @return \code{sp_match_profile} returns a fit report: a \code{data.frame} of
#' up to \code{n} fit reports for the nearest matches to \code{x} from the
#' reference profile data set, \code{ref}.

#NOTE

#' @rdname sp.match
#' @export

######################
#match
#find ref profile 'most similar to x
######################

#  the sp_dcast uses data.table
#      sp_match uses rbindlist from data.table

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

#match is giving warning
#   In min(WEIGHT_PERCENT, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
#       when (I guess) nothing there to compare...

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

#renaming appears to trip up when profile_name/id are factors
#    current fix is to force .tmp.pf.cd and .tmp.pf.nm to character
#          when extracting them...
#    maybe think about better?

#at the moment we error (stop()) function if no correlation outputs
#    so far this appears to be mostly because number of species in x
#         is less than min.bin for correlation.
#              1. this is currently hard-coded
#              2. not really looked at this in detail...
#                  could return a NULL and warning instead?
#                  could also do this earlier if min.bin set in formals
#                     but might need to rethink n, min.bin, etc???

sp_match_profile <- function(x, ref, matches=10, rescale=5,
                             min.n=6, test.x=FALSE){

  #######################
  #if ref missing
  ##################
  #to do
  #   using sp_profile(sp_find_profile("composite", by="profile_name"))
  #   looked promising

  #add .value if not there
  x <- rsp_tidy_profile(x)

  #tidy x for testing
  #.x.pr.cd <- as.character(x$PROFILE_CODE)
  #.x.pr.nm <- as.character(x$PROFILE_NAME)

  #note
  #   assuming only one profile
  #   might think about changing this in future

  if(length(unique(x$PROFILE_CODE))>1){
    x <- sp_average_profile(x, code = "test")
  } else {
    x <- sp_average_profile(x, code = "test",
                            name = paste("test>", x$PROFILE_NAME[1], sep=""))
  }

  if(test.x){
    matches <- matches + 1
  }

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

  #might handle sp_rescale_species in/output
  #   either returns object in class it is given
  #      or errors if not respeciate
  #      or errors if not respeciate unless forced
  #          but then maybe need to check requires
  #          cols are there???

  .tmp <- as.data.table(sp_rescale_species(.tmp, method=rescale))

  ###################
  #keep species names and ids for renaming
  #    as.character to stop rename tripping on factors
  #         not ideal...
  ###################
  .tmp.pr.nm <- as.character(.tmp$PROFILE_NAME)
  .tmp.pr.cd <- as.character(.tmp$PROFILE_CODE)

  ##################
  #dcast to reshape for search
  #    cols (profiles), rows(species)
  ##################

  #testing: using sp_profile_dcast
  #         code is set up for sp_profile_dcast(.tmp, widen="profile")

  ##previous code
  #.tmp <- dcast(.tmp,
  #              SPECIES_ID + SPECIES_NAME ~PROFILE_CODE,
  #              mean,
  #              na.rm=TRUE,
  #              value.var = ".value")

  .tmp <- as.data.table(sp_dcast(.tmp, widen="profile"))

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
  #need a better way to handle test
  .test <- .tmp[, "test"]

  ###########################
  #fit term
  #    currently using conventional regression coefficient

  #nb: we see a warning
  #     think it is just cases when no variance in the xy data
  #          so only one pair or (all xs same and all ys sames)???

  #########################
  #min n
  #########################
  #to do
  #   compare this and code in sp_species_cor
  #   if/when we deal with this stop message this code may need to be updated

  f <- function(x) {
    if(length(x[!is.na(x) & !is.na(.test)])>min.n){
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
  if(length(.out) > matches){
    .out <- .out[1:matches]
  }

  #could be better way to do next bit
  #just making a lookup for profile info

  #this is a pain because some profile names are replicated
  #    (several profile codes seem to have same profile name)

  if(length(.out)<1){
    #see notes....
    #sometimes this is because there are less than min.n species in the x profile
    stop("sp_match_profile> No (", min.n, " point) matches for x", call. = FALSE)
  }

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

  if(!test.x){
    if("test" %in% x$PROFILE_CODE){
      .out <- .out[tolower(.out$PROFILE_CODE)!="test",]
    } else {
      .out <- .out[1:(matches-1),]
    }
  }

  #######################
  #output
  #######################

  #think about options?
  rownames(.out) <- NULL
  return(as.data.frame(.out))
}





#need something to replace this that helps users build local profiles

#basic build needs
#   profile_name and profile_code
#   species_name and species_id
#   weight_percent (and possibly .value)

rsp_ <- function(x){
  .o <- sp_profile(x)
  .o$PROFILE_NAME <- paste("test", .o$PROFILE_NAME, sep=">")
  .o$PROFILE_CODE <- "test"
  .o
}





