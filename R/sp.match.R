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
#' @param min.n \code{numeric} (default 8), the minimum number of paired
#' species measurements in two profiles required for a match to be assessed.
#' See also \code{\link{sp_species_cor}}.
#' @param method Character (default 'pd'), the similarity measure to use, current
#' options 'pd', the Pearson's Distance (1- Pearson's correlation coefficient),
#' or 'sid', the Standardized Identity Distance (See References).
#' @param test.x Logical (default FALSE). The match process self-tests by adding
#' \code{x} to \code{ref}, which should generate a perfect fit=0 score. Setting
#' \code{test.x} to \code{TRUE} retains this as an extra record.
#' @return \code{sp_match_profile} returns a fit report: a \code{data.frame} of
#' up to \code{n} fit reports for the nearest matches to \code{x} from the
#' reference profile data set, \code{ref}.
#' @references Distance metrics are based on recommendations by Belis et al (2015)
#' and as implemented in Mooibroek et al (2022):
#'
#' Belis, C.A., Pernigotti, D., Karagulian, F., Pirovano, G., Larsen, B.R.,
#' Gerboles, M., Hopke, P.K., 2015. A new methodology to assess the performance
#' and uncertainty of source apportionment models in intercomparison
#' exercises. Atmospheric Environment, 119, 35–44.
#' https://doi.org/10.1016/j.atmosenv.2015.08.002.
#'
#' Mooibroek, D., Sofowote, U.M. and Hopke, P.K., 2022. Source apportionment of
#' ambient PM10 collected at three sites in an urban-industrial area with
#' multi-time resolution factor analyses. Science of The Total Environment,
#' 850, p.157981. http://dx.doi.org/10.1016/j.scitotenv.2022.157981.

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
##sp_match_profile(sp_profile("41220C"), aa)
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
                             min.n=8, method = "pd", test.x=FALSE){

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

  #note
  #    dropped the force option
  #    to override min.n if < min.n species in x
  #if(length(unique(x$SPECIES_ID)) < min.n & force){
  #  min.n <- length(unique(x$SPECIES_ID))
  #}

  ###############
  #do test anyway
  ###############
  #if(test.x){
    matches <- matches + 1
  #}

  x <- data.table::as.data.table(x)
  ref <- data.table::as.data.table(ref)
  .tmp <- data.table::rbindlist(list(x, ref), fill=TRUE)

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

  #.tmp <- data.table::as.data.table(sp_rescale_species(.tmp, method=rescale))
  .tmp <- data.table::as.data.table(sp_rescale_profile(.tmp, method=rescale))

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

  .tmp <- data.table::as.data.table(sp_dcast(.tmp, widen="profile"))

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

  #########################
  #method
  ########################
  #to do
  #   check with dennis re SID negative handling
  #   think about adding a log.pd
  #        but three options would mean we need stricter method handling...
  f <- FALSE
  if(tolower(method)=="pd"){
    # method pd
    f <- function(x) {
      if(length(x[!is.na(x) & !is.na(.test)])>min.n){
        suppressWarnings(1-cor(x, .test, use ="pairwise.complete.obs"))
      } else {
        NA
      }
    }
  }
  if(tolower(method)=="log.pd"){
    # method log pd
    # to think about
    #    drops if a lot are set to zero...
    f <- function(x) {
      if(length(x[!is.na(x) & !is.na(.test)])>min.n){
        suppressWarnings(1-cor(log10(x), log10(.test),
                               use ="pairwise.complete.obs"))
      } else {
        NA
      }
    }
  }
  if(tolower(method)=="sid.1"){
    # method SID
    ####################################
    #need to check this with dennis
    #how are negatives handled??
    ####################################
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test)
      x <- x[.ref]
      if(length(x)>min.n){
        .test <- as.vector(unlist(.test))[.ref]
        .ans <- (sqrt(2)/length(x))* (sum(((.test-x)/(.test+x)), na.rm = TRUE))
        if(.ans < 0) NA else .ans
        #.ans
      } else{
        NA
      }
    }
  }
  if(tolower(method)=="sid.2"){
    # method SID
    ####################################
    # based on reading I think this is closer??
    ####################################
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test)
      x <- x[.ref]
      if(length(x)>min.n){
        .test <- as.vector(unlist(.test))[.ref]
        mean(abs(x-.test)/.test, na.rm=TRUE)
      } else{
        NA
      }
    }
  }
  if(tolower(method)=="sid.3"){
    # method SID
    ####################################
    # based on reading I think this is closer??
    ####################################
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
      x <- x[.ref]
      if(length(x)>min.n){
        .test <- as.vector(unlist(.test))[.ref]
        #rescale x and ref may be different
        temp <- .test/x
        temp <- temp[is.finite(temp)]
        temp <- mean(temp, na.rm=TRUE)
        x <- x * temp
        ans <- mean(abs(x-.test)/.test, na.rm=TRUE)
        #rounding issue somewhere... or jitter... ???
        round(ans, digits=10)
      } else{
        NA
      }
    }
  }


  if(tolower(method)=="sid"){
    # method SID
    ####################################
    # based on reading I think this is closer??
    ####################################
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
      x <- x[.ref]
      if(length(x)>min.n){
        .test <- as.vector(unlist(.test))[.ref]
        #rescale x and ref may be different
        mod <- lm(.test~0+x, weights=1/x)
        x <- predict(mod)
        ans <- mean(abs(x-.test)/.test, na.rm=TRUE)
        #rounding issue somewhere... or jitter... ???
        round(ans, digits=10)
      } else{
        NA
      }
    }
  }

  if(!is.function(f)){
    stop("RSP> sp_match_profile 'method' unknown", call. = FALSE)
  }

  .out <- .tmp[, (.cols) := lapply(.SD, f), .SDcols = .cols]

  ##########################
  #chop down to build report
  ##########################

  #wonder if above is non-ideal??
  #    rows are replicates
  #    might be a better way of doing this???

  .out <- as.data.frame(.out[1, -1:-2])
  .out <- sort(unlist(.out), decreasing = FALSE)
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

  #conflicted!!!
  if(!test.x){
    matches <- matches - 1
    if("test" %in% x$PROFILE_CODE){
      .out <- .out[tolower(.out$PROFILE_CODE)!="test",]
    }
    if(nrow(.out) > (matches)){
      .out <- .out[1:matches,]
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





