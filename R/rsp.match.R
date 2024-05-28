#' @name rsp.match
#' @title Find nearest matches from reference set of profiles
#' @aliases rsp_match_profile

#' @description \code{rsp_match_profile} compares a supplied respeciate
#' profile (or similar data set) and a reference set of supplied profiles
#' and attempts to identify nearest matches on the
#' basis of similarity.
#' @param rsp A \code{respeciate} object or similar \code{data.frame} containing
#' a species profile to be compared with profiles in \code{ref}. If \code{rsp}
#' contains more than one profile, these are averaged (using
#' \code{\link{rsp_average_profile}}), and the average compared.
#' @param ref A \code{respeciate} object, a \code{data.frame} containing a
#' multiple species profiles, to be used as reference library when identifying
#' nearest matches for \code{rsp}.
#' @param matches Numeric (default 10), the maximum number of profile matches to
#' report.
#' @param rescale Numeric (default 5), the data scaling method to apply before
#' comparing \code{rsp} and profiles in \code{ref}: options 0 to 5 handled by
#' \code{\link{rsp_rescale}}.
#' @param min.n \code{numeric} (default 8), the minimum number of paired
#' species measurements in two profiles required for a match to be assessed.
#' See also \code{\link{rsp_cor_species}}.
#' @param method Character (default 'pd'), the similarity measure to use, current
#' options 'pd', the Pearson's Distance (1 - Pearson's correlation coefficient),
#' or 'sid', the Standardized Identity Distance (See References).
#' @param test.rsp Logical (default FALSE). The match process self-tests by adding
#' \code{rsp} to \code{ref}, which should generate a perfect fit=0 score. Setting
#' \code{test.rsp} to \code{TRUE} retains this as an extra record.
#' @return \code{rsp_match_profile} returns a fit report: a \code{data.frame} of
#' up to \code{n} fit reports for the nearest matches to \code{rsp} from the
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

#' @rdname rsp.match
#' @export

######################
#match
#find ref profile 'most similar to x
######################

############################
############################
## need to go through notes and code and tidy
## this is first goes at x->rsp
## also first to be worked through with SPECIATE + SPECIEEUROPE...
##    SO will need tidy and rethink?
###########################
###########################


#this uses
#########################
#  rsp_dcast (which uses data.table)
#  rbindlist from data.table

#in development

#to do
#########################

##aa <- rsp_profile(rsp_find_profile("composite", by="profile_name"))
##rsp_match_profile(rsp_profile("41220C"), aa)
##assuming 41220C exists

##NOTE rsp code is case sensitive
##   not sure if we can fix this??


#to think about
#########################

#match sometimes giving warning
#   In min(WEIGHT_PERCENT, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
#       when (I guess) nothing there to compare...

#could include a default for ref
#    currently using rsp_profile(rsp_find_profile("composite", by="profile_name"))
#        in example.

#could add error if x is more than one profile
#    could use rsp_profile_mean when written if we want to force to one
#         one profile [?? nb: that function name not decided yet]

#do we want an output option?
#    have a basic report table but could

#option to exclude test? from report

#how can users make rsp if not from respeciate archive
#   currently usign rsp_build_x
#       previously used .rsp_ [might still be around?? maybe in xxx.r ???]
#   suggestion:
#       identify needed columns, formats and names
#       maybe can build from at least:
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

# often have what looks like repliactes in SPECAITE, e.g.
## a <- rsp(80, source="eu"); rsp_match_profile(a, rsp_q_pm(), method="pd", matches=20);a
## PROFILE_CODE                                      PROFILE_NAME          fit
## 1       3400410                            Brake Lining, Asbestos 0.0003941924
## 2      340042.5                            Brake Lining, Asbestos 0.0003941924
## 3       3400430                            Brake Lining, Asbestos 0.0003941924
## 4        34004C                            Brake Lining, Asbestos 0.0003941924
## 5          4721                      Iron and Steel Manufacturing 0.0038384899
## 6          4722                      Iron and Steel Manufacturing 0.0038533767
## 7          4720                      Iron and Steel Manufacturing 0.0039276082
## 8       1121010                            Coal-Fired Power Plant 0.0039901471
# very similar profile code and profile names and fits are exactly the sample
#      (check that)

# currently assuming zeros are missing cases...
#     that might not be right...
#         could try setting zeros to
#            10/min(c(x[x>0], .test[.test>0]), na.rm=TRUE)
#          as test for SID...
#          pd will run fine with zeros but would be an issue for
#               10, rest zero's
#               (see notes...)

rsp_match_profile <- function(rsp, ref, matches=10, rescale=5,
                             min.n=8, method = "pd", test.rsp=FALSE){

  #######################
  #if ref missing
  ##################
  #to do
  #   using rsp_profile(rsp_find_profile("composite", by="profile_name"))
  #   looked promising

  #add .value if not there
  x <- .rsp_tidy_profile(rsp)

  #tidy x for testing
  #.x.pr.cd <- as.character(x$PROFILE_CODE)
  #.x.pr.nm <- as.character(x$PROFILE_NAME)

  #note
  #   assuming only one profile
  #   might think about changing this in future

  if(length(unique(x$PROFILE_CODE))>1){
    x <- rsp_average_profile(x, code = "test")
  } else {
    x <- rsp_average_profile(x, code = "test",
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
  #if(test.rsp){
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
  .tmp <- data.table::as.data.table(rsp_rescale_profile(.tmp, method=rescale))

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
  .tmp <- data.table::as.data.table(rsp_dcast(.tmp, widen="profile"))

  #nb: need the as.data.table() because rsp_profile_dcast

  #to think about:
  #    an as.is option in functions to made outputs same as inputs?
  #    disable the 'as is' assumption in previous code
  #        BUT now not sure why...

  #ignore first two columns
  #    species_id and species_name
  .cols <- names(.tmp)[3:(ncol(.tmp))]

  #get x back as a test case...
  #need a better way to handle test
  #     could as.vector(unlist(.test)) be done here
  #         (just don't do .test[.ref] here...)
  .test <- .tmp[, "test"]

  ###########################
  #fit term
  #    currently using pd 1-r and sid based on jcr method

  #nb: we see a warning
  #     think it is just cases when no variance in the xy data
  #          so only one pair or (all xs same and all ys sames)???

  #########################
  #min n
  #########################
  #to do
  #   compare this and code in rsp_cor_species
  #   if/when we deal with this stop message this code may need to be updated

  #########################
  #method
  ########################

  # currently documenting pd and sid methods

  #to do
  #   check with dennis re SID negative handling
  #
  #   think about methods
  #        but three options would mean we need stricter method handling...

  f <- FALSE
  if(tolower(method)=="pd"){
    # method pd
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
      x <- x[.ref]
      if(length(x)<min.n){
        NA
      } else {
        .t2 <- as.vector(unlist(.test))[.ref]
        suppressWarnings(1-cor(x, .t2, use ="pairwise.complete.obs"))
      }
    }
  }
  if(tolower(method)=="sid"){
    # method SID
    ####################################
    # current best estimate of a normalised SID??
    #      (we need to normalise because we do not know
    #       that all profiles are on same scale...)
    ####################################
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
      x <- x[.ref]
      if(length(x)<min.n){
        NA
      } else{
        .t2 <- as.vector(unlist(.test))[.ref]
        #rescale x and ref may be different
        mod <- lm(.t2~0+x, weights=1/x)
        x <- predict(mod)
        ans <- mean(abs(x-.t2)/.t2, na.rm=TRUE)
        #rounding issue somewhere... or jitter... ???
        round(ans, digits=10)
      }
    }
  }

  ############################################
  # following methods are undocumented tests...
  #    draft template below because
  #        all earlier versions were a little different...
  #        and it would be sensible to be

  #if(tolower(method)=="whatever"){
  #  # method whatever
  #  ####################################
  #  # why are we testing/using this??
  #  ####################################
  #  f <- function(x) {
  #    .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
  #    x <- x[.ref]
  #    if(length(x)<min.n){
  #      NA
  #    } else{
  #      .t2 <- as.vector(unlist(.test))[.ref]
  ########################################
  #    code for method.........
  #         compare x and .t2
  #         output as ans...
  #########################################
  #      #rounding issue somewhere... or jitter... ???
  #      round(ans, digits=10)
  #    }
  #  }
  #}

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
    # based on reading I think this is closer to a locally normalised form
    #       their SID??
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
    # sid.2 using zero's as points
    #       and a series e.g. 10, 0 , 0, ...
    #           correlates highly with anything where
    #           first element is higher than rest...
    #       and I think some 0s are missing values...
    ####################################
    f <- function(x) {
      .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
      x <- x[.ref]
      if(length(x)>min.n){
        .test <- as.vector(unlist(.test))[.ref]
        #rescale x and ref may be different
        temp <- .test/x
        temp <- temp[is.finite(temp)]
        # note
        #######################################
        #    also x/0 and 0/x is complicated...
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

  # no more methods after here...
  ###################################################

  if(!is.function(f)){
    stop("RSP> rsp_match_profile 'method' unknown", call. = FALSE)
  }

  #notes
  ##############################

  # next bit seems slower than I was expecting
  #      maybe better was to do this
  #          anyone any ideas ???

  # be nice to be able to calculate
  #      more than one stat at a time...
  #      maybe look at plotting two, e.g. sid vs pd as an output...
  .out <- .tmp[, (.cols) := lapply(.SD, f), .SDcols = .cols]

  # suspect above is non-ideal??
  #    rows are replicates
  #    might be a better way of doing this???


  ##########################
  #chop down to build report
  ##########################

  # suspect below is non-ideal??
  #    should be a tidier/safer way of handling this...

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
    stop("rsp_match_profile> No (", min.n, " point) matches for rsp", call. = FALSE)
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
  if(!test.rsp){
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








