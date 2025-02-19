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
#' @param min.n Numeric (or \code{NULL}), the minimum number of paired species
#' measurements required for a match to be assessed. The larger \code{min.n},
#' the greater the required \code{rsp} and \code{ref} profile overlap, so the
#' better the matching confidence for paired cases but also the more likely
#' that a sparse but relevant \code{ref} profile may be missing. The default
#' option, \code{NULL}, is 65\% of the number of species in \code{rsp} or 6
#' if larger.
#' @param method Character (default 'sid * srd'), the ranking metric used to
#' rank profile matches. The function calculates several matching metrics:
#' 'pd', the Pearson's Distance (1 - Pearson's correlation coefficient),
#' 'srd', like pd but using the Spearman Ranked data correlation coefficient,
#' and 'sid', the Standardized Identity Distance (See References). All the
#' metrics tend to zero for better matches, and the \code{method} can be
#' any character string that can be evaluated from any of these, e.g.,
#' \code{'pd'}, \code{'srd'}, \code{'sid'}, and combinations thereof.
#' @param self.test Logical (default FALSE). The match process self-tests by adding
#' \code{rsp} to \code{ref}, which should generate an ideal (nearness = 0) score.
#' Setting \code{self.test} to \code{TRUE} retains this as an extra record.
#' @param ... Additional arguments, typically ignore but sometimes used for
#' function development. Currently, testing \code{rm.reps} (logical) option to
#' remove what appear to be replicate profile matches from the result set. This
#' is based on the assumption that identical 'pd' and 'sid' scores identical
#' identical \code{ref} profiles (or identical overlaps with \code{rsp}) but is
#' not validated, so handle with care...
#' @param output Character, output options, including: \code{'summary'} (the
#' default) a \code{data.frame} of the requested best \code{matches}, ranked
#' according to the \code{method} used; \code{'data'} the full data set used
#' to make plots; \code{'plot'} the associated output from
#' \code{\link{rsp_plot_match}}; or, a combination of these.
#' @return By default \code{rsp_match_profile} returns a fit report summary: a
#' \code{data.frame} of up to \code{matches} fit reports for the nearest
#' matches to profiles from the reference profile data set, \code{ref}. (See
#' also \code{output} above for other options). If several options are requested,
#' earlier options are report (e.g. using \code{print} or \code{plot}) and only
#' the final option is returned.
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
#'
#' @seealso \code{\link{rsp_plot_match}}

#NOTE

######################
#match
#find ref profile 'most similar to rsp in ref'
######################

############################
############################
## need to go through notes and code and tidy
## this is first goes at x->rsp
## also first to be worked through with SPECIATE + SPECIEEUROPE...
##    SO will need tidy and rethink?
###########################
###########################


#####################################
# rsp_match_profile
#####################################

# kr v.0.3 2025-01-28

#this uses
#########################
#  rsp_dcast/rsp_melt (which uses data.table)
#  rbindlist from data.table

#in development

#to do
#########################

##aa <- rsp_profile(rsp_find_profile("composite", by="profile_name"))
##rsp_match_profile(rsp_profile("41220C"), aa)
##assuming 41220C exists

##NOTE rsp code is case sensitive
##   not sure if we can fix this??


## compare SPECIEUROPE and SPECIATE
## a <- rsp(3, source="eu"); rsp_match_profile(a, rsp_q_pm(), method="sid", min.n=2)
## will not work or match well...


#to think about
#########################

#match sometimes giving warning
#   In min(WEIGHT_PERCENT, na.rm = TRUE) :
#   no non-missing arguments to min; returning Inf
#       when (I guess) nothing there to compare...
#           THIS OLD???

#could include a default for ref
#    currently using rsp_profile(rsp_find_profile("composite", by="profile_name"))
#        in example.

#could add error if x is more than one profile
#    could use rsp_profile_mean when written if we want to force to one
#         one profile [?? nb: that function name not decided yet]

#do we want an output option?
#    have a basic report table but could

#how can users make rsp if not from respeciate archive
#   currently using rsp_build_x
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

# often have what looks like replicates in SPECAITE, e.g.
## a <- rsp(80, source="eu"); rsp_match_profile(a, rsp_us_pm(), method="pd");a
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
#          could include option to make zero's something like min * 0.1 ??


#tested this time_check
#################################
# code currently in grey.area


#a <- rsp(80, source="eu"); rsp_test_match(a, rsp_us_pm(), method="sid", matches=10);a
#rsp_match_profile(rsp(172, source="eu"), rsp_us_pm(), method="sid2 * pd", self.test = TRUE, min.n=10)

#a <- rsp_eu_pm2.5()
#unique(a$.profile.id) #valid cases ???


#' @rdname rsp.match
#' @export

#current tests

##> rsp_match_profile(rsp(1, source="eu"), rsp_eu())
##.profile.id                                                             .profile  n          pd        srd          sid   nearness
##1         EU:1                                                               Cement 37 0.000000000 0.00000000 0.0000000000 0.00000000
##2        EU:32                                             Cement kiln (coal fired) 36 0.007359504 0.00000000 0.0005234852 0.00000000
##3        EU:37 Production of fire proof material for steel production (natural gas) 37 0.329987298 0.05377620 0.2725989157 0.01465934
##4        EU:33                                               Boiler coal Fired <5MW 37 0.431629628 0.07044033 0.5812085383 0.04094052
##5        EU:27                                           Powerplant coal combustion 37 0.597289509 0.11640715 0.3736188810 0.04349191
##6       EU:278                                               Poor state of pavement 24 0.117074094 0.09589039 0.4669850279 0.04477938
##7       EU:277                                                        Near by works 25 0.114951117 0.12846154 0.4177102696 0.05365970
##8       EU:275                                                          Urban roads 25 0.154247300 0.13461538 0.4159940526 0.05599920
##9        EU:28                  Powerplant industial,coal (and coke gas) combustion 37 0.822872828 0.12186111 0.4853303384 0.05914289
##10      EU:258                                                         Ore terminal 32 0.276827403 0.12417348 0.5204505221 0.06462615

##> rsp_match_profile(rsp(1, source="eu"), rsp_us_pm())
##.profile.id                            .profile  n        pd        srd       sid   nearness
##1      US:4377                         Cement Kiln 28 0.2976599 0.05338075 0.2387444 0.01274435
##2     US:91004 Draft Cement Production - Composite 28 0.1777894 0.06022444 0.2547280 0.01534085
##3      US:4332                         Cement Kiln 28 0.2361266 0.06310738 0.2441649 0.01540861
##4      US:4378                         Cement Kiln 28 0.3639180 0.06283360 0.2647696 0.01663643
##5      US:4365                  Vegetative Burning 25 0.4610301 0.06430434 0.3079190 0.01980053
##6   US:2720330            Cement Kiln (Coal-Fired) 24 0.2286342 0.07597372 0.2987559 0.02269760
##7   US:2720310            Cement Kiln (Coal-Fired) 26 0.3984772 0.08132060 0.3069426 0.02496076
##8  US:272032.5            Cement Kiln (Coal-Fired) 26 0.3984772 0.08132060 0.3069426 0.02496076
##9    US:27203C            Cement Kiln (Coal-Fired) 26 0.3984772 0.08132060 0.3069426 0.02496076
##10     US:4331                         Cement Kiln 26 0.6171709 0.09342868 0.2787134 0.02603982

#possible example

#a <- rsp_match_profile(rsp(1, source="eu"), rsp_eu())
#b <- rsp(a)
#b <- b[order(factor(b$.profile.id, levels=unique(a$.profile.id))),]
#rsp_plot_match(rsp(1, source="eu"), b)

#rsp_find_profile("steel", "manuf")
#a <- rsp(4718, source="US")
#rsp_match_profile(a, rsp_eu())


##########################
# next steps
##########################

# tidy up the output section at end of code
#      also finish documenting output when we do this!
# like to have data.table alternatives to switching data type
#     identify where switching data... .table/.frame/.table
#           add and test alternatives...
#           (one is for the eval method)

rsp_match_profile <- function(rsp, ref, matches=10, rescale=5,
                              min.n=NULL, method = "sid * srd",
                              self.test=FALSE, ..., output = "summary"){

  ####################
  #setup
  ####################

  #if ref missing ???

  .xargs <- list(...)

  #rsp handling
  x <- .rsp_tidy_profile(rsp)
  #averaging
  #   assuming only one x profile
  #   or something to treat as one profile
  if(length(unique(x$.profile.id))>1){
    x <- rsp_average_profile(x, code = "test")
  } else {
    x <- rsp_average_profile(x, code = "test",
                             name = paste("test>", x$.profile[1], sep=""))
  }
  x <- data.table::as.data.table(x)

  # if min.n assignment is undeclared
  .test <- length(unique(x$.species.id[!is.na(x$.value) & x$.value>0]))
  if(is.null(min.n)){
    min.n <- floor(.test*0.65)
    if(min.n < 6){
      min.n <- 6
    }
  }
  if(min.n < 3){
    min.n <- 3
  }
  if(min.n > .test){
    stop("rsp_match_profile> min.n (" , min.n, ") exceeds species in rsp",
         call. = FALSE)
  }

  # get ref profile order
  .ref.pr.ord <- if(is.factor(ref$.profile.id)) {
    levels(ref$.profile.id)
  } else {
    unique(ref$.profile.id)
  }
  #ref profiles
  ref <- data.table::as.data.table(ref)

  #merge for matching
  .tmp <- data.table::rbindlist(list(x, ref), fill=TRUE)
  .tmp <- data.table::as.data.table(rsp_rescale_profile(.tmp, method=rescale))
  .tmp <-rsp_melt_wide(rsp_dcast(as.data.frame(.tmp), widen="profile.id"),
                      drop.nas=FALSE, pad=FALSE)
  .tmp <- data.table::merge.data.table(.tmp, x, by=c(".species.id", ".species"),
                                       suffixes = c(".ref", ".x"))

  #################
  # calc methods
  ##################

  #correction for .value.x. versus .value.ref
  .correct <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x>0 & .test>0
    x <- x[.ref]
    if(length(x) < 3){
      NA_real_
    } else {
      .t2 <- .test[.ref]
      #mean(.t2/x)
      mean(x/.t2)
    }
  }
  #n
  .n <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x>0 & .test>0
    x <- x[.ref]
    length(x)
  }
  #pd (pearson distance)
  .pd <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
    x <- x[.ref]
    if(length(x)<3){
      NA_real_
    } else {
      #.t2 <- as.vector(unlist(.test))[.ref]
      .t2 <- .test[.ref]
      ans <- suppressWarnings(1-cor(x, .t2,
                                    use ="pairwise.complete.obs",
                                    method = "pearson"))
      round(ans, digits=12)
    }
  }
  #srd (spearman ranked distance)
  .srd <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
    x <- x[.ref]
    if(length(x)<3){
      NA_real_
    } else {
      #.t2 <- as.vector(unlist(.test))[.ref]
      .t2 <- .test[.ref]
      ans <- suppressWarnings(1-cor(x, .t2,
                                    use ="pairwise.complete.obs",
                                    method = "spearman"))
      round(ans, digits=12)
    }
  }
  #sid ( Standardized Identity Distance, see Belis and Mooibroek)
  .sid <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x>0 & .test>0
    x <- x[.ref]
    if(length(x) < 3){
      NA_real_
    } else {
      .t2 <- .test[.ref]
      #x <- x * mean(.t2/x)
      .t2 <- .t2 * mean(x/.t2)
      ans <- mean(((x-.t2)^2)/((x+.t2)^2), na.rm=TRUE)
      #rounding issue somewhere... or jitter... ???
      round(ans, digits=12)
    }
  }
  #other calcs
  #zzz (see notes)
  .zzz <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x>0 & .test>0
    x <- x[.ref]
    if(length(x) < 3){
      NA_real_
    } else {
      .t2 <- .test[.ref]
      #x <- x * mean(.t2/x)
      .t2 <- .t2 * mean(x/.t2)
      ans <- mean((abs(x-.t2)*(1/sqrt(2)))/((x+.t2)/2), na.rm=TRUE)
      #rounding issue somewhere... or jitter... ???
      round(ans, digits=12)
    }
  }
  # krd (kendall ranked distance)
  .krd <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x!=0 & .test!=0
    x <- x[.ref]
    if(length(x)<3){
      NA_real_
    } else {
      #.t2 <- as.vector(unlist(.test))[.ref]
      .t2 <- .test[.ref]
      ans <- suppressWarnings(1-cor(x, .t2,
                                    use ="pairwise.complete.obs",
                                    method = "kendall"))
      round(ans, digits=12)
    }
  }
  #log pearson
  .log.pd <- function(x) {
    .ref <- !is.na(x) & !is.na(.test) & x>0 & .test>0
    # not getting eq.4 in Bellis et al 2015
    #x <- -1/log(x[.ref], base=exp(1))
    #x[!is.finite(x)] <- 0
    x <- log(x[.ref], base=exp(1))
    if(length(x)<3){
      NA_real_
    } else {
      .t2 <- log(.test[.ref], base=exp(1))
      suppressWarnings(1-cor(x, .t2, use ="pairwise.complete.obs"))
    }
  }

  #main calculates
  .test <- as.data.frame(subset(.tmp, .profile.id.ref=="test"))$.value.x
  .tmp <- data.table::as.data.table(.tmp)

  if("test.output" %in% names(.xargs) && .xargs$test.output){
    #test output (showning all metrics being tested/considered)
    .ans <- .tmp[, .(
      .profile = .profile.ref[1],
      corr = .correct(.value.ref),
      n = .n(.value.ref),
      pd = .pd(.value.ref),
      srd = .srd(.value.ref),
      krd = .krd(.value.ref),
      log.pd = .log.pd(.value.ref),
      sid = .sid(.value.ref),
      zzz = .zzz(.value.ref)
    ), by=.(.profile.id.ref)]
  } else {
    # standard output
    .ans <- .tmp[, .(
      .profile = .profile.ref[1],
      corr = .correct(.value.ref),
      n = .n(.value.ref),
      pd = .pd(.value.ref),
      srd = .srd(.value.ref),
      sid = .sid(.value.ref)
    ), by=.(.profile.id.ref)]
  }

  #thinking this should be do-able in data.table...
  .ans <- as.data.frame(.ans)
  .test <- try(with(as.data.frame(.ans), eval(parse(text=method))),
               silent = TRUE)
  if(class(.test)[1]=="try-error"){
    stop("rsp_match_profile> Sorry, can't evaluate method (", method, ")",
        call. = FALSE)
  }
  .ans$nearness <- .test

  # rm.reps via ...
  #  option to remove what look like replicated profile matches
  #  BUT keep test no matter what
  if("rm.reps" %in% names(.xargs) && .xargs$rm.reps){
    .ans <- .ans[!duplicated(.ans$pd) & !duplicated(.ans$sid) | .ans$.profile.id.ref=="test",]
  }

  .ans <- data.table::as.data.table(.ans)[n >= min.n][order(nearness)]
  if(self.test){
    .ans <- rbind(.ans[.profile.id.ref=="test"],
                 .ans[.profile.id.ref!="test"])
    .ans <- .ans[1:(matches+1)]
  } else {
    .ans <- .ans[.profile.id.ref!="test"]
    .ans <- .ans[1:matches]
  }

  #setup plot.data
  .tmp <- data.table::merge.data.table(.tmp, .ans, by=c(".profile.id.ref"),
                                       all.x=FALSE, all.y=TRUE)
  .tmp <- .tmp[order(nearness)]
  .tmp[, .value.ref := .value.ref / corr]
  .tmp[, .test.z := (.value.ref + .value.x) /2]
  .tmp[, .test.z1 := min(c(.value.ref, .value.x)), by=1:nrow(.tmp)]
  .tmp[, .test.z2 := max(c(.value.ref, .value.x)), by=1:nrow(.tmp)]
  .tmp <- .tmp[order(nearness)]
  # like to stay in data.table if possible ???
  .tmp <- as.data.frame(.tmp)
  .tmp <- .tmp[!is.na(.tmp$.test.z) & !is.na(.tmp$.test.z1) &
               !is.na(.tmp$.test.z2), ]
  .tmp$.test.zn <- .tmp$.test.z/.tmp$.test.z
  .tmp$.test.z1n <- .tmp$.value.ref/.tmp$.test.z
  #reorder
  .tmp <- .tmp[order(factor(.tmp$.profile.id.ref,
                            levels=c(unique(.ans$.profile.id.ref)))), ]
  .tmp$.profile.id.ref <- factor(.tmp$.profile.id.ref,
                                levels=c(unique(.ans$.profile.id.ref)))

  #tidy .ans
  names(.ans) <- gsub("[.]ref$", "", names(.ans))
  .ans <- .ans[, !"corr"]

  #output options
  ##########################
  # this needs more thinking about/tidying
  #    check how widely we are using output
  #    standardise use/options, so it and others are concistent...

  output <- tolower(paste(output, collapse=","))

  #output
  ################
  #shortcuts
  if(output=="summary"){
    return(as.data.frame(.ans))
  }
  ls <- list(summary=.ans, data=.tmp)
  if(output=="plot.data"){
    return(list(summary=.ans,
                data=.tmp))
  }
  plt <- rsp_plot_match(ls, ..., output="plot")
  ls$plot <- plt
  .rsp_function_output(ls, output)
}








