#' @name sp.reshape
#' @title (re)SPECIATE profile reshaping functions
#' @aliases sp_dcast sp_dcast_profile sp_dcast_species sp_melt_wide

#' @description Functions for reshaping (re)SPECIATE profiles

#' @description \code{sp_dcast} and \code{sp_melt_wide} reshape supplied
#' (re)SPECIATE profile(s). \code{sp_dcast} converts these from their supplied
#' long form to a widened form, \code{dcast}ing the data set by either species
#' or profiles depending on the \code{widen} setting applied.
#' \code{sp_dcast_profile} and \code{sp_dcast_species} are wrappers for these
#' options. \code{sp_melt_wide} attempts to return a previously widened data
#' set to the original long form.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles in standard long form or widened form for
#' \code{\link{sp_dcast}} and \code{\link{sp_melt_wide}}, respectively.
#' @param widen character, when widening \code{x} with
#' \code{\link{sp_dcast}}, the data type to \code{dcast},
#' currently \code{'species'} (default) or \code{'profile'}. See Note.
#' @param pad logical, when \code{melt}ing a previously widened data set,
#' should \code{x} be re-populated with species and/or profile meta-data,
#' discarded when widening.
#' @return \code{sp_dcast} returns the wide form of the supplied
#' \code{respeciate} profile. \code{sp_melt_wide}
#' returns the (standard) long form of a previously widened profile.

#' @note Conventional long-to-wide reshaping of data, or \code{dcast}ing, can
#' be slow and memory inefficient. So, \code{respeciate} uses the
#' \code{\link[data.table:dcast]{data.table::dcast}}
#' method. The \code{sp_dcast_species} method,
#' applied using \code{wide='species'}, is effectively:
#'
#' \code{dcast(..., PROFILE_CODE+PROFILE_NAME~SPECIES_NAME, value.var="WEIGHT_PERCENT")}
#'
#' And, the alternative \code{wide='profile'}:
#'
#' \code{dcast(..., SPECIES_ID+SPECIES_NAME~PROFILE_CODE, value.var="WEIGHT_PERCENT")}
#'
#' Although, \code{respeciate} uses a local version of \code{WEIGHT_PERCENT} called
#' \code{.value}, so the EPA source information can easily be recovered. See also
#' \code{\link{sp_rescale_profile}}.
#'
#' @references
#'   Dowle M, Srinivasan A (2023). _data.table: Extension of `data.frame`_.
#'   R package version 1.14.8, <https://CRAN.R-project.org/package=data.table>.

#NOTE

#' @rdname sp.reshape
#' @export

##   now imports from xxx.r
##   #' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

#in development sp_melt_wide below

#maybe think about padding sp_dcast_profile

######################
#dcast
#long_to_wide reshape
######################

sp_dcast <- function(x, widen = "species"){

  ####################
  #see ?data.table::dcast for examples
  ####################

  # currently running with any x
  # but likely errors out with anything but a respeciate object...

  #note: should this handle non-respeciate objects?
  #      maybe not but with a force option to (at own risk) override??

  #note: thinking about adding formal to set the wide term
  #      so user can set the dcast term, e.g. ~ species or profile
  #      name key? wide? variable.name?

  #adds .value if missing
  ## using .value rather the WEIGHT_PERCENT in case rescaled
  x <- rsp_tidy_profile(x)

  #save class
  cls <- class(x)

  xx <- as.data.table(x)

  #stop if wide option not known.
  if(!widen %in% c("species", "profile")){
    stop("unknown wide option")
  }
  if(widen=="species"){
    out <- dcast(xx,
                 PROFILE_CODE + PROFILE_NAME ~ SPECIES_NAME,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }
  if(widen=="profile"){
    out <- dcast(xx,
                 SPECIES_ID + SPECIES_NAME ~PROFILE_CODE,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }

  #maybe use species_id in species dcast??
  ########################################

  #then add matched species_names, making this unique??
  #   ~species_name makes labeling easier but will be
  #       merging any duplicated species_names...
  #   could also add back in other profile info but not species info...
  #       which means we will loose that it you melt(dcast(...))

  #output
  #   just outputting as data.frame at moment
  #       not sure if this can be respeciate and
  #          don't want a respeciate wide class
  #   could use an output arg??? as.is, data.frame, etc...

  out <- as.data.frame(out)
  #class(out) <- cls
  out
}

#' @rdname sp.reshape
#' @export

sp_dcast_profile <- function(x, widen = "profile"){
  sp_dcast(x=x, widen=widen)
}



#' @rdname sp.reshape
#' @export

sp_dcast_species <- function(x, widen = "species"){
  sp_dcast(x=x, widen=widen)
}




#' @rdname sp.reshape
#' @export

##   now imports from xxx.r
##   #' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

# wanted melt to go with dcast
#    curently just to reverse the dcast action
#    see e.g.
#    https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html

#padding needs work
#

######################
#melt
#wide_to_long reshape
######################

sp_melt_wide <- function(x, pad = TRUE){

  ####################
  #see ?data.table::melt for examples
  ####################

  #adds .value if missing
  ## using .value rather the WEIGHT_PERCENT in case rescaled
  x <- rsp_tidy_profile(x)

  #save class
  cls <- class(x)

  xx <- as.data.table(x)

  #################
  #test log/wide
  ####################
  #should be able to simplify this a lot
  .test <- c("PROFILE_NAME", "PROFILE_CODE", "SPECIES_ID", "SPECIES_NAME",
             "WEIGHT_PERCENT", ".value")
  .test <- .test[.test %in% names(xx)]
  if(length(.test)>2){
    stop("sp_melt_wide halted; x already looks like a long profile.", call.=FALSE)
  }
  .test.sp <- length(grep("PROFILE", .test))
  .test.pr <- length(grep("SPECIES", .test))
  if(.test.pr>0 & .test.sp>0){
    stop("sp_melt_wide halted; x already looks suspect.", call.=FALSE)
  }
  .long <- "bad"
  if(.test.pr>0 & length(.test)==.test.pr){
    .id.vars <- .test
    .long <- "PROFILE_CODE"
  }
  if(.test.sp>0 & length(.test)==.test.sp){
    .id.vars <- .test
    .long <- "SPECIES_NAME"
  }
  if(.long=="bad"){
    stop("sp_melt_wide halted; x already looks suspect.", call.=FALSE)
  }

  #should only be species.wide or profile.wide
  #   if we get to here

  out <- melt(xx, id.vars = .id.vars)
  names(out)[names(out)=="variable"] <- .long
  names(out)[names(out)=="value"] <- "WEIGHT_PERCENT"
  out$.value <- out$WEIGHT_PERCENT

  #merge if padding
  #####################
  #might not be best way of doing it
  #maybe a 'pad what is missing'
  #
  if(pad){
    PROFILES <- as.data.table(sysdata$PROFILES)
    SPECIES_PROPERTIES <- as.data.table(sysdata$SPECIES_PROPERTIES)
    if(.long=="PROFILE_CODE"){
      out <- merge(out, PROFILES, by = .long, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
      .tmp <- intersect(names(out), names(SPECIES_PROPERTIES))
      out <- merge(out, SPECIES_PROPERTIES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    } else {
      #.long must be "SPECIES_NAME"
      out <- merge(out, SPECIES_PROPERTIES, by = .long, all.y=FALSE,
                  all.x=TRUE, allow.cartesian=TRUE)
      .tmp <- intersect(names(out), names(PROFILES))
      out <- merge(out, PROFILES, by = .tmp, all.y=FALSE,
                   all.x=TRUE, allow.cartesian=TRUE)
    }
  }

  out <- as.data.frame(out)
  #class(out) <- cls
  out
}


########################
#unexported, old code and text notes
#######################

#test data 219 records
#aa <- sp_profile(sp_find_profile("ae6", by="profile_type"))

#idiot-test reference.

#I don't think I am using it anywhere...

#########################
#testing getting rid of this
#########################

#rsp_build_wide_profile <- function(x){
#  .usp <- unique(x$SPECIES_NAME)
#  .upr <- unique(x$PROFILE_CODE)
#  ref <- data.frame(t(rep(NA, length(.usp))))
#  names(ref) <- .usp
#  ans <- lapply(.upr, function(.pr){
#    temp <- x[x$PROFILE_CODE==.pr,]
#    out <- data.frame(t(temp$WEIGHT_PERCENT))
#    names(out) <- temp$SPECIES_NAME
#    out<- modifyList(ref, out)
#    out$PROFILE_CODE <- .pr
#    out$PROFILE_NAME <- temp$PROFILE_NAME[1]
#    out
#    #ref
#  })
#  do.call(rbind, ans)
#}

#require(dplyr)

#aa %>% group_by(PROFILE_CODE, SPECIES_ID) %>%
#   summarise(PROFILE_NAME = PROFILE_NAME[1],
#             SPECIES_NAME = SPECIES_NAME[1],
#             SPEC_MW = SPEC_MW[1],
#             total = sum(WEIGHT_PERCENT, na.rm=T),
#             mean = mean(WEIGHT_PERCENT, na.rm=T),
#             sd = sd(WEIGHT_PERCENT, na.rm=T),
#             n = length(WEIGHT_PERCENT[!is.na(WEIGHT_PERCENT)]))

#require(data.table)

#test_wide <- function(x){
  ##as above
  #####################
  ##see ?data.table::dcast for examples
  #####################
  #xx <- as.data.table(x)
  #out <- dcast(xx,
  #             PROFILE_CODE + PROFILE_NAME ~SPECIES_NAME,
  #             mean,
  #             na.rm=TRUE,
  #             value.var = "WEIGHT_PERCENT")
  ##maybe use species id in dcast
  ##then add matched species names, making unique??
  ##you can keep profile but not species info...
  #as.data.frame(out)
#}

#plot(test_wide(aa)$Iron, rsp_build_wide_profile(aa)$Iron)
## ~ 2 seconds

#aa <- aa <- sp_profile(find_sp_profile("gas", by="profile_type"))
## 2641 profiles
## (full archive 6855)
#plot(test_wide(aa)$Formaldehyde)
## much faster than
#plot(rsp_build_wide_profile(aa)$Formaldehyde)
#plot(test_wide(aa)$Formaldehyde, rsp_build_wide_profile(aa)$Formaldehyde)
##straight y=x
#
