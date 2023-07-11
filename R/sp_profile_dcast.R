#' @name sp.profile.dcast
#' @title sp_profile reshaping functions
#' @aliases sp_profile_dcast

#' @description sp_profile functions for reshaping (re)SPECIATE profiles

#' @description \code{\link{sp_profile_dcast}} reshapes the supplied
#' (re)SPECIATE profile(s), converting it from long to short form. See Notes
#' for further details.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param wide character, when reshaping \code{x} the data type to 'widen',
#' currently \code{'species'} (default) or \code{'profile'}. See Note.
#' @return \code{sp_profile_dcast} returns the wide form of the supplied
#' \code{respeciate} profile.

#' @note Conventional long-to-wide reshaping of data, or \code{dcast}ing, can
#' be slow and memory inefficient. So, \code{respeciate} uses the
#' \code{\link{data.table::dcast}} method. The default method,
#' applied using \code{wide='species'}, is effectively:
#'
#' \code{dcast(..., PROFILE_CODE+PROFILE_NAME~SPECIES_NAME, value.var="WEIGHT_PERCENT")}
#'
#' And, the alternative \code{wide='profile'}:
#'
#' \code{dcast(..., SPECIES_ID + SPECIES_NAME ~PROFILE_CODE, value.var="WEIGHT_PERCENT")}
#'
#' Although, \code{respeciate} uses a local version of \code{WEIGHT_PERCENT} called
#' \code{.value}, so the EPA source information can easily be recovered. See also
#' \code{\link{sp_profile_rescale}}
#'
#' @references
#'   Dowle M, Srinivasan A (2023). _data.table: Extension of `data.frame`_.
#'   R package version 1.14.8, <https://CRAN.R-project.org/package=data.table>.

#NOTE

#' @rdname sp.profile.dcast
#' @export
#' @import data.table

# may need to set data.table specifically??
#      data.table::as.data.table, etc??

######################
#dcast
#long_to_wide reshape
######################

sp_profile_dcast <- function(x, wide = "species"){

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

  if(!wide %in% c("species", "profile")){
    stop("unknown wide option")
  }
  if(wide=="species"){
    out <- dcast(xx,
                 PROFILE_CODE + PROFILE_NAME ~ SPECIES_NAME,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }
  if(wide=="profile"){
    out <- dcast(xx,
                 SPECIES_ID + SPECIES_NAME ~PROFILE_CODE,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }


  #maybe use species_id in species dcast??

  #then add matched species_names, making this unique??
  #   ~species_name makes labeling easier but will be
  #       merging any duplicated species_names...
  #   could also add back in other profile info but not species info...
  #       which means we will loose that it you melt(dcast(...))

  #output
  #   just outputting as data.frame at moment
  #       not sure if this can be respeciate and
  #          don't want a respeciate wide class
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

rsp_build_wide_profile <- function(x){
  .usp <- unique(x$SPECIES_NAME)
  .upr <- unique(x$PROFILE_CODE)
  ref <- data.frame(t(rep(NA, length(.usp))))
  names(ref) <- .usp
  ans <- lapply(.upr, function(.pr){
    temp <- x[x$PROFILE_CODE==.pr,]
    out <- data.frame(t(temp$WEIGHT_PERCENT))
    names(out) <- temp$SPECIES_NAME
    out<- modifyList(ref, out)
    out$PROFILE_CODE <- .pr
    out$PROFILE_NAME <- temp$PROFILE_NAME[1]
    out
    #ref
  })
  do.call(rbind, ans)
}

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
