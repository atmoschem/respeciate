#' @name rsp.reshape
#' @title Reshaping respeciate data sets
#' @aliases rsp_dcast rsp_dcast_profile rsp_dcast_species rsp_melt_wide

#' @description Functions for reshaping respeciate profiles

#' @description \code{rsp_dcast} and \code{rsp_melt_wide} reshape supplied
#' respeciate profile(s). \code{rsp_dcast} converts these from their supplied
#' long form to a widened form, \code{dcast}ing the data set by either species
#' or profiles depending on the \code{widen} setting applied.
#' \code{rsp_dcast_profile} and \code{rsp_dcast_species} are wrappers for these
#' options. \code{rsp_melt_wide} attempts to return a previously widened data
#' set to the original long form.
#' @param rsp A \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles in standard long form or widened form using
#' \code{\link{rsp_dcast}} and \code{\link{rsp_melt_wide}}, respectively.
#' @param widen character, when widening \code{rsp} with
#' \code{\link{rsp_dcast}}, the data type to \code{dcast},
#' currently \code{'species'} (default) or \code{'profile'}. See Note.
#' @param pad logical or character, when \code{melt}ing a previously widened
#' data set, should output be re-populated with species and/or profile
#' meta-data, discarded when widening. This is currently handled by
#' \code{\link{rsp_pad}}. The default \code{TRUE} applies standard settings,
#' so does not include profile sources reference meta-data. (See
#' \code{\link{rsp_pad}} for other options).
#' @param drop.nas logical, when \code{melt}ing a previously widened
#' data set, should output be stripped of any rows containing empty
#' weight/value columns. Because not all profile contains all species, the
#' \code{dcast}/\code{melt} process can generate empty rows, and this step
#' attempt account for that when working with standard re(SPECIATE)
#' profiles. It is, however, sometimes useful to check first, e.g. when
#' building profiles yourself.
#' @return \code{rsp_dcast} returns the wide form of the supplied
#' \code{respeciate} profile. \code{rsp_melt_wide}
#' returns the (standard) long form of a previously widened profile.

#' @note Conventional long-to-wide reshaping of data, or \code{dcast}ing, can
#' be slow and memory inefficient. So, \code{respeciate} uses the
#' \code{\link[data.table:dcast]{data.table::dcast}}
#' method. The \code{rsp_dcast_species} method,
#' applied using \code{widen='species'}, is effectively:
#'
#' \code{dcast(..., PROFILE_CODE+PROFILE_NAME~SPECIES_NAME, value.var="WEIGHT_PERCENT")}
#'
#' And, the alternative \code{widen='profile'}:
#'
#' \code{dcast(..., SPECIES_ID+SPECIES_NAME~PROFILE_CODE, value.var="WEIGHT_PERCENT")}
#'
#' Although, \code{respeciate} uses a local version of \code{WEIGHT_PERCENT} called
#' \code{.value}, so the EPA source information can easily be recovered. See also
#' \code{\link{rsp_rescale_profile}}.
#'
#' @references
#'   Dowle M, Srinivasan A (2023). _data.table: Extension of `data.frame`_.
#'   R package version 1.14.8, <https://CRAN.R-project.org/package=data.table>.

#NOTE

#############################
# these use
#############################
# .rsp_tidy_profile
# data.table::as.data.table
# data.table::dcast
# data.table::melt
# rsp_pad






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

#' @rdname rsp.reshape
#' @export

rsp_dcast <- function(rsp, widen = "species"){

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

  ######################
  # SPECIEUROPE data
  ######################
  if("rsp_eu" %in% class(rsp)){
    rsp <- .rsp_eu2us(rsp)
  }
  #######################

  #adds .value if missing
  ## using .value rather the WEIGHT_PERCENT in case rescaled
  x <- .rsp_tidy_profile(rsp)


  #save class
  cls <- class(x)

  xx <- data.table::as.data.table(x)

  #stop if widen option not known.
  if(!widen %in% c("species", "profile")){
    stop("unknown widen option")
  }
  if(widen=="species"){
    out <- data.table::dcast(xx,
                 PROFILE_CODE + PROFILE_NAME ~ SPECIES_NAME,
                 mean,
                 na.rm=TRUE,
                 value.var = ".value")
  }
  if(widen=="profile"){
    out <- data.table::dcast(xx,
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
  if(widen=="species"){
    class(out) <- c("rsp_sw", cls)
  } else {
    class(out) <- c("rsp_pw", cls)
  }
  out
}

######################################
# rsp_dcast(..., widen) shortcuts
######################################

#' @rdname rsp.reshape
#' @export

rsp_dcast_profile <- function(rsp, widen = "profile"){
  rsp_dcast(rsp=rsp, widen=widen)
}


#' @rdname rsp.reshape
#' @export

rsp_dcast_species <- function(rsp=rsp, widen = "species"){
  rsp_dcast(rsp=rsp, widen=widen)
}





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

#' @rdname rsp.reshape
#' @export

rsp_melt_wide <- function(rsp, pad = TRUE, drop.nas = TRUE){

  ####################
  #see ?data.table::melt for examples
  ####################

  #adds .value if missing
  ## using .value rather the WEIGHT_PERCENT in case rescaled
  x <- .rsp_tidy_profile(rsp)

  #save class
  cls <- class(x)

  xx <- data.table::as.data.table(x)

  #################
  #test log/wide
  ####################
  #should be able to simplify this a lot
  .test <- c("PROFILE_NAME", "PROFILE_CODE", "SPECIES_ID", "SPECIES_NAME",
             "WEIGHT_PERCENT", ".value")
  .test <- .test[.test %in% names(xx)]
  if(length(.test)>2){
    stop("RSP> melt halted; rsp already looks like a long profile.", call.=FALSE)
  }
  .test.sp <- length(grep("PROFILE", .test))
  .test.pr <- length(grep("SPECIES", .test))
  if(.test.pr>0 & .test.sp>0){
    stop("RSP> melt halted; rsp looks looks suspect.", call.=FALSE)
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
    stop("RSP> melt halted; rsp looks suspect.", call.=FALSE)
  }

  #should only be species.wide or profile.wide
  #   if we get to here

  out <- data.table::melt(xx, id.vars = .id.vars)
  names(out)[names(out)=="variable"] <- .long
  names(out)[names(out)=="value"] <- ".value"
  if("SPECIES_ID" %in% names(out)){
    out$SPECIES_ID <- as.character(out$SPECIES_ID)
  }
  if("SPECIES_NAME" %in% names(out)){
    out$SPECIES_NAME <- as.character(out$SPECIES_NAME)
  }
  if("PROFILE_CODE" %in% names(out)){
    out$PROFILE_CODE <- as.character(out$PROFILE_CODE)
  }

  #out$WEIGHT_PERCENT <- out$.value

  #merge if padding
  #####################
  #might not be best way of doing it
  #   could pass other args to pad
  #   might need to think about the .value/WEIGHT_PERCENT handling

  if(is.logical(pad) && pad){
    pad <- "standard"
  }
  if(is.character(pad)){
    out <- rsp_pad(out, pad, drop.nas)
    #tidy bad profile_name
    if(all(is.na(out$PROFILE_NAME)) && "PROFILE_CODE" %in% names(out)){
      out$PROFILE_NAME <- out$PROFILE_CODE
    }
    #tidy bad species_id
    if(all(is.na(out$SPECIES_ID)) && "SPECIES_NAME" %in% names(out)){
      out$SPECIES_ID <- as.character(-as.numeric(factor(out$SPECIES_NAME)))
    }

  }

  ################################
  # could tidy structure here??
  ################################

  # if weight_percent but not .value add .value
  # if.value but not weight_percent add .value
  # similar for profile_name/code and species_name/id

  # is that done in rsp_build_x ??


  #drop.nas...
  if(drop.nas){
    if(".value" %in% names(out)){
      out <- out[!is.na(out$.value),]
    } else {
      if("WEIGHT_PERCENT" %in% names(out)){
        out <- out[!is.na(out$WEIGHT_PERCENT),]
      }
        #do we want to warn if nothing to strip
        #if so, in else here??
    }
  }

  #output
  #need to rationalise outputs!!!
  #.rsp_build_respeciate(out)
  out <- as.data.frame(out)
  class(out) <- cls[!cls %in% c("rsp_pw", "rsp_sw")]
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
