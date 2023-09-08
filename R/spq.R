#' @name spq
#' @title spq_ quick access to common re(SPECIATE) sub-samples
#' @aliases spq_gas spq_other spq_pm spq_pm.ae6 spq_pm.ae8 spq_pm.cr1
#' spq_pm.simplified

#' @description \code{spq_} functions are quick access wrappers to commonly
#' requested re(SPECIATE) sub-samples.
#' @return \code{spq_} functions typically return a \code{respeciate}
#' \code{data.frame} of the requested profiles.
#'
#' For example:
#'
#' \code{sqr_gas} returns all gaseous profiles (\code{PROFILE_TYPE == 'GAS'}).
#'
#' \code{sqr_pm} returns all particulate matter (PM) profiles not classified
#' as a special PM type (\code{PROFILE_TYPE == 'PM'}).
#'
#' The special PM types are subsets of PM profiles intended for special
#' applications, and these include \code{sqr_pm.ae6} (type \code{PM-AE6}),
#' \code{sqr_pm.ae8} (type \code{PM-AE8}), \code{sqr_pm.cr1} (type
#' \code{PM-CR1}), \code{sqr_pm.simplified} (type \code{PM-Simplified})
#' ans \code{sqr_other} (\code{PROFILE_TYPE == 'OTHER'}).
#'


#############################
#NOTE
############################

#might not be keeping these

#profile types
#GAS, OTHER, PM, PM-AE6        PM-AE8        PM-CR1 PM-Simplified

#spq_gas,


#' @rdname spq
#' @export

spq_gas <- function(){
  sp_profile(sp_profile_info("gas", by = "profile_type", partial=FALSE))
}

#' @rdname spq
#' @export

spq_other <- function(){
  sp_profile(sp_profile_info("other", by = "profile_type", partial=FALSE))
}

#' @rdname spq
#' @export

spq_pm <- function(){
  sp_profile(sp_profile_info("pm", by = "profile_type", partial=FALSE))
}

#' @rdname spq
#' @export

spq_pm.ae6 <- function(){
  sp_profile(sp_profile_info("pm-ae6", by = "profile_type", partial=FALSE))
}

#' @rdname spq
#' @export

spq_pm.ae8 <- function(){
  sp_profile(sp_profile_info("pm-ae8", by = "profile_type", partial=FALSE))
}

#' @rdname spq
#' @export

spq_pm.cr1 <- function(){
  sp_profile(sp_profile_info("pm-cr1", by = "profile_type", partial=FALSE))
}

#' @rdname spq
#' @export

spq_pm.simplified <- function(){
  sp_profile(sp_profile_info("pm-simplified", by = "profile_type", partial=FALSE))
}




