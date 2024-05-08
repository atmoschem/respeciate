#' @name rsp.q
#' @title rsp_q_ provide quick access to common re(SPECIATE) sub-samples
#' @aliases rsp_q rsp_q_gas rsp_q_other rsp_q_pm rsp_q_pm.ae6 rsp_q_pm.ae8
#' rsp_q_pm.cr1 rsp_q_pm.simplified

#' @description \code{rsp_q_} functions are quick access wrappers to commonly
#' requested re(SPECIATE) sub-samples.
#' @return \code{rsp_q_} functions typically return a \code{respeciate}
#' \code{data.frame} of the requested profiles.
#'
#' For example:
#'
#' \code{rsp_q_gas()} returns all gaseous profiles in re(SPECIATE)
#' (\code{PROFILE_TYPE == 'GAS'}).
#'
#' \code{rsp_q_pm} returns all particulate matter (PM) profiles in re(SPECIATE)
#' not classified as a special PM type (\code{PROFILE_TYPE == 'PM'}).
#'
#' The special PM types are subsets profiles intended for special
#' applications, and these include \code{rsp_q_pm.ae6} (type \code{PM-AE6}),
#' \code{rsp_q_pm.ae8} (type \code{PM-AE8}), \code{rsp_q_pm.cr1} (type
#' \code{PM-CR1}), and \code{rsp_q_pm.simplified} (type \code{PM-Simplified}).
#'
#' \code{rsp_q_other} returns all profiles classified as other in re(SPECIATE)
#' (\code{PROFILE_TYPE == 'OTHER'}).
#'


#############################
#NOTES
############################

# might not be keeping these

# should be a quicker way of doing this...
#    maybe try going sysdata directly instead of using rsp_profile_info???
#       BUT might not be much a speed saving...

#profile types
#GAS, OTHER, PM, PM-AE6        PM-AE8        PM-CR1 PM-Simplified

# any others worth doing???

#' @rdname rsp.q
#' @export

rsp_q_gas <- function(){
  rsp_profile(rsp_profile_info("gas", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.q
#' @export

rsp_q_other <- function(){
  rsp_profile(rsp_profile_info("other", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.q
#' @export

rsp_q_pm <- function(){
  rsp_profile(rsp_profile_info("pm", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.q
#' @export

rsp_q_pm.ae6 <- function(){
  rsp_profile(rsp_profile_info("pm-ae6", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.q
#' @export

rsp_q_pm.ae8 <- function(){
  rsp_profile(rsp_profile_info("pm-ae8", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.q
#' @export

rsp_q_pm.cr1 <- function(){
  rsp_profile(rsp_profile_info("pm-cr1", by = "profile_type", partial=FALSE))
}

#' @rdname rsp.q
#' @export

rsp_q_pm.simplified <- function(){
  rsp_profile(rsp_profile_info("pm-simplified", by = "profile_type", partial=FALSE))
}




