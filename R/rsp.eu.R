#' @name rsp.eu
#' @title Quick access to common SPECIAEUROPE subsets.
#' @aliases rsp_eu rsp_eu_pm10 rsp_eu_pm2.5

#' @description \code{\link{rsp_eu}} and \code{rsp_eu_} functions are
#' quick access wrappers to commonly requested SPECIEUROPE subsets.

#' @return \code{rsp_eu} and \code{rsp_eu_}functions typically return
#' a \code{respeciate} \code{data.frame} of the requested profiles:
#'
#' \code{rsp_eu()} returns all profiles in the local version of
#' \code{\link{SPECIEUROPE}}
#'
#' \code{rsp_eu_pm10} returns all SPECIEUROPE profiles classified as
#' PM10 (using \code{Particle.Size=="PM10"}), \code{rsp_eu_pm10} for PM2.5
#' and so on...
#'
#' @seealso \code{\link{SPECIEUROPE}}


#############################
#NOTES
############################

# might not be keeping these


###########################
# functions
###########################

# work in progress

# get all of SPEIEUROPE
#     not a big job at moment...
#          < 300 profiles

#' @rdname rsp.eu
#' @export

rsp_eu <- function(){
  rsp(rsp_find_profile(source="eu"))
}

#subsets

# SPECIEUROPE Particle.Size
# "PM10"  "TSP"   "PM2.5" "PMraw" "PM50"  "PM1"   "PM"
#      currently only doing pm10 and pm2,5

#' @rdname rsp.eu
#' @export

rsp_eu_pm10 <- function(){
  rsp(rsp_find_profile("pm10", by="particle.size",
                       source="eu", partial=FALSE))
}

#' @rdname rsp.eu
#' @export

rsp_eu_pm2.5 <- function(){
  rsp(rsp_find_profile("pm2.5", by="particle.size",
                       source="eu", partial=FALSE))
}


