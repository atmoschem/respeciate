#' @name rsp.combine
#' @title combining respeciate profiles
#' @aliases rsp_lbind

#' @description Functions to combining respeciate data sets.

#' @description \code{rsp_lbind} binds two or more \code{respeciate}-like
#' objects. The default option is to stack the supplied data sets (e.g.
#' \code{respeciate}, \code{data.frame}, etc) like \code{rbindlist}
#' in \code{data.table} (or row_bind in dplyr). This matches columns by name
#' before stacking the supplied data sets.

#' @param ...  (various) This function is intended to be quite flexible. All
#' supplied arguments are tested and handled as follows: \code{respeciate}-like
#' objects are passed to \code{data.table::rbindlist} as a list to rbind using
#' \code{data.table} methods; Any other arguments that are valid
#' \code{rbindlist} arguments are passed on 'as is'; And, anything else is
#' (hopefully) ignored.

#' @return \code{rsp_lbind} attempts to return a single stacked version of the
#' supplied data sets. If it is successful, the (stacked) data set is typically
#' returned as a \code{respeciate} object or a \code{data.frame} with a warning
#' if it is missing columns \code{respeciate} expects.

## #' @note
## TO DO: want a statement here about why we are not going with a rbind method
## basically, it is a pain...
##     ... if any of the data sets is a data.frame, and
##          you get rbind.data.frame/default
##     is some online discussion of its behaviour

## #' @seealso [link to data.table::rbindlist if we can do that...]
## not sure I am loading package

#' @references
#'   Dowle M, Srinivasan A (2023). data.table: Extension of `data.frame`.
#'   R package version 1.14.8, \url{https://CRAN.R-project.org/package=data.table}.

##########################################
# general NOTES
##########################################

# not loading all of data.table, see xxx.r...

######################
#rsp_lbind
######################

#' @rdname rsp.combine
#' @export

#issues
############################

# not finished

# bits of documentation to complete
#    links to data.table::rbindlist need to be added
#         in text and see also
# error messages need tidying
# currently not handling the class quite right
#       ok for standard respeciate object,
#             probably not for rsp_x
#             definitely not for wides...
#             stack, but probably breaks respeciate rules for
#                  data.frame, etc without expected respeciate names .... ?
#       (see notes in function)

#think about...
#############################

# might need option to force object class
# option(s) to make errors/warning silent ????
# could we extend lbind to do cbind as well as rbind ????
#       could we extend lbind to do merge as well ???

rsp_lbind <- function(...){

  # setup
  #####################################
  .args <- list(...)

  # ... handling 1: the data.sets
  #####################################
  # based on method in rsp() method v 0.3
  # allows data.frames, respeciate objects, etc
  .rsp <- lapply(.args, function(.rsp){
    if(is.data.frame(.rsp)){ .rsp }})

  #might want to think about respeciate handling ???
  #  if for example something is wide ???? or rsp_x ????

  # rbindlist
  ###################################
  # currently using loa-like approach...
  #    allowing only extra args formally declared in rbindlist
  .refs <- names(formals(data.table::rbindlist))
  .lbind <- modifyList(list(l=.rsp, fill=TRUE),
                       .args[names(.args) %in% .refs])
  .bound.dt <- do.call(data.table::rbindlist, .lbind)
  if(nrow(.bound.dt)==0){
    stop("RSP> Nothing to list and rbind...",
         call. = FALSE)
  }

  # output
  ###########################

  # should this try to force class to that of first data set in call ???
  #     might make most sense ???

  .rsp <- try(as.respeciate(.bound.dt), silent=TRUE)
  if(class(.rsp)[1]=="try-error"){
    warning("RSP> not obvious respeciate; returning as data.frame...",
            call. = FALSE)
    as.data.frame(.bound.dt)
  } else {
    .rsp
  }
}



