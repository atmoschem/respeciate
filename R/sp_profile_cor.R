#' @name sp.profile.cor
#' @title sp_profile correlation
#' @aliases sp_profile_cor

#' @description sp_profile functions for studying relationships between
#' multi-profile (re)SPECIATE data sets

#' @description \code{\link{sp_profile_cor}} generates a correlation matrix
#' for the supplied (re)SPECIATE data sets.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param min.n Numeric (default 3), the minimum number of species measurements
#' needed in a profile for the function to use it in correlation caluclations.
#' Here, it should be noted that this does not guarantee at the three matched
#' pairs of measurements needed to calculate a correlation coefficient because
#' not all profiles contain all species, so there may still be insufficient
#' overlap on a case-by-case basis.
#' @param output Character vector, required function output: \code{'report'} the
#' calculated correlation matrix; \code{'plot'} a heat map of that correlation
#' matrix.
#' @return Depending on the \code{output} option, \code{sp_profile_cor} returns
#' one or more of the following: the correlation matrix, a heat map of the
#' correlation matrix.

#NOTE

#' @rdname sp.profile.cor
#' @export

#  using data.table for dcast

######################
#cor
#generate correlation matrix
######################

#in development

#to do
#########################

#speed up the correlation calculation
#   currently painful using for loop
#   can't use stats::cor on whole data set with min.n option
#   but there should be a way to make this a lot faster and retain that...

#formals for cor calculation control
#   set use, etc???

#formals for plot control
#   na colour
#   colours for correlation coefficients

#colour key
#   currently using legend but it looks a little crude


#aa <- sp_profile(sp_find_profile("ae8", by="profile_type"))
#sp_profile_cor(aa)

sp_profile_cor <- function(x, min.n = 3,
                           output = c("plot", "report")){
  #if ref missing
  .x <- sp_profile_dcast(x)

  #no point doing any have less than min.n values?
  .test <- apply(.x, 2, function(x) length(.x[!is.na(x)]))
  .x <- .x[.test > min.n]

  #any point doing any with only 1 unique value??

  #############################
  #calculate correlations
  #############################

  #Could not stop bad cors using...
  #.cor <- cor(.x[-1:-2], use="pairwise.complete.obs")

  #sd = 0 warnings?
  #   maybe cases where 1 unique case, e.g. x=c(1,1,1,1,1), y=c(2,2,2,2)
  #   or where overlap is low <2 x=c(1,1,1,NA,NA), y=c(NA,NA,1,NA,NA)

  #pokemon cludge...
  f <- function(x, y) {
    test <- !is.na(x) & !is.na(y)
    x <- x[test]
    y <- y[test]
    if(length(x) >= min.n){
      if(length(unique(x))==1 || length(unique(y))==1){
        NA
      } else {
        cor(x,y, use="pairwise.complete.obs")
      }
    } else { NA }
  }

  .X <- .x[-1:-2]
  .cor <- as.data.frame(matrix(NA, nrow=ncol(.X), ncol=ncol(.X)))
  for(i in 1:ncol(.cor)) {
    for(j in 1:ncol(.cor)) {
      .cor[i, j] <- f(.X[, i], .X[, j]) # apply f() to each combination
    }
  }
  row.names(.cor) <- colnames(.cor) <- colnames(.X)

  #bit of cheat to make a plot with base r graphics
  .tmp <- .cor
  .tmp[is.na(.tmp)] <- -2
  cols <- c(rep("#CFCFCF", 50), cm.colors(100))

  #output
  ####################
  #currently aiming for
  #   report (invisible), plot or both
  #   report class data.frame

  #NB: plot is heatmap without the dendrograms
  #stackover suggests it is hard going modifying
  #https://stackoverflow.com/questions/29893630/r-draw-heatmap-with-clusters-but-hide-dendrogram

  if("plot" %in% output){
    heatmap(as.matrix(.tmp), Rowv = NA, Colv = NA,
            col =cols,
            scale="none")
    legend(x="topleft",
           y.intersp = 0.5,
           border = NA,
           legend=c(" 1", "", " 0", "", "-1", "", "na"),
           fill=cols[c(150, 125, 100, 75, 51, NA, 1)])
  }
  if("report" %in% output){
    return(invisible(.cor))
  }
}


