#' @name sp.cor
#' @title (re)SPECIATE Species Correlations
#' @aliases sp_species_cor

#' @description sp_species functions for studying relationships between
#' species in multi-profile (re)SPECIATE data sets.

#' @description \code{\link{sp_species_cor}} generates a by-species correlation
#' matrix of the supplied (re)SPECIATE data sets.
#' @param x \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param min.n \code{numeric} (default 3), the minimum number of species measurements
#' needed in a profile for the function to use it in correlation calculations.
#' Here, it should be noted that this does not guarantee the three matched
#' pairs of measurements needed to calculate a correlation coefficient because
#' not all profiles contain all species, so there may still be insufficient
#' overlap on a case-by-case basis.
#' @param cols a series of \code{numeric}, \code{character} or other class values
#' that can be translated into a color gradient, used to color valid cases when
#' generating plots and color keys, default \code{c("#80FFFF", "#FFFFFF", "#FF80FF")}
#' equivalent to \code{\link{cm.colors}} output.
#' @param na.col \code{numeric}, \code{character} or other class that can be
#' translated into a single color, used to color \code{NA}s when generating
#' plots and color keys, default grey \code{"#CFCFCF"}.
#' @param heatmap \code{logical} or \code{list}, heat map settings. Options
#' include \code{TRUE} (default) to generate the heat map without modification;
#' \code{FALSE} to not plot it; or a list of heat map options to alter the plot
#' appearance.
#' @param key \code{logical} or \code{list}, color key settings if plotting
#' the correlation matrix heat map. Options include \code{TRUE} (default) to
#' generate the key without modification; \code{FALSE} to not include the key;
#' or a list of options to alter the key appearance.
#' @param report \code{logical} or \code{character}, the required function
#' output. Options include: \code{'silent'} (default), to return the
#' correlation matrix invisibly; \code{TRUE} to return the matrix
#' (visibly); and, \code{FALSE} to not return it.
#' @return By default \code{sp_species_cor} invisibly returns the calculated
#' correlation matrix a plots it as a heat map, but arguments including
#' \code{heatmap} and \code{report} can be used to modify function outputs.


#NOTE

# list options to do for heatmap and key!!!

#' @rdname sp.cor
#' @export

#  using data.table for dcast

######################
#cor
#generate correlation matrix
######################

########################
#in development
#######################

#to think about
#########################

#speed up the correlation calculation
#   currently painful using for loop
#      can't use stats::cor on whole data set and include min.n option
#      but there should be a way to make this a lot faster and retain that...

#formals for cor calculation control
#   include cor:use, etc???


#plot handling could be tidier
#   at moment plot handling is in an if/else
#       with/without NAs handled differently
#       but think this could be passed down into key and plot
#            holding for now until key is in other plots
#            and have an idea how it'll work
#                (see also key)

#col key
#   currently using local function rsp_col_key
#      could export or more to another package but would added to depends

#plot and col key fine control
#   suspecting will need better fine control of both
#   re plot
#      font size, heatplot options
#   re col key
#      font size, scale position, style/type, range
#         currently holding (see plot handling and col key)

#starting thinking about above
#   see output control below

#output control
#    currently uses lots of settings:
#       heatmap TRUE/FALSE to plot heatmap of correlation matrix
#           or list of heatmap plot settings
#       key TRUE/FALSE to add a col key to the heatmap
#           or list of col key plot settings
#       report "silent" to return the correlation matrix invisibly,
#           or TRUE/FALSE to return it visibly or not at all.
#    The list options are not yet done...


#aa <- sp_profile(sp_find_profile("ae8", by="profile_type"))
#sp_species_cor(aa)

sp_species_cor <- function(x, min.n = 3,
                           cols = c("#80FFFF", "#FFFFFF", "#FF80FF"),
                           na.col = "#CFCFCF", heatmap = TRUE,
                           key = TRUE, report = "silent"){
  #if ref missing
  .x <- sp_dcast(x, wide="species")

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

  #I'm calling this the pokemon cludge...
  #got to catch 'em all...
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

  #cheats to test plot output
  #base r graphics is getting painful

  #.cor[.cor>0.5 & .cor<1] <- 0.5
  #.cor[is.na(.cor)] <- -0.5
  #.cor <- .cor/2

  #output
  ####################
  #currently aiming for
  #   report (invisible), plot or both
  #   report class data.frame
  #   output plot only does not mean anything...

  #NB: plot is heatmap without the dendrograms
  #stackover suggests it is hard going modifying
  #https://stackoverflow.com/questions/29893630/r-draw-heatmap-with-clusters-but-hide-dendrogram

  if((is.logical(heatmap) && heatmap) | (is.list(heatmap))){
    .cols.max <- 300
    if(max(.cor, na.rm=TRUE) < 1){
      .z <- (1 - max(.cor, na.rm=TRUE))*100
      .cols.max <- .cols.max - .z
    }
    .tmp <- .cor
    .tmp[is.na(.tmp)] <- -2
    cols <- c(rep(na.col, 100), colorRampPalette(cols)(200))
    if(any(is.na(.cor))){
      #plot when
      #.cor includes nas
      heatmap(as.matrix(.tmp), Rowv = NA, Colv = NA,
              #cexRow = 0.5, cexCol = 0.5, #axis size
              col =cols[1:.cols.max],
              scale="none")

      if((is.logical(key) && key) | (is.list(key))){
        rsp_col_key(c(-1,1), cols=cols[101:300], #type=1,
                    ticks= -1:1, y=0.75,
                    na.col = na.col)
      }
    } else {
      #plot when
      #.cor has no NAs
      .cols.min <- 101
      if(min(.cor, na.rm=TRUE) < 1){
        .z <- (1 + min(.cor, na.rm=TRUE))*100
        .cols.min <- .cols.min + .z
        print(.cols.min)
      }
      heatmap(as.matrix(.tmp), Rowv = NA, Colv = NA,
              #cexRow = 0.5, cexCol = 0.5, #axis size
              col =cols[.cols.min:.cols.max],
              scale="none")
      if((is.logical(key) && key) | (is.list(key))){
        rsp_col_key(c(-1,1), cols=cols[101:300], #type=1,
                    ticks= -1:1, y=0.75,
                    na.col = NA)
      }
    }
  }

  #report handling
  if((is.logical(report) && report) | (is.character(report))){
    if(is.character(report) && report=="silent"){
      return(invisible(.cor))
    } else {
      return(.cor)
    }
  } else {
    return(invisible(NULL))
  }

}


