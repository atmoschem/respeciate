##############################
#setup code, misc code,
#testing code, etc
##############################

#currently no hooks, etc...

#####################
#to check
#####################

# I think all build/check issues associate with
#     xxx_test and its depends...
#        (not keeping unless we can get it to work better)


utils::globalVariables(c("sysdata", ".SD", "ans", "control",
                         "PROFILE_CODE", "PROFILE_NAME", "PROFILE_TYPE",
                         "SPECIES_ID", "SPECIES_NAME",
                         "SPEC_MW", "WEIGHT_PERCENT", ".", ".value"))

########################
#to think about...
#######################

# all @import here
#    in case we have to move to data.table::as.data.table, etc...
#    moving to data.table::as.data.table...
# #' @import data.table

#   data.table used by:
#         rsp_test_profile,
#         sp_dcast_profile, and those that use dcast?
#               sp_species_cor
#               sp_profile_distance
#         and others???
#               need to identify them

#' @importFrom lattice xyplot barchart panel.grid panel.xyplot panel.barchart
#' trellis.par.get simpleTheme yscale.components.default prepanel.default.xyplot
#' @importFrom latticeExtra doubleYScale panel.ablineq
#' @importFrom data.table ":="
#' @importFrom stats sd cophenetic cor cutree dist hclust heatmap AIC
#' as.formula coefficients formula lm nls nls.control predict update
#' @importFrom utils modifyList head packageVersion
#' @importFrom graphics axis barplot par legend lines rect text abline
#' grid mtext plot.new plot.window points polygon title
#' @importFrom grDevices cm.colors colorRampPalette as.graphicsAnnot
#' dev.flush dev.hold heat.colors



#might be able to drop legend?
#   check plot.respeciate


##############################
#common unexported
##############################


#rsp_plot_fix
#########################
# general tidy function for data before plotting
#    merges duplicate species in profiles
#    makes profile names unique if duplicated
#    tidies species names for use in labelling

#used by
###################
#plot.respeciate
#sp_plot_profile

#uses
####################
#rsp_tidy_profile
#rsp_test_respeciate
#rsp_test_profile



rsp_plot_fix <- function(x, silent = FALSE, ...){

  .x.args <- list(...)
  x <- rsp_tidy_profile(x)
  ##test object type
  test <- rsp_test_respeciate(x, level=2, silent=TRUE)
  if(test != "respeciate"){
    if(test %in% c("respeciate.profile.ref", "respeciate.species.ref")){
      stop("RSP> No plot method for respeciate.reference files.",
           call. = FALSE)
    } else {
      stop("RSP> suspect respeciate object!",
           call. = FALSE)
    }
    #don't stop - respeciate profile
  }
  #check for duplicates
  x <- rsp_test_profile(x)
  if(any(x$.n>1) & !silent){
    warning(paste("RSP> found duplicate species in profiles (merged and averaged...)",
                  sep=""), call.=FALSE)
  }
  #shorten names for plotting
  x$SPECIES_NAME <- rsp_tidy_species_name(x$SPECIES_NAME)

  ####################################
  #issue profile names are not always unique
  ####################################
  test <- x
  test$SPECIES_ID <- ".default"
  test <- rsp_test_profile(test)
  ###################
  #rep_test
  #can now replace this with data.table version
  #BUT check naming conventions for .n
  ###################

  #does this need a warning?
  if(length(unique(test$PROFILE_NAME))<nrow(test)){
    if(!silent){
      warning(paste("RSP> found profiles with common names (making unique...)",
                    sep=""), call. = FALSE)
    }
    test$PROFILE_NAME <- make.unique(test$PROFILE_NAME)
    x <- x[names(x) != "PROFILE_NAME"]
    x <- merge(x, test[c("PROFILE_NAME", "PROFILE_CODE")], by="PROFILE_CODE")
    ############################
    #why not just
    #x$PROFILE_NAME <- make.unique(x$PROFILE_NAME)
    ############################
  }

  #out
  x
}




############################
#unexported function to
#test is x respeciate

##aa <- sp_profile(sp_find_profile("ae8", by="profile_type"))
##bb <- sp_find_profile("ethanol")
##bb <- sp_find_species("butane")

##rsp_test_respeciate(aa, level=1)
## level 1 TRUE for respeciate only (needs to look like respeciate)
## level likewise but TRUE for respeciate, respeciate.ref and respeciate.spcs

##might need to rethink this if I want to use it as is.respeciate method
##  it only allows 1 argument.

## could also test for .value

rsp_test_respeciate <- function(x, level = 1,
                                silent = FALSE){
  test <- class(x)
  out <- "bad"
  test <- test[1]=="respeciate"
  #order below matters
  #maybe tidy??
  if(all(c("SPECIES_NAME", "SPECIES_ID") %in% names(x))){
    out <- "respeciate.species.ref"
  }
  if(all(c("PROFILE_NAME", "PROFILE_CODE") %in% names(x))){
    out <- "respeciate.profile.ref"
  }
  if(all(c("SPECIES_NAME", "SPECIES_ID", "PROFILE_NAME", "PROFILE_CODE",
           "WEIGHT_PERCENT") %in% names(x))){
    out <- "respeciate"
  }
  if(!silent){
    if(test && out=="bad"){
      warning("suspect respeciate object!")
    }
    if(nrow(x)==0){
      warning("empty respeciate object!")
    }
  }
  if(level==1){
    return(test)
  }
  return(out)
}

#######################
#tidy profile

#now using a .value column as a local version of WEIGHT_PERCENT...
#then WEIGHT_PERCENT remains as EPA made it even if we rescale...

## testing this idea at the moment
##     make .value using rsp_tidy_profile
##     enabled in plot.respeciate, sp_profile_rescale, sp_profile_dcast
##                rsp_test_profile

rsp_tidy_profile <- function(x){
  #.value is local version of weight
  if(!".value" %in% names(x)){
    x$.value <- x$WEIGHT_PERCENT
  }
  x
}






###########################
#tidy species names

#currently not exported
#quick code to tidy species names

#currently used in plot.respeciate

#note: not fully tested

#thinking about

#    option foreshorten any names longer than [n] characters???
#    similar function to tidy profile names

rsp_tidy_species_name <- function(x){

  #attempts shorten names by remove other versions
  #names seem to be in format a (or b || c)
  #where (guessing) a is main name and
  #         b and c are alternative terms.

  #not fully tested,
  #   might still be more cases this dies on

  #gsub("[(].*","", x) failed if name a includes brackets
  #example:#"(2-methylpropyl)benzene (or isobutylbenzene)"

  #sub("[(][^(]or+$", "", x) failed if b or c includes brackets
  #also left space at end so needed sub("^\\s+|\\s+$", "", x)

  #sometimes it is "( or "
  x <- gsub(" [(] or ", " (or ", x)
  #next removes from last "(or" onwards
  x <- gsub("[(]or .*","", x)
  sub("^\\s+|\\s+$", "", x)
}


################################
#rsp_test_profile

#file:///C:/Users/trakradmin/Downloads/datatable.pdf
##rsp_test_profile(aa)

rsp_test_profile <- function(x){

  #set up .value if not there
  x <- rsp_tidy_profile(x)

  #######################################
  #track and return original class?
  #      testing
  #######################################
  tmp <- class(x)
  xx <- data.table::as.data.table(x)
  out <- xx[,
            .(PROFILE_NAME = PROFILE_NAME[1],
              SPECIES_NAME = SPECIES_NAME[1],
  #######################
  #test
  ########################
              #SPEC_MW = SPEC_MW[1],
  ########################
              .total = sum(.value, na.rm = TRUE),
              .value = mean(.value, na.rm = TRUE),
              .n = length(.value[!is.na(.value)]),
              .sd = sd(.value, na.rm = TRUE)
            ),
            by=.(PROFILE_CODE, SPECIES_ID)]

  #might regret .value
  out$WEIGHT_PERCENT <- out$.value
  #note: I *think* this is fine as long as x is never permanently replaced
  #      with the rsp_test_profile without a warning
  #           might be cases where we want to change WEIGHT_PERCENT

  #           change case making an average profile

  #output
  #might regret this...
  #   but would like to leave user free to use e.g. dplyr rather than
  #       data.table if that is their preference
  out <- as.data.frame(out)
  class(out) <- tmp
  out
}

# now replaced with rsp_test_profile

#rsp_test (old version)

#base case idiot test

#rsp_test <- function(x){
#  .prf <- unique(x$PROFILE_CODE)
#  ans <- lapply(.prf, function(y){
#    temp <- subset(x, PROFILE_CODE==y)
#    .spc <- unique(temp$SPECIES_ID)
#    ans <- lapply(.spc, function(z){
#      temp2 <- subset(temp, SPECIES_ID==z)
#      data.frame(PROFILE_CODE = y,
#                 PROFILE_NAME = temp2$PROFILE_NAME[1],
#                 SPECIES_ID = z,
#                 SPECIES_NAME = temp2$SPECIES_NAME[1],
#                 COUNT = length(temp2$WEIGHT_PERCENT[!is.na(temp2$WEIGHT_PERCENT)]),
#                 TOTAL = sum(temp2$WEIGHT_PERCENT[!is.na(temp2$WEIGHT_PERCENT)]),
#                 SPEC_MW = temp2$SPEC_MW[1],
#                 WEIGHT_PERCENT=mean(temp2$WEIGHT_PERCENT, na.rm=TRUE))
#    })
#    do.call(rbind, ans)
#  })
#  do.call(rbind, ans)
#}

#require(dplyr)
#test1 <- function(x){
#  x %>%
#    group_by(PROFILE_CODE, SPECIES_ID) %>%
#    summarise(PROFILE_NAME = PROFILE_NAME[1],
#              SPECIES_NAME = SPECIES_NAME[1],
#              SPEC_MW = SPEC_MW[1],
#              total = sum(WEIGHT_PERCENT, na.rm=T),
#              mean = mean(WEIGHT_PERCENT, na.rm=T),
#             sd = sd(WEIGHT_PERCENT, na.rm=T),
#             n = length(WEIGHT_PERCENT[!is.na(WEIGHT_PERCENT)]))
#}
#aa <- sp_profile(sp_find_profile("ae6", by="profile_type")$PROFILE_CODE)
#require(data.table)
#test2 <- function(x){
#  #######################################
#  #could track and return original class?
#  #######################################
#  xx <- as.data.table(x)
#  out <- xx[,
#            .(PROFILE_NAME = PROFILE_NAME[1],
#              SPECIES_NAME = SPECIES_NAME[1],
#              SPEC_MW = SPEC_MW[1],
#              .total = sum(WEIGHT_PERCENT, na.rm = TRUE),
#              WEIGHT_PERCENT = mean(WEIGHT_PERCENT, na.rm = TRUE),
#              .n = length(WEIGHT_PERCENT[!is.na(WEIGHT_PERCENT)]),
#              .sd = sd(WEIGHT_PERCENT, na.rm = TRUE)
#            ),
#            by=.(PROFILE_CODE, SPECIES_ID)]
#  #output
#  #currently data.frame
#  #could be respeciate
#  as.data.frame(out)
#}









#####################
#testing
#####################

#playing

#function(x, subset,...){
#  ans <- match.call()
#  ans <- as.character(ans)
#  return(ans)
#}

#ggplot example
#require(ggplot2)
#ggplot() + geom_col(aes(y=SPECIES_NAME, x=WEIGHT_PERCENT), data=aa) + facet_grid(.~PROFILE_NAME)


############################
#color key
############################

########################
#using this in:
########################

#sp_species_cor


#started with:
#https://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette

#removed dev.new

#see also regarding adding to existing plot
#https://www.statmethods.net/advgraphs/layout.html

# One figure in row 1 and two figures in row 2
# row 1 is 1/3 the height of row 2
# column 2 is 1/4 the width of the column 1
##attach(mtcars)
##layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
##       widths=c(3,1), heights=c(1,2))
##hist(wt)
##hist(mpg)
##hist(disp)

# Add boxplots to a scatterplot
##par(fig=c(0,0.8,0,0.8), new=TRUE)
##plot(mtcars$wt, mtcars$mpg, xlab="Car Weight",
##     ylab="Miles Per Gallon")
##par(fig=c(0,0.8,0.55,1), new=TRUE)
##boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
##par(fig=c(0.65,1,0,0.8),new=TRUE)
##boxplot(mtcars$mpg, axes=FALSE)
##mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)

# Function to plot color bar

#currently not exporting

#notes
#change min and max or .min and .max
#move title into plot?
#add key.style?
#add key.position?
#how to set plot range when na is included verus excluded
#    needs to be a function of min/max range not 0.5, 1.5, etc
#    maybe 0.25 ish??? maybe tick or half tick range????

#key is the data to be color-scaled or its range
#     currently just used for range

#cols is col.palette to apply to key range
#   the col ramp is messy... seems to need border
#        that could be issue if transparency using...

#x, y are key (proportional, 0 to 1) positions

#na.col color of na's
#na.cex scaling for na box width
#    do we need any other NA controls??
#          e.g. na.border if needed to be different to main border???

#axes to do
#    not using at moment
#        but would separate axes and border colours
#             not sure it is wanted/needed...

#ticks, nticks axes tick positions and number of ticks, respectively
#     think about these

#title still work in progress
#     not nice version on type=1


#have type = 1/2 for horizontal/vertical
#   think about types 3 and 4
#        horizontal with text at top
#        vertical with text on other side

#think about exporting this once sorted???
#   or passing to another package/better home
#       (not sure how that fits with package remit)



rsp_col_key <- function(key, cols, x, y = NULL,
                        ticks, nticks,
                        na.col = "grey", na.cex = 0.25,
                        title = "", axes, bg, border,
                        type = 2,
                        ...){

    #setup
    op <- par(no.readonly = TRUE)

    if(missing(x)){
      #currently just doing this option
      #like key.pos "top-left", key.style = 1 (horizontal, annotation below)
      x <- 0.1
    }
    if(is.null(y)){
      y <- 0.9
    }
    .min <- min(key, na.rm=TRUE)
    .max <- max(key, na.rm=TRUE)
    if(missing(ticks)){
      ticks <- pretty(c(.min, .max), 3)
    }
    if(missing(nticks)){
      nticks <- length(ticks)
    }
    .na.width <- na.cex * (.max-.min)
    if(missing(bg)){
      bg <- "white"
    }
    if(missing(border)){
      border <- "black"
    }
    scale <- (length(cols)-1)/(.max-.min)

#print(.max-.min)
#print(.na.width)
    #print(.min)
    #print(.max)

    #key.style 1
    if(type==1){
      #horizontal, header before, annotation after
      #margins
      .mai <- c(0.1,0.1,0.1,0.1)
      if(title ==""){
        #no title
        .fig <- c(x-0.1, x+0.1, y-0.04, y+0.1)
        .wdt <- 12
      } else {
        #title
        .fig <- c(x-0.1, x+0.1, y-0.12, y+0.1)
        .wdt <- 15
      }

      if(is.na(na.col)){
        .brd <- c(.na.width, .na.width)
      } else {
        .brd <- c(.na.width, .na.width*2)
      }

      #position col key
      par(fig = .fig, mai = .mai, new=TRUE)

      #plot col key

      #region
      plot(c(.min -.brd[2], .max+(.brd[1]*0.5)), c(-1, .wdt),
           type='n', bty='n', xaxt='n', xlab='',
           yaxt='n', ylab='', main="", font.main = 1)
      #bg + border
      rect(.min -.brd[2], -1, .max+(.brd[1]*0.5),  .wdt,
           col=bg, border=border)
      #title
      if(title !=""){
        text(.min+((.max-.min)/2)+(.na.width*0.75), 13, labels=title, col="black", cex=0.75)
      }
      #col scale
      for (i in 1:(length(cols)-0)) {
        x <- (i-1)/scale + .min
        rect(x,5,x+1/scale,10,col=cols[i], border=NA)
      }
      #axes
      lines(c(.min, .max), c(5,5), col="black")
      for (i in ticks) {
        lines(c(i,i), c(5,4),col="black")
      }
      #axes annotation
      text(ticks, rep(2, length(ticks)), labels=ticks,
           cex=0.75, adj=0.5)
      #na block
      if(!is.na(na.col)){
        rect(.min-(.na.width*0.5), 5,.min-(.na.width*1.5), 10,  col=na.col, border="black")
        text(.min-.na.width, 2, labels="NA", col="black", cex=0.75)
      }

    }

    if(type==2){
      #horizontal, header before, annotation after
      #margins
      .mai <- c(0.1,0.1,0.1,0.1)
      if(title ==""){
        #no title
        .fig <- c(x-0.05, x+0.05, y-0.2, y+0.1)
        .wdt <- 12
      } else {
        #title
        .fig <- c(x-0.05, x+0.05, y-0.2, y+0.1)
        .wdt <- 15
      }

      if(is.na(na.col)){
        .brd <- c(.na.width, .na.width)
      } else {
        .brd <- c(.na.width, .na.width*2)
      }

      #position col key
      par(fig = .fig, mai = .mai, new=TRUE)

      #plot col key

      #region
      plot(c(-1, .wdt), c(.min-.brd[2], .max+(.brd[1]*0.5)),
           type='n', bty='n', xaxt='n', xlab='',
           yaxt='n', ylab='', main="", font.main = 1)
      #bg + border
      rect(-1, .min-.brd[2], .wdt, .max+(.brd[1]*0.5),
           col=bg, border=border)
      #title
      if(title !=""){
        text(.min+((.max-.min)/2)+(.na.width*0.75), 13, labels=title,
             col="black", cex=0.75)
      }

      #for (i in 1:(length(lut)-1)) {
      #  y = (i-1)/scale + min
      #  rect(0,y,10,y+1/scale, col=lut[i], border=NA)
      #}

      #col scale
      ####################
      #note
      ####################
      #this needs work because rect needs colored border
      #which kills transparent ranges...
      #does that matter
      for (i in 1:(length(cols))) {
        y <- (i-1)/scale + .min
        rect(5,y-(1/scale),10,y,col=cols[i], border=cols[i])
      }
      #axes
      lines(c(5,5), c(.min, .max), col="black")
      for (i in ticks) {
        lines(c(5,4), c(i,i), col="black")
      }
      #axes annotation
      text(rep(2, length(ticks)), ticks, labels=ticks,
           cex=0.75, adj=0.5)
      #na block
      if(!is.na(na.col)){
        rect(5, .min-(.na.width*0.5), 10, .min-(.na.width*1.5), col=na.col, border="black")
        text(2, .min-.na.width, labels="NA", col="black", cex=0.75)
      }

    }

  par(op)
}

#plot(iris$Sepal.Length, iris$Sepal.Width)
#rsp_col_key(c(1,-1), colorRampPalette(c("light green", "yellow", "orange", "red"))(100), title="testing")








###########################
#diagnostic
##########################

#######################
##rethink this!!!!
#######################

#parking for now


#https://www.rpubs.com/NYang/ANLY530-IRIS

#devtools::unload(); devtools::load_all()
#a <- xxx_test(); subset(a, a$.ref)$PROFILE_NAME

#xxx_test <- function(){
#
#  .tmp <- sysdata$PROFILES
#  .out <-as.data.table(sp_dcast_profile(sp_profile(.tmp$PROFILE_CODE)))
#  .tmp <- .tmp[c("PROFILE_CODE", "Keywords")]
#  .tmp$.ref <- grepl("wildfire", tolower(.tmp$Keywords)) |
#    grepl("burning", tolower(.tmp$Keywords))
#  .out <- merge(.out, .tmp)
#  .out <- .out[-1599,]
#  #glfit<-glm(as.numeric(.out$.ref)~.out$`Organic carbon` + .out$Nitrate +
#  #             .out$Sulfate, family = 'binomial')
#  glfit<-glm(as.numeric(.out$.ref)~.out$Calcium + .out$Lead +
#                            .out$Zinc +.out$Manganese, family = 'binomial')
#  print(summary(glfit))
#  .out$.pred <- NA
#  .out$.pred[as.numeric(names(predict(glfit)))] <- predict(glfit, type="response")
#  .out
#  #glfit
#}



######################################
#matching
######################################

#Camp Fire

#pb (lead) 0.04 - 0.13 ug/m3
#zinc      0.15 - 0.45
#manganese 0.008 - 0.012
#calcium   0.1-0.2
#organic carbon 20-100

#



########################################
#pca
########################################

#see notes at
#https://www.ime.usp.br/~pavan/pdf/PCA-R-2013
#on r pca methods

#https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
#https://stats.stackexchange.com/questions/56089/using-varimax-rotated-pca-components-as-predictors-in-linear-regression
#https://stackoverflow.com/questions/34944179/doing-pca-with-varimax-rotation-in-r
#on varimax rotation
