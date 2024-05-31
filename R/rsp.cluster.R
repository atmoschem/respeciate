#' @name rsp.cluster
#' @title Profile cluster analysis methods
#' @aliases rsp_distance_profile

#' @description Functions for studying similarities (or
#' dissimilarities) within respeciate data sets

#' @description \code{\link{rsp_distance_profile}} calculates the statistical distance
#' between respeciate profiles, and clusters profiles according to nearness.
#' @param rsp A \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles.
#' @param output Character vector, required function output: \code{'report'} the
#' calculated distance matrix; \code{'plot'} a heat map of that distance
#' matrix.
#' @note Please note: function in development; structure and arguments may be
#' subject to change.
#' @return Depending on the \code{output} option, \code{sp_distance_profile} returns
#' one or more of the following: the correlation matrix, a heat map of the
#' correlation matrix.


########################
#to think about
########################

#think about other functions
#   variations on current dist
#   kmeans
#   others

# https://www.statology.org/k-means-clustering-in-r/

#NOTE

#  using data.table for dcast

# start build the code for the matching method
# and the dissim function, etc...


#needs a lot of work
#    needs thinking through
#    needs options/formals

#output like in rsp_cor_species

#also check through and consider other options in sp_profile_cor

#currently tracking

#think about how we handle too-big matrices, e.g.
#   aa <- rsp_profile(rsp_find_profile("ae6", by="profile_type"))
#   rsp_distance_profile(aa)


#test
######################

#aa <- rsp_profile(rsp_find_profile("ae8", by="profile_type"))
#rsp_distance_profile(aa)

#' @rdname rsp.cluster
#' @export

rsp_distance_profile <- function(rsp, output = c("plot", "report")){

  #add .value if missing
  ######################
  # SPECIEUROPE data
  ######################
  if("rsp_eu" %in% class(rsp)){
    rsp <- .rsp_eu2us(rsp)
  }
  #######################
  x <- .rsp_tidy_profile(rsp)

  # make by profile (rows) by species (columns) data.frame
  # move profile_code to row.names for heatmap
  .x <- rsp_dcast(x, widen = "species")
  .tmp <- .x[-1:-2]
  row.names(.tmp) <- .x[,1]

  #dist calculation
  #reset NAs to 0
  #   that might not be best option...
  .dst <- dist(scale(.tmp, center = TRUE, scale = TRUE))
  .dst[is.na(.dst)] <- 0

  #currently not using these two...
  #   should drop or re-include
  #      (think this is a shortcut to similar when used with image)
  .clst <- hclust(.dst, method = "average")
  ord <- order(cutree(.clst, k = 3))

  #print(ord)
  #image(as.matrix(.dst)[ord, ord])
  .cop <- cophenetic(.clst)
  #names(.dst) <- .x[,1]
  #image(as.matrix(.cop)[ord, ord])

  #######################
  #output
  #######################

  #think about
  #    could handle this like plot or
  #    using a heatmap and legend argument???
  #
  if("plot" %in% output){
    heatmap(as.matrix(.cop), Rowv = FALSE, Colv=FALSE, scale="none")
  }
  if("report" %in% output){
    invisible(.cop)
  }
}

#check through below on similarity matrices notes





#####################
#unexported
#####################


##from https://stackoverflow.com/questions/5639794/in-r-how-can-i-plot-a-similarity-matrix-like-a-block-graph-after-clustering-d

## MASS is for mvrnorm
#require(MASS)
## seed sets the output
#set.seed(1)
#dat <- data.frame(mvrnorm(100, mu = c(2,6,3),
#                          Sigma = matrix(c(10,   2,   4,
#                                           2,   3, 0.5,
#                                           4, 0.5,   2), ncol = 3)))

##Compute the dissimilarity matrix of the standardised data using Eucildean distances

#dij <- dist(scale(dat, center = TRUE, scale = TRUE))

##and then calculate a hierarchical clustering of these data using the group average method

#clust <- hclust(dij, method = "average")

##Next we compute the ordering of the samples on basis of forming 3 ('k') groups from the dendrogram, but we could have chosen something else here.

#ord <- order(cutree(clust, k = 3))

##Next compute the dissimilarities between samples based on dendrogram, the cophenetic distances:

#  coph <- cophenetic(clust)

##Here are 3 image plots of:

##  The original dissimilarity matrix, sorted on basis of cluster analysis groupings,
##The cophenetic distances, again sorted as above
##The difference between the original dissimilarities and the cophenetic distances
##A Shepard-like plot comparing the original and cophenetic distances; the better the clustering at capturing the original distances the closer to the 1:1 line the points will lie
##Here is the code that produces the above plots

#layout(matrix(1:4, ncol = 2))
#image(as.matrix(dij)[ord, ord], main = "Original distances")
#image(as.matrix(coph)[ord, ord], main = "Cophenetic distances")
#image((as.matrix(coph) - as.matrix(dij))[ord, ord],
#      main = "Cophenetic - Original")
#plot(coph ~ dij, ylab = "Cophenetic distances", xlab = "Original distances",
#     main = "Shepard Plot")
#abline(0,1, col = "red")
#box()
#layout(1)

#and testing

#might be something in here!!

##looks like I was using ae8 and ae6
## so 6 and 219 profiles
## full data base is 6845
## so be useful
#a <- sp_profile.2(find_sp_profile("pm-ae8", by="profile_type"))
#a <- sp_profile.2(find_sp_profile("pm-ae6", by="profile_type"))

#dat <- data.frame(MASS::mvrnorm(100, mu = c(2,6,3),
#                                Sigma = matrix(c(10,   2,   4,
#                                                 2,   3, 0.5,
#                                                 4, 0.5,   2), ncol = 3)))
#dat
#dij <- dist(scale(dat, center = TRUE, scale = TRUE))
#image(dij, main = "Original distances")
#image(as.matrix(dij), main = "Original distances")
#dij
#image(as.matrix(dij[order(dij$V1),]), main = "Original distances")
#dij
#a <- sp_profile.2(find_sp_profile("pm-ae8", by="profile_type"))
#a
#b <- test_wide(a)
#head(b)
#head(b[-1:-2])
#bb <- b[-1:-2])
#bb <- b[-1:-2]
#dij <- dist(scale(bb, center = TRUE, scale = TRUE))
#image(as.matrix(dij), main = "Original distances")
#heatmap(as.matrix(dij), main = "Original distances")
#b$PROFILE_NAME
#heatmap(as.matrix(dij))
#?heatmap
#a <- sp_profile.2(find_sp_profile("pm-ae6", by="profile_type"))
#b <- test_wide(a)
#bb <- b[-1:-2]
#heatmap(as.matrix(dij))
#dij <- dist(scale(bb, center = TRUE, scale = TRUE))
#heatmap(as.matrix(dij))
#clust <- hclust(dij, method = "average")
#?hclust
#class(dij)
#dij
#summary(dij)
#dij[is.na(dij=0)]
#dij[is.na(dij)]
#dij[is.na(dij)]<- 0
#heatmap(as.matrix(dij))
#clust <- hclust(dij, method = "average")
#ord <- order(cutree(clust, k = 3))
#heatmap(as.matrix(dij)[ord, ord])
#coph <- cophenetic(clust)
#image(as.matrix(coph)[ord, ord], main = "Cophenetic distances")
#image(as.matrix(dij)[ord, ord])
#ord <- order(cutree(clust, k = 7))
#coph <- cophenetic(clust)
#image(as.matrix(dij)[ord, ord])
#image(as.matrix(coph)[ord, ord], main = "Cophenetic distances")
#image((as.matrix(coph) - as.matrix(dij))[ord, ord],
#      main = "Cophenetic - Original")
#plot(coph ~ dij, ylab = "Cophenetic distances", xlab = "Original distances",
#     main = "Shepard Plot")
#abline(0,1, col = "red")
#dij <- dist(scale(bb, center = TRUE, scale = TRUE))
#summary(dij)
#dij[is.na(dij)]
#as.vector(dij)
#dij[is.na(dij)] <- (max(dij, na.rm=TRUE)*5)
#ord <- order(cutree(clust, k = 7))
#clust <- hclust(dij, method = "average")
#image(as.matrix(coph)[ord, ord], main = "Cophenetic distances")
#image(as.matrix(dij)[ord, ord], main = "Cophenetic distances")
#image(as.matrix(dij), main = "Cophenetic distances")
#dij <- dist(scale(bb, center = TRUE, scale = TRUE))
#dij[is.na(dij)] <- (max(dij, na.rm=TRUE)*1)
#clust <- hclust(dij, method = "average")
#ord <- order(cutree(clust, k = 7))
#image(as.matrix(dij)[ord, ord], main = "Cophenetic distances")
#image(as.matrix(dij), main = "Cophenetic distances")
#heatmap(dij)
#heatmap(as.matrix(dij))
#?heatmap
#max(dij)
#which(max==max(dij))
#which(max=max(dij))
#where(max=max(dij))
#?which
#where(max~max(dij))
#which(max~max(dij))
#which(dij==max(dij))
#summary(dij)
#?dist
#dij[order(dij)]
#dij[order(dij)][1:10]
#a <- sp_profile.2(find_sp_profile("pm-ae8", by="profile_type"))
#b <- test_wide(a)
#bb <- b[-1:-2]
#dij <- dist(scale(bb, center = TRUE, scale = TRUE))
#plot(dij)
#plot(as.matrix(dij))
#heatmap(as.matrix(dij))
#dij
#cor(bb)
#?cor
#cor(bb, use="pairwise.complete.obs")
#heapmap(cor(bb, use="pairwise.complete.obs"))
#heatmap(cor(bb, use="pairwise.complete.obs"))
#a <- sp_profile.2(find_sp_profile("pm-ae8", by="profile_type"))
#b <- test_wide(a)
#bb <- b[-1:-2]
#heatmap(cor(bb, use="pairwise.complete.obs"))
#cor(bb, use="pairwise.complete.obs")
#plot(cor(bb, use="pairwise.complete.obs"))
#is(cor(bb, use="pairwise.complete.obs"))
#o <- cor(bb, use="pairwise.complete.obs")
#o[is.na(o)] <- -2
#heatmap(o)
#o[o== -2] <- -4
#heatmap(o)
#o[o== -4] <- -1
#heatmap(o)
#?heatmap
#heatmap(o, Rowv = FALSE)
#heatmap(o, Rowv = FALSE, Colv=FALSE)
#o
#View(o)
#o[o== -1] <- 0
#heatmap(o, Rowv = FALSE, Colv=FALSE)
#heatmap(o, Rowv = FALSE, Colv=FALSE, scale="none")
#heatmap(o, scale="none")
#heatmap(dij, scale="none")
#heatmap(as.matrix(dij), scale="none")
#a
#b
#apply(b, 1, function(x) {length(x[!na(x)])})
#apply(b, 1, function(x) {length(x[!is.na(x)])})
#apply(b, 2, function(x) {length(x[!is.na(x)])})
#b$Nickel
#?cor
#longley
#cor(longley)
#heatplot(cor(longley), scale="none")
#heatmap(cor(longley), scale="none")
#symnum(cor(longley))
#b
#apply(b, 2, function(x) {length(x[!is.na(x)])})
#oo <- b[apply(b, 2, function(x) {length(x[!is.na(x)])})>3]
#oo
#cor(oo)
#o <- cor(oo, use="pairwise.complete.obs")
#o <- cor(oo[(-1:-2)], use="pairwise.complete.obs")
#heatmap(o, scale="none")
#o
#heatmap(o^2, scale="none")
#heatmap(o, scale="none")
#find_sp_species("Ethanol")
#find_sp_profile("Ethanol", by="species_name")
#find_sp_profile("Ethanol", by="species_name", partial=FALSE)
#find_sp_profile("Ethanne", by="species_name", partial=FALSE)
#find_sp_profile("Ethane", by="species_name", partial=FALSE)
#pr <- sp_profile(find_sp_profile("Ethane", by="species_name", partial=FALSE))
#pr
#summary(factor(pr$PROFILE_NAME))
#summary(factor(pr$PROFILE_TYPE))
#summary(factor(pr$SPECIES_NAME))
#pr$SPECIES_NAME=="Ethane"
#pr[pr$SPECIES_NAME=="Ethane",]
#pr[pr$SPECIES_NAME=="Ethane",]
#plot(pr[pr$SPECIES_NAME=="Ethane",]$WEIGHT_PERCENT)
#summary(factor(pr$PROFILE_NAME))
#.pr <- pr[pr$SPECIES_NAME=="Ethane",]
#summary(factor(.pr$PROFILE_NAME))
#summary(factor(.pr$PROFILE_CODE))
#.pr <- .pr[.pr$PROFILE_CODE=="0296",]
#.pr
#as.data.frame(.pr)
#View(.pr)
#sysdata$REFERENCES
#test2(.pr)
#test_wide(.pr)
#pr
