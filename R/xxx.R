##############################
#setup code, misc code,
#testing code, etc
##############################

#currently no hooks, etc...


utils::globalVariables(c("SPECIES_ID", "SPECIES_NAME", "PROFILE_NAME",
                         "SPEC_MW", "WEIGHT_PERCENT", ".", ".value"))
########################
#to think about...
#######################

# all @import here
#    in case we have to move to data.table::as.data.table, etc...

#' @import data.table

#   data.table used by:
#         rsp_test_profile,
#         sp_dcast_profile, and those that use dcast?
#               sp_species_cor
#               sp_profile_distance
#         and others???
#               need to identify them

#' @importFrom stats sd cophenetic cor cutree dist hclust heatmap
#' @importFrom utils modifyList
#' @importFrom graphics axis barplot par legend
#' @importFrom grDevices cm.colors


##############################
#common unexported
##############################


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
##  is only allows 1 argument.

## could also test for .value

rsp_test_respeciate <- function(x, level = 1){
  test <- class(x)
  if(level == 1){
    if(test[1] == "respeciate"){
      if(all(c("SPECIES_NAME", "SPECIES_ID", "PROFILE_NAME", "PROFILE_CODE",
               "WEIGHT_PERCENT") %in% names(x))){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  if(level==2){
    if(test[1] == "respeciate"){
      if(all(c("SPECIES_NAME", "SPECIES_ID", "PROFILE_NAME", "PROFILE_CODE",
               "WEIGHT_PERCENT") %in% names(x))){
        return(TRUE)
      }
    }
    if(test[1] == "respeciate.ref"){
      if(all(c("PROFILE_NAME", "PROFILE_CODE") %in% names(x))){
        return(TRUE)
      }
    }
    if(test[1] == "respeciate.spcs"){
      if(all(c("SPECIES_NAME", "SPECIES_ID") %in% names(x))){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  FALSE
}

#######################
#tidy profile

#I am thinking of using a .value column as my value column
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
#tidy names

#currently not exported
#quick code to tidy species names

#currently used in plot.respeciate

#note: not fully tested


rsp_tidy_species_name <- function(x){

  #attempts shorten names by remove other versions
  #names seem to be in format a (or b || c)
  #where (guessing) a is main name and
  #         b and c are alternatives.

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
  xx <- as.data.table(x)
  out <- xx[,
            .(PROFILE_NAME = PROFILE_NAME[1],
              SPECIES_NAME = SPECIES_NAME[1],
              SPEC_MW = SPEC_MW[1],
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


