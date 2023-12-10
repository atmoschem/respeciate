#' @name sp.plot
#' @title plotting (re)SPECIATE profiles
#' @aliases sp_plot_profile

#' @description General plots for \code{respeciate} objects.

#' @description \code{sp_plot} functions generate plots for supplied
#' (re)SPECIATE profile data sets.
#' @param x A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @param id numeric, indices of profiles to use when
#' plotting (e.g. \code{id=1:6} plots first 6 profiles).
#' @param multi.profile character, how plot should handle
#' multiple profiles, e.g. 'group' or 'panel' (default
#' group).
#' @param order logical, order the species in the
#' profile(s) by relative abundance before plotting.
#' @param log logical, log y scale when plotting.
#' @param ... any additional arguments, typically passed on the lattice
#' plotting functions.
#' @param silent logical, hide warnings when generating plots (default
#' \code{FALSE})
#' @param multi.species, like \code{multi.profile} but for species.
#' @return \code{sp_plot} graph, plot, etc usually as a trellis object.
#' @note These functions are currently in development, so may change.

#functions
# sp_plot_profile
# sp_plot_species

# plot.respeciate is wrapper for sp_plot_profile

#use unexported
# rsp_plot_fix



#JOBS
#######################

#reference lattice and latticeEXtra packages in documents...

#all functions need work
#see function fix, tidy, etc job notes in code

#thinking about an sp_plot_compare(x, y)
#  to compare profile x and profile(s) y
#  started project (in own-notes)

###################################
#sp_plot_profile
###################################

#' @rdname sp.plot
#' @export
#   now done in xxx.r

##########################
#notes
##########################
#moved this to lattice for paneling option

############################
#using rsp_plot_fix for warning/handling for
#  duplicate species in profiles (handling merge/mean)
#  duplicated profile names (handling make unique)

#############################
#using rsp_test_profile
#   when ordering...

#see in code notes about jobs


sp_plot_profile <-   function(x, id, multi.profile = "group",
                              order=TRUE, log=FALSE, ...,
                              silent=FALSE){

  #setup
  .x.args <- list(...)

  #currently not even trying to stack logs...
  if("stack" %in% names(.x.args) && .x.args$stack){
    if(log){
      ######################
      #to do
      #document issues
      stop("RSP> Sorry, currently not stacking logs.",
           call. = FALSE)
    }
  }
  #others refs
  #.sp.ord <- unique(x$SPECIES_ID)
  .sp.pro <- unique(x$PROFILE_CODE)
  #n/profile handling
  profile <- if (missing(id)) {
    .sp.pro
  } else {
    id
  }
  if (is.numeric(profile)) {
    profile <- if (all(profile == -1)) {
      .sp.pro
    }
    else {
      .sp.pro[profile]
    }
  }
  if (!any(profile %in% .sp.pro) | any(is.na(profile))) {
    stop("RSP> unknown profile(s) or missing ids, please check",
         call. = FALSE)
  }
  if(length(profile)>6 & missing(id)){
    if(!silent){
      warning("RSP> ", length(profile), " profiles... ",
              "just showing first 6 to reduce plot clutter",
              "\n\t (maybe use id to force larger range if sure)",
              sep="", call.=FALSE)
    }
    profile <- profile[1:6]
  }
  x <- x[x$PROFILE_CODE %in% profile,]

  #check for duplicates, etc...
  #tidy naming etc...
  x <- rsp_plot_fix(x, silent=silent, ...)

  ##test something to plot
  if(nrow(x)==0){
    ######################
    #think about this
    ######################
    #maybe stop() instead???
    #stop("empty respeciate object?")
    #maybe warning() aw well??
    return(invisible(NULL))
  }

  ####################################
  #switching profile from profile_code to profile_name...
  #    for plot labeling
  ####################################
  profile <- unique(x$PROFILE_NAME)
  #should think about other naming options???

  #order largest to smallest
  #############################
  #like to also be able to order by molecular weight
  ##############################
  if(order){
    ################################
    #bit of a cheat...
    ################################
    test <- x
    test$PROFILE_CODE <- ".default"
    test <- rsp_test_profile(test)
    #previous barplot had bedside
    if("stack" %in% names(.x.args) && .x.args$stack){
      test <- test[order(test$.total, decreasing = TRUE),]
      xx <- unique(test$SPECIES_NAME)
    } else {
      test <- x[order(x$WEIGHT_PERCENT, decreasing = TRUE),]
      xx <- unique(test$SPECIES_NAME)
    }
  } else {
    xx <- unique(x$SPECIES_NAME)
  }
  x <- x[c(".value", "PROFILE_NAME", "SPECIES_NAME")]

  x$SPECIES_NAME <- factor(x$SPECIES_NAME,
                           levels = xx)

  ##################
  #profile bar chart
  ##################
  p1.ls <- list(x= .value~SPECIES_NAME,
                data=x, ylab="Profile Loading", xlab="",
                #NB: prepanel seemed to break ylim when stacking
                panel = function(x, y, origin, ...){
                  rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                               panel.grid, ...)
                  if(missing(origin)){
                    ######################################
                    #we are getting -Inf from somewhere...
                    ######################################
                    .y <- y[is.finite(y)]
                    origin <- if(min(.y, na.rm=TRUE) < 0 ) {
                      min(.y, na.rm=TRUE) - 0.02
                    } else {
                      0
                    }
                  }
                  panel.barchart(x=x, y=y, origin=origin, ...)
                },
                between=list(y=.2),
                scales=list(x=list(rot=90,
                                   cex=0.7,
                                   alternating=1),
                            y=list(rot=c(0,90),
                                   cex=0.7,
                                   alternating=3,
                                   relation="free"))
  )

  #1.if multiple profiles, panel or group
  if(length(profile)>1){
    if(tolower(multi.profile) %in% c("panel", "panels")){
      #paneling multiple panels
      p1.ls$x <- .value~SPECIES_NAME | PROFILE_NAME
    } else {
      #grouping multiple panels
      p1.ls$x <- .value~SPECIES_NAME
      p1.ls$groups <- x$PROFILE_NAME
    }
  }

  #2. if log=TRUE
  #shortcut to scales(y(log)) and yscale.component
  if(log){
    p1.ls$scales$y$log <- 10
    p1.ls$yscale.components <- rsp_yscale.component.log10
  }

  #3. extra user settings
  p1.ls <- modifyList(p1.ls, .x.args)

  #4. tidy loose ends

  #set cols
  if("col" %in% names(p1.ls)){
    if(is.function(p1.ls$col)){
      p1.ls$col <- if("groups" %in% names(p1.ls)){
        p1.ls$col(length(profile))
      } else {
        p1.ls$col(1)
      }
    }
  } else {
    p1.ls$col <- if("groups" %in% names(p1.ls)){
      rep(trellis.par.get("superpose.polygon")$col,
          length.out=length(profile))
    } else {
      trellis.par.get("superpose.polygon")$col[1]
    }
  }
  #here we must have col
  #################################
  #issue here if user sets par.settings
  .tmp <- list(superpose.polygon = list(col = p1.ls$col),
               superpose.symbol = list(fill = p1.ls$col))
  p1.ls$par.settings <- if("par.settings" %in% names(p1.ls)){
    modifyList(.tmp, p1.ls$par.settings)
  } else {
    .tmp
  }
  #add key if needed
  #################################
  #issue if user uses key = FALSE: should remove..
  #could be issue here if user uses auto.key???
  #issue if user wants to use x and y to put key in plot: also needs space=NULL
  #like to track border as well as col...
  #want to add box behind legend when key in plot...
  if("groups" %in% names(p1.ls)){
    .tmp <- list(space="right",
                 #title="Legends",
                 rectangles=list(col=rep(p1.ls$col,
                                         length.out=length(profile))),
                 text = list(profile, cex=0.7))
    p1.ls$key <- if("key" %in% names(p1.ls)){
      modifyList(.tmp, p1.ls$key)
    } else {
      .tmp
    }
  }
  do.call(barchart, p1.ls)
}



###################################
#sp_plot_species
###################################

#' @rdname sp.plot
#' @export

#in development

#taken straight from sp_plot_profile
#so lots of redundancy

sp_plot_species <- function(x, id, multi.species = "group",
                              order=FALSE, log=FALSE, ...,
                              silent=FALSE){

  #setup
  .x.args <- list(...)

  ######################################
  #not sure we are using stack for this
  ######################################
  #currently not even trying to stack logs...
  if("stack" %in% names(.x.args) && .x.args$stack){
    if(log){
      ######################
      #to do
      #document issues
      stop("RSP> Sorry, currently not stacking logs.",
           call. = FALSE)
    }
  }


  #need to get species as character
  .sp.ord <- as.character(unique(x$SPECIES_ID))
  #.sp.pro <- unique(x$PROFILE_CODE)
  #n/profile handling
  species <- if (missing(id)) {
    .sp.ord
  } else {
    id
  }

  if (is.numeric(species)) {
    species <- if (all(species == -1)) {
      .sp.ord
    }
    else {
      .sp.ord[species]
    }
  }
  if (!any(species %in% .sp.ord) | any(is.na(species))) {
    stop("RSP> unknown species(s) or missing ids, please check",
         call. = FALSE)
  }
  if(length(species)>20 & missing(id)){
    if(!silent){
      warning("RSP> ", length(species), " species... ",
              "just showing first 20 to reduce plot clutter",
              "\n\t (maybe use id to force larger range if sure)",
              sep="", call.=FALSE)
    }
    species <- species[1:20]
  }
  x <- x[x$SPECIES_ID %in% species,]

  #check for duplicates, etc...
  #tidy naming etc...
  x <- rsp_plot_fix(x, silent=silent, ...)

  ##test something to plot
  if(nrow(x)==0){
    ######################
    #think about this
    ######################
    #maybe stop() instead???
    #stop("empty respeciate object?")
    #maybe warning() aw well??
    return(invisible(NULL))
  }

  ####################################
  #like sp_plot_profile...
  ####################################
  species <- unique(x$SPECIES_NAME)
  #should think about other naming options???
  x$SPECIES_NAME <- factor(x$SPECIES_NAME, levels=species)

#ignoring option to re-order at moment

  #order largest to smallest
  #############################
  #like to also be able to order by molecular weight
  ##############################
  if(order){
    ################################
    #bit of a cheat...
    ################################
    test <- x
    test$PROFILE_CODE <- ".default"
    test <- rsp_test_profile(test)
    #previous barplot had bedside
    if("stack" %in% names(.x.args) && .x.args$stack){
      test <- test[order(test$.total, decreasing = TRUE),]
      xx <- unique(test$SPECIES_NAME)
    } else {
      test <- x[order(x$WEIGHT_PERCENT, decreasing = TRUE),]
      xx <- unique(test$SPECIES_NAME)
    }
  } else {
    xx <- unique(x$SPECIES_NAME)
  }
  x <- x[c(".value","PROFILE_CODE", "PROFILE_NAME", "SPECIES_NAME")]


  ##################
  #profile bar chart
  ##################

  #dcast and melt to add in any missed entries as NAs
  #(for the plot trail)
  #not padding
  x <- sp_melt_wide(sp_dcast_species(x), pad=FALSE, drop.nas = FALSE)


  ###############################
  #species handling
  ##############################

  #dropped for now...
  #x$SPECIES_NAME <- factor(x$SPECIES_NAME,
  #                         levels = xx)

  #############################
  # x axis handling
  #############################

  #currently calculating and using sample index
  #could add alternatives
  #  use profile_code column as is
  #     x$x.id <- x$PROFILE_CODE
  #        would also want an xlab change....
  #  use a different column?
  #  convert to factor
  #       but then by default lattice shows all factors labels...
  #  format using a supplied function???
  x$x.id <- as.numeric(factor(x$PROFILE_CODE))

  ##############################
  #species alignment



  p1.ls <- list(x= .value~x.id,
                data=x, ylab="Measurement", xlab="Sample [index]",
                type="l",
                #NB: prepanel seemed to break ylim when stacking
                panel = function(x, y, ...){
                  rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                               panel.grid, ...)
                  panel.xyplot(x=x, y=y, ...)
                },
                between=list(y=.2),
                scales=list(x=list(rot=0,
                                   cex=0.7,
                                   alternating=1),
                            y=list(rot=c(0,90),
                                   cex=0.7,
                                   alternating=3,
                                   relation="free"))
  )

  #1.if multiple profiles, panel or group
  if(length(species)>1){
    if(tolower(multi.species) %in% c("panel", "panels")){
      #paneling multiple panels
      p1.ls$x <- .value~x.id | SPECIES_NAME
    } else {
      #grouping multiple panels
      p1.ls$x <- .value~x.id
      p1.ls$groups <- x$SPECIES_NAME
    }
  }

  #2. if log=TRUE
  #shortcut to scales(y(log)) and yscale.component
  if(log){
    p1.ls$scales$y$log <- 10
    p1.ls$yscale.components <- rsp_yscale.component.log10
  }

  #3. extra user settings
  p1.ls <- modifyList(p1.ls, .x.args)

  #4. tidy loose ends

  #set cols
  if("col" %in% names(p1.ls)){
    if(is.function(p1.ls$col)){
      p1.ls$col <- if("groups" %in% names(p1.ls)){
        p1.ls$col(length(species))
      } else {
        p1.ls$col(1)
      }
    }
  } else {
    p1.ls$col <- if("groups" %in% names(p1.ls)){
      rep(trellis.par.get("superpose.line")$col,
          length.out=length(species))
    } else {
      trellis.par.get("superpose.line")$col[1]
    }
  }
  #here we must have col
  #################################
  #issue here if user sets these????
  p1.ls$par.settings = list(superpose.polygon = list(col = p1.ls$col),
                            superpose.symbol = list(fill = p1.ls$col))
  #add key if needed
  #################################
  #issue if user uses key = FALSE
  #could be issue here if user uses auto.key???
  #like to track border as well as col...
  if("groups" %in% names(p1.ls)){
    .tmp <- list(space="right",
                 #title="Legends",
                 lines=list(col=rep(p1.ls$col,
                                    length.out=length(species))),
                 text = list(species, cex=0.7))
    p1.ls$key <- if("key" %in% names(p1.ls)){
      modifyList(.tmp, p1.ls$key)
    } else {
      .tmp
    }
  }
  do.call(xyplot, p1.ls)
}
