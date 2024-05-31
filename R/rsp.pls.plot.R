#' @name rsp.pls.plot
#' @title Plots for use with respeciate profile Positive Least Squares models
#' @aliases pls.plot pls_plot pls_plot_species pls_plot_profile

#' @description
#' The \code{pls_plot} functions are intended for use with PLS models built
#' using \code{rsp_pls_profile} (documented separately). They generate some
#' plots commonly used with source apportionment model outputs.

#' @param pls A \code{rsp_pls_profile} output, intended for use with
#' \code{pls_} functions.
#' @param id numeric or character
#' identifying the species or profile to plot. If numeric, these are treated
#' as indices of the species or profile, respectively, in the PLS model; if
#' character, species is treated as the name of species and profile is treated
#' as the profile code. Both can be concatenated to produce multiple plots and
#' the special case \code{id = -1} is a short cut to all species or profiles,
#' respectively.
#' @param plot.type numeric, the plot type if
#' multiple options are available.
#' @param ... other arguments, typically passed on to the associated
#' \code{lattice} plot.
#' @param output character, output method, one of: 'plot' to return just the
#' requested plot; 'data' to return just the data; and, c('plot', 'data') to
#' plot then return the data invisibly (default).
#' @param log (for \code{pls_plot_profile} only) logical, if \code{TRUE} this
#' applies 'log' scaling to the primary Y axes of the plot.

#########################
# need to check terminology for this...
#      The zero handling is a based on offset in plot(..., log="y", off.set)
#      but automatically estimated...
# shifted type to plot.type because it conflicts with type in lattice::xyplot....

# all plots use .rsp_plot_output
#     so need to update them all if .rsp function formals change
#     also pls_plot_profile uses rsp_plot_profile with output forced...
#          for first plot layer

# all plots have old versions
#     pls_plot... old
#         can hopefully loose these at some point.

#' @return \code{pls_plot}s produce various plots commonly used in source
#' apportionment studies.

# GENERAL NOTES

# TO DO

# these all need code tidying

# check individual function notes



####################################
###################################
## pls_plots
###################################
###################################

#these are all draft


###################################
###################################
## pls_plot
###################################
###################################


#' @rdname rsp.pls.plot
#' @export

## this replaces previous pls_plot (now pls_plot.old)

##   now imports via data.table::
##        need this to kill the as.data.table load message
##   #' @import data.table

#test
#devtools::load_all()
#d1 <- readRDS("C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\test\\my.working.rds")
#ref <- rsp(c("4868", "4914", "8948", "91155", "91163", "95441", "95529"))
#mod <- rsp_pls_profile(d1, ref, power=2)
#pls_plot(mod)


pls_plot <- function (pls, plot.type = 1, ...,
                      output="default"){

  #current using lattice/latticeExtra for the panelling/layers...

  ########################
  # to do
  ########################
  # id

  #basic plots in development...
  #    plot element ordering
  #         currently as it comes...
  #         because *I think* pls_report kills all pre-model handling...
  #    no id handling
  #         that maybe need to be in plot type...
  #         maybe also want to do it at end
  #               so missing case locations are retained (if needed for plot)...

  ############################
  # nags
  ############################

  # pls_plot(..., horizontal=FALSE) errors
  #      should flip x and y...

  # type = 1
  ############################
  #in development
  #pls fit summary
  #   simple proportional fit plot
  #think like
  #    https://latticeextra.r-forge.r-project.org/man/postdoc.html
  #    (but without the 100 percent (proportion=1) limit...)


  # type = 2
  ############################
  #to do?

  #################
  #setup
  #################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- .rsp_profile_code_order(dat)
  ######################################
  #option to not do name simplification?
  #
  dat$SPECIES_NAME <- .rsp_tidy_species_name(dat$SPECIES_NAME)
  .sp.ref <- unique(dat$SPECIES_NAME)

  #type
  if(!plot.type %in% c(1)){
    stop("pls_plot: plot.type unknown, check ?pls_plot",
         call. = FALSE)
  }

  ############################
  #type 1
  ############################
  if(plot.type==1){
    ##################################
    # note
    # maybe use .rsp_get_prop_from_pls
    #     but check naming change...???
    ##################################
    .tmp <- names(dat)
    .tmp <- .tmp[grep("^.x_", .tmp)]
    .refs <- c(.tmp, "pred")
    #make summary pls. prop.table
    .ans <- lapply(.sp.ref, function(x){
      .tmp <- subset(dat, SPECIES_NAME==x)
      .d2 <- .tmp[1, c("SPECIES_NAME", .refs)]
      for(.ref in .refs){
        #use only paired cases to calculate skew...
        .tmp2 <- .tmp[c(.ref, ".value")]
        .tmp2[.tmp2==0] <- NA
        .tmp2 <- na.omit(.tmp2)
        .d2[, .ref] <- sum(.tmp2[,.ref], na.rm=TRUE) / sum(.tmp2[,".value"], na.rm=TRUE)
      }
      .d2
    })
    .ans <- do.call(rbind, .ans)

    #barchart formula
    .for <- paste(.tmp, collapse="+")
    .for <- as.formula(paste("SPECIES_NAME~", .for, sep=""))
    .tmp <- gsub("^.x_", "", .tmp)
    #plot lists
    gr.ls <- list(h=-1, v=-1, col="grey", lty=3)
    if("grid" %in% names(.x.args) && is.list(.x.args$grid)){
      gr.ls <- modifyList(gr.ls, .x.args$grid)
      .x.args$grid <- NULL
    }
    bar.ls <- list(v=c(0.5,1,2), lty=c(3,2,3), col="red")
    if("bars" %in% names(.x.args) && is.list(.x.args$bars)){
      bar.ls <- modifyList(bar.ls, .x.args$bars)
      .x.args$bars <- NULL
    }
    if("col" %in% names(.x.args)){
      #could allow function as col input???
      .cols <- rep(.x.args$col, length.out=length(.tmp))
    } else {
      .cols <- rainbow(length(.tmp))
      .x.args$col <- .cols
    }
    ky.ls <- list(space="right", text=list(text=.tmp),
                  rect=list(col=.cols))
    if("key" %in% names(.x.args) && is.list(.x.args$key)){
      if(any(c("x", "y") %in% names(.x.args$key))){
        ky.ls$space <- NULL
      }
      .x.args$key <- modifyList(ky.ls, .x.args$key)
    } else {
      .x.args$key <- ky.ls
    }
    #####################################
    #note
    #maybe use .rsp_panelpal for grid and bars
    #       see pls_plot_profile ???
    #####################################
    pl.ls <- list(x=.for, data=.ans, origin=0, stack=TRUE,
                  grid=TRUE, bars=TRUE, xlim=c(-0.025, NA),
                  xlab="mean(model) / mean(measurements)",
                  #prepanel=function(...){
                  #  ans <- lattice::prepanel.default.bwplot(...)
                  #  print(ans)
                  #  ans
                  #},
                  panel=function(...){
                    .temp <- list(...)
                    if(.temp$grid){
                      do.call(panel.grid, gr.ls)
                    }
                    panel.barchart(...)
                    if(.temp$bars){
                      do.call(panel.abline, bar.ls)
                    }
                  }
    )
    pl.ls <- modifyList(pl.ls, .x.args)
    p <- do.call(barchart, pl.ls)
  }

  #output
  ############################
  .rsp_plot_output(as.data.frame(.ans), pl.ls, p, output)
}









####################################
####################################
## pls_plot_profile
####################################
####################################


#' @rdname rsp.pls.plot
#' @export

##   now imports from xxx.r
##   #' @import data.table


#############################
#this needs a lot of work
#############################

#test
#devtools::load_all()
#d1 <- readRDS("C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\test\\my.working.rds")
#ref <- rsp(c("4868", "4914", "8948", "91155", "91163", "95441", "95529"))
#mod <- rsp_pls_profile(d1, ref, power=2)
#pls_plot_profile(mod)


# log scale may need work
#    but that is in rsp_plot_profile/unexported functions...

pls_plot_profile <- function (pls, plot.type=1, log = FALSE, ...,
                              output="default")
{
  #new version of pls_plot

  #to do
  ##############################
  # log (needs better axes control) but that is in rsp_profile_plot
  # id needs to be enabled...

  #setup
  #############################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- .rsp_profile_code_order(dat)
  ######################################
  #option to not do name simplification?
  #
  dat$SPECIES_NAME <- .rsp_tidy_species_name(dat$SPECIES_NAME)
  .sp.ref <- unique(dat$SPECIES_NAME)

  #plot.type control
  if(!plot.type %in% c(1)){
    stop("pls_plot_profile: plot.type unknown, check ?pls_plot_profile",
         call. = FALSE)
  }

  ############################
  #type 1
  ############################
  if(plot.type==1){
    #make first plot and output .ans
    ##############################
    #get profiles .m_ columns
    .ans <- .rsp_get_m_from_pls(dat)
    .p1.prof <- unique(.ans$PROFILE_CODE)
    #send to rsp_plot_profile with any user arguments
    #   to make first plot
    #set cols
    #set cols
    p1.ls <- list(rsp=.ans, layout =c(1, length(.p1.prof)), log=log,
                  multi.profile = "panel", id=1:length(.p1.prof),
                  order=FALSE, silent=TRUE)
    if(!"col" %in% names(p1.ls)){
      #maybe need better handling
      p1.ls$col <- trellis.par.get("superpose.symbol")$col[1]
    }
    #issue with species_code not being known made this tricky...
    p1.ls <- modifyList(p1.ls, .x.args)
    p1.ls$output <- "plot"
    p1 <- do.call(rsp_plot_profile, p1.ls)

    #make second plot and .ans2
    ######################################
    .ans2 <- .rsp_get_prop_from_pls(dat)
    .ans2$.pc <- .ans2$.prop * 100
    #could do this in the panel so any missing is greyed out ???
    .ans2$.pc[is.na(.ans2$.pc)] <- 0
    p2.ls <- .rsp_panelPal("tc", list(x =.pc~factor(SPECIES_NAME)|factor(PROFILE_CODE),
                                       data=.ans2,
                                       type=c("h", "p"), pch=18, layout=c(1,7),
                                       ylab="Total Contribution (%)",
                                       scales=list(x=list(rot=90))),
                  #note: function is cheat to use .rsp... outside lattice
                  #      could make it the default if no panel set in call???
                  function(...){list(...)}, ...)
    #if tc layer not turned off..
    if(!is.null(p2.ls)){
      if(!"col" %in% names(p2.ls)){
        #maybe need better handling
        p2.ls$col <- trellis.par.get("superpose.symbol")$col[2]
      }
      p2 <- do.call(xyplot, p2.ls)
      p1 <- update(doubleYScale(p1, p2, add.ylab2 = TRUE),
                   par.settings = simpleTheme(col=c(p1.ls$col[1], p2.ls$col[1]))
                   )
    }
  }

  #output
  ############################
  #list is void here...
  .rsp_plot_output(list(profile = .ans, tc = .ans2), list(), p1, output)
}








####################################
####################################
## pls_plot_species
####################################
####################################




#' @rdname rsp.pls.plot
#' @export

##   now imports from xxx.r
##   #' @import data.table


#############################
#this needs a lot of work
#############################

#test
#devtools::load_all()
#d1 <- readRDS("C:\\Users\\trakradmin\\OneDrive - University of Leeds\\Documents\\pkg\\respeciate\\test\\my.working.rds")
#ref <- rsp(c("4868", "4914", "8948", "91155", "91163", "95441", "95529"))
#mod <- rsp_pls_profile(d1, ref, power=2)
#pls_plot_species(mod)

# id enabled but
#    species order is always as supplied...
#         probably actually alphabetic
#         look like order(character(unique(PROFILE_CODE)))

# to do
#    limit default output to < 7 plots?
#    .rsp_panelpal handling like other plots
#        layer .mod ???
#    log ??? (not sure it is needed/useful)
#    decide how to reorder or rename species, profiles and x data
#        (do this in plots and data ???)
#    decide how to modify .index

pls_plot_species <- function (pls, id, plot.type=1, ...,
                              output = "default")
{
  #new version of pls_plot

  #to do
  ##############################
  # most stuff
  # log not sure about doing them...
  # id

  #setup
  #############################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- .rsp_profile_code_order(dat)
  .sp.ref <- unique(dat$SPECIES_NAME)
  my.species <- if (missing(id)) {
    .sp.ref
    #default option (print the lot...)
    ############################
    #possibly a warning if lots of species to plot
    ##################
  } else {
    id
  }
  if (is.numeric(my.species)) {
    if (all(my.species == -1)) {
      my.species <- .sp.ref
    }
    else {
      my.species <- .sp.ref[my.species]
    }
  }
  if(length(my.species)>6 & missing(id)){
    #to think about
    #   option to turn off warning???
    #      (using in older versions of code)
    #if(!silent){
      warning("RSP/PLS> ", length(my.species), " species... ",
              "just showing first 6 to reduce plot clutter",
              "\n\t (maybe use id to force larger range if sure)",
              sep="", call.=FALSE)
    #}
    my.species <- my.species[1:6]
  }

  ######################################
  #option to not do name simplification?
  #
  dat$SPECIES_NAME <- .rsp_tidy_species_name(dat$SPECIES_NAME)
  .sp.ref <- unique(dat$SPECIES_NAME)
  my.species <- .rsp_tidy_species_name(my.species)

  if (!any(my.species %in% .sp.ref)) {
    stop("pls_plot_species> unknown species, please check", call. = FALSE)
  }

  .tmp <- dat[c("SPECIES_NAME", "PROFILE_CODE", ".value")]
  .tmp <- data.table::as.data.table(.tmp)
  .tmp <- data.table::dcast(.tmp, PROFILE_CODE ~ SPECIES_NAME,
                            mean,
                            na.rm=TRUE,
                            value.var = ".value")
  .tmp2 <- data.table::melt(.tmp, id.vars="PROFILE_CODE", variable.name="SPECIES_NAME",
                           value.name=".value")
  .tmp <- dat[c("SPECIES_NAME", "PROFILE_CODE", "pred")]
  .tmp <- data.table::as.data.table(.tmp)
  .tmp <- data.table::dcast(.tmp, PROFILE_CODE ~ SPECIES_NAME,
                            mean,
                            na.rm=TRUE,
                            value.var = "pred")
  .tmp <- data.table::melt(.tmp, id.vars="PROFILE_CODE", variable.name="SPECIES_NAME",
                           value.name="pred")
  .tmp <- data.table::merge.data.table(.tmp2, .tmp)
  .tmp <- as.data.frame(.tmp)
  .tmp$.index <- as.numeric(factor(.tmp$PROFILE_CODE, levels=.ord.pro.c,
                                  ordered = TRUE))
  .tmp<- .tmp[order(.tmp$.index),]

  #plot.type control
  if(!plot.type %in% c(1,2)){
    stop("pls_plot_species: plot.type unknown, check ?pls_plot_profile",
         call. = FALSE)
  }

  .tmp <- subset(.tmp, SPECIES_NAME %in% my.species)

  ############################
  #type 1
  ############################
  if(plot.type==1){
    .mc <- if ("mod.col" %in% names(.x.args)) {
      .x.args$mod.col
    } else {
      "red"
    }
    plt <- list(x=pred~.value | SPECIES_NAME, data=.tmp,
                  #prepanel forces x and y lims to same range
                  prepanel=function(...){
                    .tmp <- prepanel.default.xyplot(...)
                    .tmp$xlim <- range(c(.tmp$xlim, .tmp$ylim))
                    .tmp$ylim <- .tmp$xlim
                    .tmp
                  },
                  panel= function(x, y, xlim, ylim, ...){
                    #user control of grid - like loa...
                    .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                  lattice::panel.grid, ...)
                    #TO DO
                    #user control of y=x
                    panel.ablineq(a = 0, b = 1, adj = c(0,1),
                                  col.line ="grey", lty=2, label="")
                    #user control of main plotted data via
                    # standard lattice
                    panel.xyplot(x=x, y=y,  ...)
                    #CURRENTLY JUST col via mod.col
                    #user control of model
                    panel.ablineq(lm(y ~ x + 0), cex = 0.8,
                                  x = min(c(x, y), na.rm=TRUE),
                                  y = max(c(x, y), na.rm=TRUE),
                                  r.squared = TRUE, adj = c(0,1),
                                  sep = " (", sep.end = ")",
                                  offset=0, varStyle = NULL,
                                  col.line = .mc, col.text = .mc, digits = 2)
                  },
                  xlab="Measurement", ylab="model",
                  scales=list(y=list(relation="free",
                                     rot=90),
                              x=list(relation="free")))
    plt <- modifyList(plt, .x.args)
    p <- do.call(xyplot, plt)

  }
  if(plot.type==2){
    #xlab
    if(!"xlab" %in% names(.x.args)){
      .x.args$xlab <- "Sample [index]"
    }
    if(!"ylab" %in% names(.x.args)){
      .x.args$ylab <- "Measurement, Model"
    } else {
      if(length(.x.args$ylab)>1){
        if(!"key.text" %in% names(.x.args)){
          .x.args$key.text <- .x.args$ylab[1:2]
        }
        .x.args$ylab <- paste(.x.args$ylab[1], .x.args$ylab[2], sep =", ")
      }
    }
    if(!"key.text" %in% names(.x.args)){
      .x.args$key.text <- c("Measurement", "Model")
    }
    if(!"col" %in% names(.x.args)){
      .x.args$col <- trellis.par.get("superpose.line")$col[1:2]
    }
    if("mod.col" %in% names(.x.args)){
      .x.args$col <- c(.x.args$col[1], .x.args$mod.col)
    }
    p2.ls <- list(x= .value + pred ~ .index | SPECIES_NAME, data=.tmp,
                  auto.key = list(text=.x.args$key.text,
                                  space="top", columns=2),
                  type="l",
                  panel= function(...){
                    .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                  panel.grid, ...)
                    lattice::panel.xyplot(...)
                  },
                  scale=list(relation="free"),
                  par.settings = simpleTheme(col=.x.args$col))
    p2.ls <- modifyList(p2.ls, .x.args)
    p <- do.call(xyplot, p2.ls)
    ##plot(p)

  }

  #output
  ############################
  #this needs working up based on input from Dennis...
  ## plot(p)
  ## return(invisible(.tmp))
  .rsp_plot_output(as.data.frame(.tmp), list(p2.ls=plt, p2.ls=p2.ls), p, output)

}














######################################
######################################
## unexported
######################################
######################################

#old
#  holding until new versions are fully up and running...



## pls_plot.old

############################
# currently not exporting...
############################

# #' @rdname rsp.pls.plot
# #' @export

##   now imports via data.table::
##        need this to kill the as.data.table load message
##   #' @import data.table
##

#############################
#this needs a lot of work
#############################

# this uses unexported rsp_profile_pie function below...
#     both pls_plot and rsp_profile_pie need work...


pls_plot.old <- function (pls, n, plot.type = 1, ...){

  #current using lattice/latticeExtra for the panelling/layers...

  #basic plots finished but...
  #    currently not passing arguments generally
  #    the par setting seem to be dropped when using plot(p)
  #         ahead of end of function

  ############################
  # nags
  ############################

  # type = 1
  ############################

  # note sure about the layer naming
  # zero is not bottom of barchart...

  # type = 2
  ############################

  # the label positioning is messy (see not about nudge)

  # cex setting too small if only one panel...

  #wondering about
  #    https://latticeextra.r-forge.r-project.org/man/postdoc.html
  #    as an alternative to type=2
  #    (but 100 percent measured rather than proportion...)

  #################
  #setup
  #################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- .rsp_profile_code_order(dat)
  #dat$SPECIES_NAME <- .rsp_tidy_species_name(dat$SPECIES_NAME)
  .sp.ref <- unique(dat$SPECIES_NAME)
  #species
  #  now defaulting to all plots
  species <- if (missing(n)) {
    species <- .sp.ref
  }
  else {
    n
  }
  if (is.numeric(species)) {
    if (all(species == -1)) {
      species <- .sp.ref
    }
    else {
      species <- .sp.ref[species]
    }
  }
  if (!any(species %in% .sp.ref)) {
    stop("RSP_PLS> unknown species, please check", call. = FALSE)
  }
  ################################
  #note:
  #  could condition here BUT currently
  #  holding on to everything until just before plot
  #  might not need to do this....
  #################################

  .sp.ord <- unique(dat$SPECIES_ID)
  #####################################
  #messy at moment...
  .sp.m.pro <- names(dat)[grep("^.n_", names(dat))]
  .sp.pro <- gsub("^.n_", "", .sp.m.pro)

  #line col....
  .col <- lattice::trellis.par.get("superpose.line")$col[1]

  #bar cols
  .cols <- if ("col" %in% names(.x.args)) {
    #could include if you supply a function..?
    #could use col.regions?
    .cols <- .x.args$col
  }
  else {
    .cols <- heat.colors(n = length(.sp.m.pro))
  }
  if (length(.cols) != length(.sp.m.pro)) {
    stop("pls_plot> halted; expecting ", length(.sp.m.pro),
         "colours; given ", length(.cols), sep = "", call. = FALSE)
  }

  ######################
  # build x_[profile]
  ######################
  #now done in pls_report
  for (i in .sp.pro) {
    dat[, paste(".x_", i, sep = "")] <- dat[, paste(".m_", i, sep = "")] *
      dat[, paste(".n_", i, sep = "")]
  }
  .sp.x.pro <- names(dat)[grep("^.x_", names(dat))]
  .rep <- dat[c("SPECIES_NAME", "SPECIES_ID", "PROFILE_CODE",
                .sp.x.pro)]
  .rep <- data.table::melt(data.table::as.data.table(.rep),
                           id = c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE"))
  .tot <- data.table::as.data.table(dat)
  .cs <- c(".value", "pred", .sp.x.pro)
  .tot <- .tot[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
               .SDcols = .cs, by = c("SPECIES_ID", "SPECIES_NAME")]

  ###########################
  # now plotting as panels
  # using
  ###########################

  ######################################################
  # now using rsp_ function to track all pls model cases
  # previous method only tracked valid cases for the plotted data
  #    so no gaps where models dropped/not built...
  #########################################################
  .rep$.index <- as.numeric(factor(.rep$PROFILE_CODE, levels = .ord.pro.c,
                                   ordered = TRUE))
  dat$.index <- as.numeric(factor(dat$PROFILE_CODE, levels = .ord.pro.c,
                                  ordered = TRUE))

  .tmp <- dat[c("SPECIES_ID", "PROFILE_CODE", ".index", ".value", "pred")]
  .rep <- data.table::merge.data.table(.rep, .tmp)

  .rep$variable <- gsub("^x_", "", .rep$variable)

  #print(names(.rep))
  #return(dat)

  .rep <- subset(as.data.frame(.rep), SPECIES_NAME %in% species)

  if (1 %in% plot.type) {

    #lattice sets panel order based
    .sp <- if(is.factor(.rep$SPECIES_NAME)){
      levels(.rep$SPECIES_NAME)
    } else {
      sort(unique(.rep$SPECIES_NAME))
    }
    .sp <- .sp[.sp %in% .rep$SPECIES_NAME]
    #.y.scale <- lapply(unique(.rep$SPECIES_NAME), function(x){
    .y.scale <- lapply(.sp, function(x){
      .tmp <- .rep[.rep$SPECIES_NAME==x,]
      c(0, max(c(.tmp$.value, .tmp$pred), na.rm=TRUE))
    })
    ###############################################
    #use loa method to generalise this?
    ###############################################

    p2 <- lattice::xyplot(.value ~ .index | SPECIES_NAME, .rep,
                          panel=lattice::panel.xyplot,
                          type="s", xlab="Sample [index]",
                          ylab="Measurement",
                          scales=list(relation="free"),
                          ylim=.y.scale)

    p <- lattice::barchart(value ~ factor(.index) | SPECIES_NAME, .rep,
                           groups=.rep$variable, stack=TRUE,
                           panel=function(x, y, col, groups, ..., subscripts){
                             #grid control like loa
                             .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                           lattice::panel.grid, ...)
                             lattice::panel.barchart(x=x, y=y, col=col,
                                                     groups=groups,
                                                     subscripts=subscripts, ...)
                             .y <- .rep$.value[subscripts]
                             #col needs to be from option[1]
                             lattice::panel.xyplot(x=x, y=.y,
                                                   col=.col,
                                                   type="l",
                                                   subscripts=subscripts,...)
                           },
                           scales=list(relation="free"),
                           #auto.key=list(space="top", columns=2,
                           #               col.line=.cols,
                           #              points=FALSE, rectangles=TRUE),
                           ylim=.y.scale,
                           col=.cols,
                           border=NA,
                           #par.settings = list(superpose.polygon = list(col = .cols,
                           #                          pch =c (15, 15)),
                           #superpose.symbol = list(fill = .cols)),
                           auto.key=list(space="top", columns = 3,
                                         cex=0.8,
                                         points=FALSE,
                                         rectangles=TRUE)) #,
    #xscale.components = function(lim,...){
    #  lim <- as.numeric(as.character(lim))
    #  ans <- lattice::xscale.components.default(lim=lim,...)
    #  print(ans)
    #  ans
    #})
    plot(update(latticeExtra::doubleYScale(p2, p, add.axis = FALSE),
                par.settings = list(superpose.polygon = list(col = .cols),
                                    superpose.symbol = list(fill = .cols))))

    #p2 <- lattice::xyplot(.value ~ factor(.index) | SPECIES_NAME, dat,
    #                      type="l", scales=list(relation="free"))
    #plot(cheat(p, latticeExtra::as.layer(p2)))

    #plot(latticeExtra::doubleYScale(p, p2, add.axis=FALSE, add.ylab2=FALSE))
  }
  if (2 %in% plot.type) {


    p <- lattice::xyplot(value ~ .index | SPECIES_NAME, .rep,
                         groups=.rep$variable,
                         totals=.rep$.value,
                         scales=list(relation="free",
                                     draw=FALSE),
                         ylab="", xlab="",
                         col = .cols,
                         auto.key=list(space="top", columns = 3,
                                       cex=0.8,
                                       points=FALSE,
                                       rectangles=TRUE),
                         ylim=c(-2,2), xlim=c(-2,2),
                         between = list(x = 0.2, y = 0.2),
                         panel=.rsp_panel.pie,
                         par.settings = list(superpose.polygon = list(col = .cols),
                                             axis.line = list(col = 'transparent'),
                                             superpose.symbol = list(fill = .cols))
    )
    plot(p)

  }
  invisible(.rep)
}




########################
#currently not exporting
########################


pls_plot_profile.old <- function (pls, n, log = FALSE, ...)
{
  #########################
  #previous plot used base r graphics
  #this moved to lattice/latticeExtra
  #so we can panel outputs
  #########################

  #setup
  .x.args <- list(...)
  .plt.args <- .x.args[names(.x.args %in% c())]
  dat <- pls_report(pls)
  .sp.ord <- unique(dat$SPECIES_ID)
  .sp.m.pro <- names(dat)[grep("^.m_", names(dat))]
  .sp.pro <- gsub("^.m_", "", .sp.m.pro)
  #defaulting n to all profiles as one plot
  profile <- if (missing(n)) {
    profile <- .sp.pro
  } else {
    n
  }
  if (is.numeric(profile)) {
    if (all(profile == -1)) {
      profile <- .sp.pro
    }
    else {
      profile <- .sp.pro[profile]
    }
  }
  if (!any(profile %in% .sp.pro)) {
    stop("RSP_PLS> unknown profile(s), please check", call. = FALSE)
  }

  #########################
  #build x_[profile]
  #########################
  n_profile <- paste(".n_", profile, sep = "")
  m_profile <- paste(".m_", profile, sep = "")
  dat <- dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE",
               n_profile, m_profile, "pred", ".value")]
  for (i in profile) {
    dat[, paste(".x_", i, sep = "")] <- dat[, paste(".m_", i, sep = "")] *
      dat[, paste(".n_", i, sep = "")]
  }

  .rep <- data.table::as.data.table(dat)
  .cols <- c(".value", "pred", paste(".x_", profile, sep = ""))
  .rep <- .rep[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
               .SDcols = .cols, by = c("SPECIES_ID", "SPECIES_NAME")]
  .rep <- as.data.frame(.rep)

  #########################
  # y2 setup
  #########################
  # by default this is .value
  #   but might want mod prediction
  if ("y2" %in% names(.x.args) && .x.args$y2 == "pred") {
    for (i in profile) {
      .rep[, paste("pc_", i, sep = "")] <-
        (.rep[, paste(".x_", i, sep = "")]/.rep$pred) * 100
    }
  }
  else {
    for (i in profile) {
      .rep[, paste("pc_", i, sep = "")] <-
        (.rep[, paste(".x_", i, sep = "")]/.rep$.value) * 100
    }
  }
  #might not need all of following now we
  #we are not pulling apart to plot one at time...
  dat <- dat[!duplicated(dat$SPECIES_NAME), ]
  dat$PROFILE_NAME <- dat$PROFILE_NAME[1]
  dat$PROFILE_CODE <- dat$PROFILE_CODE[1]
  dat <- merge(.rep, dat[c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE",
                           m_profile)], )
  dat <- dat[order(ordered(dat$SPECIES_ID, levels = .sp.ord)), ]


  ################################
  # build pc_[profile]
  ################################
  rownames(dat) <- 1:nrow(dat)
  .ref <- names(dat)[grep("pc_", names(dat))]
  .oth <- c("SPECIES_ID", "SPECIES_NAME", "PROFILE_CODE", ".value", "pred")
  .temp <- data.table::as.data.table(dat[c(.oth, gsub("^pc_", ".x_", .ref))])
  .d1 <- data.table::melt(.temp, measure.vars = gsub("^pc_", ".x_", .ref),
                          variable.name = "pls_profile", value.name = "loading")
  .temp <- data.table::as.data.table(dat[c(.oth, .ref)])
  .d2 <- data.table::melt(.temp, measure.vars = .ref,
                          variable.name = "pls_profile", value.name = "percent_contr")
  .d2$pls_profile <- gsub("^pc_", ".x_", .d2$pls_profile)
  dat <- as.data.frame(merge(.d1, .d2, all=T))
  #############################

  ############################
  #now using lattice to handle logs
  ###############
  #.dat <- dat
  #don't need local version of dat because not changing data ahead of plot
  #if(log){
  #  .dat$loading <- log10(.dat$loading)
  #  .ylim <- lapply(profile, function(x){
  #    .temp <- subset(.dat, pls_profile==x)
  #    .temp <- range(.temp$loading, na.rm=TRUE, finite=TRUE)
  #    if(.temp[1] == .temp[2]){
  #      .temp <- c(.temp[1]-1, .temp[1]+1)
  #    }
  #    range(c(floor(.temp), ceiling(.temp)))
  #  })
  #} else {
  #  .ylim <- lapply(profile, function(x){
  #    .temp <- subset(.dat, pls_profile==x)
  #    .temp <- range(.temp$loading, na.rm=TRUE, finite=TRUE)
  #    range(pretty(.temp))
  #  })
  #}

  ######################
  #plot
  ######################
  #now using lattice/latticeExtra
  ##
  #think there is more here that can be generalized...
  p1.ls <- list(x = loading~SPECIES_NAME | pls_profile,
                data=dat, ylab="Source Loading",
                panel = function(...){
                  .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                panel.grid, ...)
                  panel.barchart(...)
                },
                between=list(y=.2),
                scales=list(x=list(rot=90),
                            y=list(rot=c(0,90),
                                   relation="free")),
                layout=c(1,length(profile)))
  if(log){
    p1.ls$scales$y$log=10
    p1.ls$yscale.components <- .rsp_yscale.component.log10
  }
  p1.ls <- modifyList(p1.ls, .x.args)
  if(!"col" %in% names(p1.ls)){
    p1.ls$col <- trellis.par.get("superpose.line")$col[1]
  }
  p1 <- do.call(barchart, p1.ls)
  if("mod" %in% names(.x.args) && !.x.args$mod){
    #if mod FALSE just plot 1
    plot(p1)
  } else {
    #add mod layer (total contributions) as y2
    .col2 <- if("mod.col" %in% names(.x.args)){
      .x.args$mod.col
    } else {
      trellis.par.get("superpose.line")$col[2]
    }
    p2.ls <- list(x = percent_contr ~ factor(SPECIES_NAME) | pls_profile,
                  pch=16, type=c("h", "p"), col= c(.col2, .col2),
                  ylab="Total Contribution (%)",
                  data=dat)
    .tmp <- .x.args[grepl("^mod[.]", names(.x.args))]
    if(length(.tmp)>0){
      names(.tmp) <- gsub("^mod[.]", "", names(.tmp))
      p2.ls <- modifyList(p2.ls, .tmp)
    }
    p2 <- do.call(xyplot, p2.ls)
    plot(update(doubleYScale(p1, p2, add.ylab2 = TRUE),
                par.settings = simpleTheme(col=c(p1.ls$col[1], .col2))))
  }

  ############
  #output
  ############
  #could pass plot and data as list???
  return(invisible(dat))
}



## #' @rdname rsp.pls.plot
## #' @export

##   now imports from xxx.r
##   #' @import data.table

#############################
#this needs a lot of work
#############################

#currently not exporting

pls_plot_species.old <- function (pls, id, plot.type = 1, ...)
{

  #not including NAs....

  ###########################
  # setup
  ###########################
  .x.args <- list(...)
  dat <- pls_report(pls)
  .ord.pro.c <- .rsp_profile_code_order(dat)
  .sp.ref <- unique(dat$SPECIES_NAME)
  species <- if (missing(id)) {
    .sp.ref
    #default option (print the lot...)
    ############################
    #possibly a warning if lots of species to plot
    ##################
  } else {
    id
  }
  if (is.numeric(species)) {
    if (all(species == -1)) {
      species <- .sp.ref
    }
    else {
      species <- .sp.ref[species]
    }
  }
  if (!any(species %in% .sp.ref)) {
    stop("RSP_PLS> unknown species, please check", call. = FALSE)
  }
  ############################
  #if not earlier, then here?
  #possibly a warning if lots of species to plot
  ##################

  #########################
  #could drop a lot of this??
  #########################
  .xlb <- if ("xlab" %in% names(.x.args)) {
    .x.args$xlab
  } else {
    "Measurement"
  }
  .ylb <- if ("ylab" %in% names(.x.args)) {
    .x.args$ylab
  } else {
    "Model"
  }
  .bc <- if ("col" %in% names(.x.args)) {
    .x.args$col
  } else {
    par("col")
  }
  .mc <- if ("mod.col" %in% names(.x.args)) {
    .x.args$mod.col
  } else {
    "red"
  }
  dat <- subset(dat, SPECIES_NAME %in% species)
  #    lims <- range(c(d2$.value, d2$pred), na.rm = TRUE, finite = TRUE)
  #    mod <- lm(pred ~ 0 + .value, d2)
  #    .sum <- paste("y = ", signif(summary(mod)$coefficients[1,
  #        1], 3), "x (adj.R2 = ", signif(summary(mod)$adj.r.squared,
  #        3), ")", sep = "")
  .lims <- lapply(species, function(x){
    .d <- subset(dat, SPECIES_NAME==x)
    range(c(.d$pred, .d$.value), finite=TRUE, na.rm=TRUE)
  })
  if (1 %in% plot.type) {
    .mc <- if ("mod.col" %in% names(.x.args)) {
      .x.args$mod.col
    } else {
      "red"
    }
    p1.ls <- list(x=pred~.value | SPECIES_NAME, data=dat,
                  #prepanel forces x and y lims to same range
                  prepanel=function(...){
                    .tmp <- prepanel.default.xyplot(...)
                    .tmp$xlim <- range(c(.tmp$xlim, .tmp$ylim))
                    .tmp$ylim <- .tmp$xlim
                    .tmp
                  },
                  panel= function(x, y, xlim, ylim, ...){
                    #user control of grid - like loa...
                    .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                  lattice::panel.grid, ...)
                    #TO DO
                    #user control of y=x
                    panel.ablineq(a = 0, b = 1, adj = c(0,1),
                                  col.line ="grey", lty=2, label="")
                    #user control of main plotted data via
                    # standard lattice
                    panel.xyplot(x=x, y=y,  ...)
                    #CURRENTLY JUST col via mod.col
                    #user control of model
                    panel.ablineq(lm(y ~ x + 0), cex = 0.8,
                                  x = min(c(x, y), na.rm=TRUE),
                                  y = max(c(x, y), na.rm=TRUE),
                                  r.squared = TRUE, adj = c(0,1),
                                  sep = " (", sep.end = ")",
                                  offset=0, varStyle = NULL,
                                  col.line = .mc, col.text = .mc, digits = 2)
                  },
                  xlab="Measurement", ylab="model",
                  scales=list(y=list(relation="free",
                                     rot=90),
                              x=list(relation="free")))
    p1.ls <- modifyList(p1.ls, .x.args)
    p <- do.call(xyplot, p1.ls)
    plot(p)

    #      plot(d2$.value, d2$pred, type = "n", main = i, col = .bc,
    #          xlab = .xlb, ylab = .ylb, xlim = lims, ylim = lims)
    #      grid()
    #      abline(a = 0, b = 1, col = "grey")
    #      points(d2$.value, d2$pred)
    #      abline(mod, col = .mc, lty = 2)
    #      text(lims[1], lims[2], .sum, adj = c(0, 1), cex = 0.75)
  }
  if (2 %in% plot.type) {
    #xlab
    if(!"xlab" %in% names(.x.args)){
      .x.args$xlab <- "Sample [index]"
    }
    if(!"ylab" %in% names(.x.args)){
      .x.args$ylab <- "Measurement, Model"
    } else {
      if(length(.x.args$ylab)>1){
        if(!"key.text" %in% names(.x.args)){
          .x.args$key.text <- .x.args$ylab[1:2]
        }
        .x.args$ylab <- paste(.x.args$ylab[1], .x.args$ylab[2], sep =", ")
      }
    }
    if(!"key.text" %in% names(.x.args)){
      .x.args$key.text <- c("Measurement", "Model")
    }
    if(!"col" %in% names(.x.args)){
      .x.args$col <- trellis.par.get("superpose.line")$col[1:2]
    }
    if("mod.col" %in% names(.x.args)){
      .x.args$col <- c(.x.args$col[1], .x.args$mod.col)
    }


    #ylab
    #can to two terms for

    #if("ylab" %in% names(.x.args)){
    #  if(length(.x.args$ylab)>1){
    #    if(!"key.text" %in% names(.x.args)){
    #      .x.args$key.text <- .x.args$ylab[1:2]
    #    }
    #    .x.args$ylab <- paste(.x.args$ylab[1], .x.args$ylab[2], sep =", ")
    #  } else {
    #    if(!"key.text" %in% names(.x.args)){
    #      .x.args$key.text <- c("Measurement", "Model")
    #    }
    #  }
    #} else {
    #  if(!"key.text" %in% names(.x.args)){
    #    .x.args$key.text <- c("Measurement", "Model")
    #  }
    #  .x.args$ylab <- "Measurement, Model"
    #}




    #########################
    #previous code
    #########################
    #plot(d2$.value, type = "n", main = i, col = .bc,
    #    ylab = .ylb, xlab = .xlb, ylim = lims)
    #lines(d2$.value)
    #lines(d2$pred, col = .mc)
    ########################
    #using standardised index
    #make 'ordered profile codes' at top
    #      before any subsetting...
    #      .ord.pro.c <- rsp_profile_code_order(dat)
    dat$.index <- as.numeric(factor(dat$PROFILE_CODE, levels=.ord.pro.c,
                                    ordered = TRUE))
    dat<- dat[order(dat$.index),]
    p2.ls <- list(x= .value + pred ~ .index | SPECIES_NAME, data=dat,
                  auto.key = list(text=.x.args$key.text,
                                  space="top", columns=2),
                  type="l",
                  panel= function(...){
                    .rsp_panelPal("grid", list(h=-1,v=-1, col="grey", lty=3),
                                  panel.grid, ...)
                    lattice::panel.xyplot(...)
                  },
                  scale=list(relation="free"),
                  par.settings = simpleTheme(col=.x.args$col))
    p2.ls <- modifyList(p2.ls, .x.args)
    p <- do.call(xyplot, p2.ls)
    plot(p)
    ######################

    #    or any with any missing are plot on different x scale
    #    maybe find longest, take range for that
    #xyplot(.value + .pred ~ )
  }

  invisible(dat)
}











