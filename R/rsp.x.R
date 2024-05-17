#' @name rsp.x
#' @title rsp_x_ functions for grouping and subsetting (re)SPECIATE profiles
#' @aliases rsp_x rsp_x_copy rsp_x_nalkane rsp_x_btex

# still wondering if these should be rsp_cut_...

#' @description \code{rsp_x_} functions generate a vector of assignment
#' terms and can be used to subset or condition a supplied (re)SPECIATE
#' \code{data.frame}.
#'
#' Most commonly, the \code{rsp_x_} functions accept a single input, a
#' (re)SPECIATE \code{data.frame} and return a logical vector of
#' length \code{nrow(x)}, identifying species of interest as
#' \code{TRUE}. So, for example, they can be used when
#' \code{\link{subset}}ting in the form:
#'
#' \code{subset(rsp, rsp_x_nalkane(rsp))}
#'
#' ... to extract just n-alkane records from a supplied \code{respeciate}
#' object \code{rsp}.
#'
#' However, some accept additional arguments. For example, \code{rsp_x_copy}
#' also accepts a reference data set, \code{ref}, and a column identifier,
#' \code{by}, and tests \code{rsp$by \%in\% unique(ref$by)}.
#'
#' @param rsp a \code{respeciate} object, a \code{data.frame} of (re)SPECIATE
#' profiles.
#' @param ref (\code{rsp_x_copy} only) a second \code{respeciate} object, to
#' be used as reference when subsetting (or conditioning) \code{rsp}.
#' @param by (\code{rsp_x_copy} only) character, the name of the column
#' in \code{ref} to copy when subsetting (or conditioning) \code{rsp}.
#' @return \code{rsp_x_copy} outputs can be modified but, by default, it
#' identifies all species in the supplied reference data set.
#'
#' \code{rsp_x_nalkane} identifies (straight chain) C1 to C40 n-alkanes.
#'
#' \code{rsp_x_btex} identifies the BTEX group of aromatic hydrocarbons
#' (benzene, toluene, ethyl benzene, and M-, O- and P-xylene).

#############################
#NOTE
############################

#still not sure this is best way of doing this
#   but it did not seem to be slowing things down
#   and other approaches seems likely to get messy
#   really quick...
#       tidyverse folks would argue against it...

# others to do????

# others to consider???

#     PAHs different groups
#     VOCs HC network.
#     elementals???
#     monitoring network relevant subsets of species

#do we need a strategy to rationalize multiple species names
#     see rsp_x_nalkane where some species have two names in SPECIATE.

#################################
# rsp_x_copy
#################################

# identify species in rsp that are in ref(erence)

# special cases???
#     rsp_x_ref(rsp, ref, by="")
#           where rsp is respeciate object, ref is a reference
#           by is column in ref; case is x$by %in% unique(ref$by)
#              could ref also be a vector of terms???

#' @rdname rsp.x
#' @export

rsp_x_copy <- function(rsp, ref = NULL, by="species_id"){

  #maybe warn???
  if(is.null(ref)){
    ref <- rsp
  }

  names(rsp) <- tolower(names(rsp))
  names(ref) <- tolower(names(ref))
  .tmp <- unique(ref[, by])

  rsp[, by] %in% .tmp
}


#####################
#rsp_x_nalkanes
#######################

# identify only the n-alkanes in rsp...

#source names
# from https://en.wikipedia.org/wiki/List_of_straight-chain_alkanes
# (might be duplicates)

# some in SPECIATE may not standard names...
#     need to check because I am not sure if standard names are international..

# some are just [alkane] rather than n-[alkane]
#     but not sure if any are in as both [alkane] and n-[alkane]

# could try smiles, molecular formula, cas numbers???
# should be one entry/species if they are unique???

#test
## a <- sysdata$SPECIES_PROPERTIES
## b <- subset(a, rsp_x_nalkane(a))
## b[order(b$SPEC_MW),]

#' @rdname rsp.x
#' @export

rsp_x_nalkane <- function(rsp){

  #group x by is/isn't n-alkane
  tolower(rsp$SPECIES_NAME) %in% c("methane",            #C1
                                 "ethane",
                                 "propane",
                                 "n-butane",
                                 "n-pentane",          #C5
                                 "n-hexane",
                                 "n-heptane",
                                 "n-octane",
                                 "n-nonane",
                                 "n-decane",           #C10
                                 "n-undecane",
                                 "n-dodecane",
                                 "n-tridecane",
                                 "n-tetradecane", "tetradecane",
                                 "n-pentadecane", "pentadecane",
                                 "n-hexadecane", "hexadecane",
                                 "n-heptadecane", "heptadecane",
                                 "n-octadecane", "octadecane",
                                 "n-nonadecane", "nonadecane",
                                 "n-icosane", "eicosane",         #C20
                                 "n-heneicosane",
                                 "n-docosane",
                                 "n-tricosane",
                                 "n-tetracosane",
                                 "n-pentacosane",      #C25
                                 "n-hexacosane",
                                 "n-heptacosane",
                                 "n-octacosane",
                                 "n-nonacosane",
                                 "n-triacontane",      #C30
                                 "n-hentriacontane",
                                 "n-dotriacontane", "dotriacontane",
                                 "n-tritriacontane",
                                 "n-tetratriacontane",
                                 "n-pentatriacontane", #C35
                                 "n-hexatriacontane",
                                 "n-heptatriacontane",
                                 "n-octatriacontane",
                                 "n-nonatriacontane",
                                 "n-tetracontane"      #C40
  )
}





#####################
#rsp_x_btex
#######################

#  Benzene, toluene, ethylbenzene, 3 xylenes isomers

#ref
#https://www.eea.europa.eu/help/glossary/eper-chemicals-glossary/benzene-toluene-ethylbenzene-xylenes-as-btex
#   like better...

# also have btx group which is same minus ethylbenzene...

#notes
#####################

#might need more thinking about

#    toluene is sometimes reported as a mixture of toulene and another compound
#    maybe not separated by GC???

#    ethyl benzene and xylenes might have several names
#    subset(a, a$Molecular.Formula=="C8H10")

#    are xylenes in as 1,2-, 1,3- and 1,4-dimethyl benzene???
#    are spaces an issue???

#    if several names for btex, might need to think about how to
#    sample (CAS? etc), merge and compare???

#tests
#########################
## a <- sysdata$SPECIES_PROPERTIES
## b <- subset(a, rsp_x_btex(a))
## b[order(b$SPEC_MW),]

## to do

#' @rdname rsp.x
#' @export

rsp_x_btex <- function(rsp){

  #identify species that is a btex
  #might need to think about mixtures...
  #    for example all xylenes or all c2 benzenes, etc...
  tolower(rsp$SPECIES_NAME) %in% c(
    #to test/check...
    "benzene",
    "toluene",
    "ethylbenzene",
    "m-xylene",
    "o-xylene",
    "p-xylene"
  )
}






##############################
#unexported, old, test notes
##############################

# other options seemed a lot of work...

#spx_n_alkane <- function(x){
#  x <- spg_n_alkane(x)
#  x <- x[x$spg_n_alkane,]
#  x[names(x) != "spg_n_alkane"]
#}


#subset. <- function(x, subset, select, ...){
#  mf <- match.call(expand.dots = FALSE)
#  m <- match(c("x", "subset", "select"), names(mf), 0L)
#  mf <- mf[c(1L, m)]
#  mf$drop.unused.levels <- TRUE
#  .x <- as.character(deparse(mf$x))
#  .subset <- as.character(deparse(mf$subset))
#  .select <- as.character(deparse(mf$select))
#  .subs <- strsplit(.subset, "&")[[1]]
#  for(i in 1:length(.subs)){
#    .i <- gsub(" ", "", .subs[i])
#    if(is.function(try(get(.i), silent=TRUE))){
#      .subs[i] <- paste(.i, "(", .x, ")", sep="")
#    }
#  }
#  .subs <- paste(.subs, collapse = " & ", sep="")
#
#
#print(.subs)
#print(.select)
#  .eq <- paste("subset(as.data.frame(", .x,
#               "), subset = ", .subs, ")",
#               sep="")
#  print(.eq)
#  #mf <- eval(mf, parent.frame())
#  rsp_build_respeciate(
#  eval(parse(text=.eq),
#       parent.frame()))
#
##    cl <- sys.call()
##    f <- get(as.character(cl[[1]]), mode="function", sys.frame(-1))
##    cl <- match.call(definition=f, call=cl)
##    x<- as.list(cl)[-1]
##    deparse(x$subset)
#
#}
