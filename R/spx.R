#' @name spx
#' @title spx_ functions for grouping and subsetting
#' @aliases spx_n_alkane spx_btex


#' @description \code{spx_} functions generate a vector of assignment
#' terms and can be used to subset or condition a supplied
#' \code{respeciate} object. Most commonly, the \code{spx_} functions
#' return a logical vector of length = nrow(x), identifying
#' species of interest as \code{TRUE}. For example:
#'
#' \code{subset(x, spx_n_alkane(x))}
#'
#' ... to extract just n-alkane records from \code{respeciate} object
#' \code{x}.
#'
#' @param x a \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
#' profiles.
#' @return \code{spx_n_alkane} identifies C1 to C40 n-alkanes.
#'
#' \code{spx_btex} identifies the BTEX group of aromatic hydrocarbons
#' (benzene, toulene, ethylbenzene, and M-, O- and P-xylene).

#############################
#NOTE
############################

#still not sure this is best way of doing this
#   but it did not seem to be slowing things down
#   and other approaches seems likely to got messy
#   really quick...

# others to do????

# the BTEXs  - doing

# others to consider???
#     PAHs different groups
#     VOCs HC network.
#     elementals???
#     monitoring network relevant subsets of species






#####################
#spx_n_alkanes
#######################


#source names
# from https://en.wikipedia.org/wiki/List_of_straight-chain_alkanes
# (might be duplicates)
# (some not using standard names)

# could try smiles, molecular formula, cas numbers???
# should be one entry/species if they are unique???

#test
## a <- sysdata$SPECIES_PROPERTIES
## b <- subset(a, spx_n_alkane(a))
## b[order(b$SPEC_MW),]

#' @rdname spx
#' @export

spx_n_alkane <- function(x){
  #group x by is/isn't n-alkane
  tolower(x$SPECIES_NAME) %in% c("methane",            #C1
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
#spx_n_btex
#######################

#  Benzene, toluene, ethylbenzene, 3 xylenes isomers

#ref
#https://www.eea.europa.eu/help/glossary/eper-chemicals-glossary/benzene-toluene-ethylbenzene-xylenes-as-btex
#   like better...

# also have btx group which is same minus ethylbenzene...

#notes
#####################

#might need more thinking about
#    ethyl benzene and xylenes might have several names
#    subset(a, a$Molecular.Formula=="C8H10")

#    are xylenes in as 1,2-, 1,3- and 1,4-dimethyl benzene???
#    are spaces an issue???


#    if several names for btex, might need to think about how to
#    sample (CAS? etc), merge and compare???


#tests
#########################
## a <- sysdata$SPECIES_PROPERTIES
## b <- subset(a, spx_btex(a))
## b[order(b$SPEC_MW),]

## to do

#' @rdname spx
#' @export

spx_btex <- function(x){
  #group x by is/isn't btex
  tolower(x$SPECIES_NAME) %in% c(
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
