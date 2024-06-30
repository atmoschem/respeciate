#' @name rsp.id
#' @title rsp_id_ functions to identify common species groups for grouping and
#' subsetting respeciate profiles
#' @aliases rsp_id rsp_id_copy rsp_id_nalkane rsp_id_btex rsp_id_pah16
#' @description \code{rsp_id_} functions generate a vector of assignment
#' terms and can be used to subset or condition a supplied (re)SPECIATE
#' \code{data.frame}.
#'
#' Most commonly, the \code{rsp_id_} functions accept a single input, a
#' respeciate \code{data.frame} and return a logical vector of
#' length \code{nrow(x)}, identifying species of interest as
#' \code{TRUE}. So, for example, they can be used when
#' \code{\link{subset}}ting in the form:
#'
#' \code{subset(rsp, rsp_id_nalkane(rsp))}
#'
#' ... to extract just n-alkane records from a supplied \code{respeciate}
#' object \code{rsp}.
#'
#' However, some accept additional arguments. For example, \code{rsp_id_copy}
#' also accepts a reference data set, \code{ref}, and a column identifier,
#' \code{by}, and tests \code{rsp$by \%in\% unique(ref$by)}.
#'
#' @param rsp a \code{respeciate} object, a \code{data.frame} of respeciate
#' profiles.
#' @param ref (\code{rsp_id_copy} only) a second \code{respeciate} object, to
#' be used as reference when subsetting (or conditioning) \code{rsp}.
#' @param by (\code{rsp_id_copy} only) character, the name of the column
#' in \code{ref} to copy when subsetting (or conditioning) \code{rsp}.
#' @return \code{rsp_id_copy} outputs can be modified but, by default, it
#' identifies all species in the supplied reference data set.
#'
#' \code{rsp_id_nalkane} identifies (straight chain) C1 to C40 n-alkanes.
#'
#' \code{rsp_id_btex} identifies the BTEX group of aromatic hydrocarbons
#' (benzene, toluene, ethyl benzene, and M-, O- and P-xylene).

#############################
#NOTE
############################

# v 0.3.1
# changed these from rsp_x_[whatever] to rsp_id_[whatever]
#      because overlap with rsp_x object class could complicate things...

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
#     see rsp_id_nalkane where some species have two names in SPECIATE.

#################################
# rsp_id_copy
#################################

# identify species in rsp that are in ref(erence)

# special cases???
#     rsp_id_ref(rsp, ref, by="")
#           where rsp is respeciate object, ref is a reference
#           by is column in ref; case is x$by %in% unique(ref$by)
#              could ref also be a vector of terms???

#' @rdname rsp.id
#' @export

rsp_id_copy <- function(rsp, ref = NULL, by=".species.id"){

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
#rsp_id_nalkanes
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
## b <- subset(a, rsp_id_nalkane(a))
## b[order(b$SPEC_MW),]

#' @rdname rsp.id
#' @export

rsp_id_nalkane <- function(rsp){

  #group x by is/isn't n-alkane
  tolower(rsp$.species) %in% c("methane",            #C1
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
#rsp_id_btex
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
## b <- subset(a, rsp_id_btex(a))
## b[order(b$SPEC_MW),]

## to do

#' @rdname rsp.id
#' @export

rsp_id_btex <- function(rsp){

  #identify species that is a btex
  #might need to think about mixtures...
  #    for example all xylenes or all c2 benzenes, etc...
  tolower(rsp$.species) %in% c(
    #to test/check...
    "benzene",
    "toluene",
    "ethylbenzene",
    "m-xylene",
    "o-xylene",
    "p-xylene"
  )
}


#' @rdname rsp.id
#' @export

rsp_id_pah16 <- function(rsp){

  #identify species that are on epa 16 priority pahs list
  # but also several are on other list
  # https://uk-air.defra.gov.uk/assets/documents/reports/cat08/0512011419_REPFIN_all_nov.pdf
  # (above is UK document, will be something more driection from EPA)

  #Naphthalene
  #Acenaphthylene
  #Acenaphthene
  #Fluorene
  #Anthracene
  #Phenanthrene
  #Fluoranthene
  #Pyrene
  #Benz[a]anthracene
  #Chrysene
  #Benzo[b]fluoranthene
  #Benzo[k]fluoranthene
  #Benzo[a]pyrene
  #Dibenz[ah]anthracene
  #Indeno[123cd]pyrene
  #Benzo[ghi]perylene

    tolower(rsp$.species.id) %in% c(
    #testing use of .species.id
      "611",               #Naphthalene 91-20-3
      "847",               #Acenaphthylene 208-96-8
      "846",               #Acenaphthene  83-32-9
      "883",               #Fluorene 86-73-7
      "852",               #Anthracene 120-12-7
      "902",               #Phenanthrene 85-01-8
      "882",               #Fluoranthene 206-44-0
      "904",               #Pyrene 129-00-0
      "854",               #Benz[a]anthracene, BUT as Benz(a)anthracene 56-55-3
      "867",               #Chrysene 218-01-9
      "1171",              #Benzo[b]fluoranthene 205-99-2
      "1610",              #Benzo[k]fluoranthene 207-08-9
      "855",               #Benzo[a]pyrene 50-32-8
      "1848",              #Dibenz[ah]anthracene as Dibenz[a,h]anthracene
                           #      but also in mixtures in SPECIATE
      "884",               #Indeno[123cd]pyrene as Indeno[1,2,3-cd]pyrene 193-39-5
      "858"                #Benzo[ghi]perylene as Benzo(ghi)perylene 191-24-2
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
