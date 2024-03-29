% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp.rescale.R
\name{sp.rescale}
\alias{sp.rescale}
\alias{sp_rescale}
\alias{sp_rescale_profile}
\alias{sp_rescale_species}
\title{(re)SPECIATE profile rescaling functions}
\usage{
sp_rescale(x, method = 2, by = "species")

sp_rescale_profile(x, method = 1, by = "profile")

sp_rescale_species(x, method = 2, by = "species")
}
\arguments{
\item{x}{A \code{respeciate} object, a \code{data.frame} of re(SPECIATE)
profiles.}

\item{method}{numeric, the rescaling method to apply:
  1 \code{x/total(x)};
  2 \code{x/mean(x)};
  3 \code{x-min(x)/max(x)-min(x)};
  4 \code{x-mean(x)/sd(x)};
  5 \code{x/max(x)}.
The alternative 0 returns the records to their original
values.}

\item{by}{character, when rescaling \code{x} with
\code{\link{sp_rescale}}, the data type to group and rescale,
currently \code{'species'} (default) or \code{'profile'}.}
}
\value{
\code{sp_rescale} and \code{sp_rescale} return the
\code{respeciate} profile with the percentage weight records rescaled using
the requested method. See Note.
}
\description{
Functions for rescaling

\code{sp_rescale} rescales the percentage weight records in
a supplied (re)SPECIATE profile data set. This can be by profile or species
subsets, and \code{sp_rescale_profile} and \code{sp_rescale_species} provide
short-cuts to these options.
}
\note{
Data sometimes needs to be normalised, e.g. when applying some
statistical analyses. Rather than modify the EPA records in the
\code{WEIGHT_PERCENT} column, \code{respeciate} creates a duplicate column
\code{.value} which is modified by operations like \code{sp_rescale_profile}
and \code{sp_rescale_species}. This means rescaling is always applied to
the source information, rather than rescaling an already rescaled value,
and the EPA records are retained unaffected. So, the original source
information can be easily recovered.
}
\references{
Dowle M, Srinivasan A (2023). data.table: Extension of `data.frame`.
  R package version 1.14.8, \url{https://CRAN.R-project.org/package=data.table}.
}
