\name{barplot.stratum}
\alias{barplot.stratum}
\title{Make a barplot of a binary variable, stratified by a certain function, like
which.percentile, or which.quantile.}
\usage{
  barplot.stratum(binary, strata, proportion = TRUE, ...)
}
\arguments{
  \item{binary}{a boolean vector}

  \item{strata}{an additional vector that describes which
  strata each point came from, such as which percentile.
  see \code{\link[mjcbase]{which.percentile}}}

  \item{proportion}{logical: y-axis as a proportion?
  Recommended to be TRUE}

  \item{\dots}{arguments passed to
  \code{\link[graphics]{barplot}}}
}
\value{
  none. makes a plot
}
\description{
  Make a barplot of a binary variable, stratified by a
  certain function, like which.percentile, or
  which.quantile.
}
\author{
  Mark Cowley, 2008-12-22
}

