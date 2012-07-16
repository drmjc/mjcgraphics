\name{plot.venn2D}
\alias{plot.venn2D}
\title{Plot a 2 way venn diagram}
\usage{
  plot.venn2D(x, y = NULL, names = NULL,
    mar = c(2, 2, 2, 2) + 0.2, population = NULL, ...)
}
\arguments{
  \item{x}{vectors of numerics or characters}

  \item{y}{vectors of numerics or characters}

  \item{names}{The names with which to label each of the
  circles}

  \item{mar}{the margin around the plot}

  \item{population}{The total set of values from which,
  \code{x}, and \code{y} are a subset. default=\code{NULL},
  where the population is assumed to be the set
  \code{{x,y}}}

  \item{\dots}{args passed onto vennDiagram2}
}
\description{
  Function to plot the overlap of 2 vectors in a
  vennDiagram \code{x},\code{y} can be character vectors,
  or numeric vectors, but should all be of the same type.
  note that decimal numbers are unsuitable.
}
\author{
  Mark Cowley
}

