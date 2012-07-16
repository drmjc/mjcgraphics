\name{plot.venn}
\alias{plot.venn}
\title{Plot a Venn Diagram}
\usage{
  plot.venn(x, y = NULL, z = NULL, names = NULL,
    mar = par("mar"), population = NULL, ...)
}
\arguments{
  \item{x}{vectors of numerics or characters}

  \item{y}{vectors of numerics or characters}

  \item{z}{vectors of numerics or characters (can be NULL
  for a 2-way venn).}

  \item{names}{The names with which to label each of the
  circles}

  \item{mar}{the margin around the plot}

  \item{population}{The total set of values from which, x,
  y, and z are a subset if left null, then the population
  is assumed to be the set {x,y,z}}

  \item{\dots}{args passed onto vennDiagram2}
}
\description{
  Plot the overlap of 2 or 3 character vectors in a
  vennDiagram
}
\details{
  x,y,z can be character vectors, or numeric vectors, but
  should all be of the same type. note that decimal numbers
  are unsuitable.
}
\author{
  Mark Cowley, 20 June 2005
}

