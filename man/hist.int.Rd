\name{hist.int}
\alias{hist.int}
\title{Histogram of integers
Plot a histogram of the integers in \code{x}. Useful if \code{x} is
 a vector of counts or integers.
It essentially is a smart \code{\link{hist}} that works out where to place the
breaks so that you can see the counts at each integer in \code{x}}
\usage{
  hist.int(x, xlab = "counts", col = NULL,
    add.density = FALSE, density.adjust = 7, ...)
}
\arguments{
  \item{x}{an integer vector}

  \item{xlab}{the x-axis title}

  \item{col}{a colour to be used to fill the bars.  The
  default of \code{NULL} yields unfilled bars.}

  \item{add.density}{logical: add a density trace over the
  top?}

  \item{density.adjust}{see \code{adjust} parameter to
  \code{\link{density}}}

  \item{\dots}{arguments passed to \code{\link{hist}}}
}
\description{
  Histogram of integers Plot a histogram of the integers in
  \code{x}. Useful if \code{x} is a vector of counts or
  integers. It essentially is a smart \code{\link{hist}}
  that works out where to place the breaks so that you can
  see the counts at each integer in \code{x}
}
\author{
  Mark Cowley, 16 May 2006
}

