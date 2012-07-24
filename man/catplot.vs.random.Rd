\name{catplot.vs.random}
\alias{catplot.vs.random}
\title{Permutation based CAT plots.}
\usage{
  catplot.vs.random(x, y, B = 100, sizes = 1:2000,
    sizes.random = NULL, ylim = c(0, 1),
    proportion = FALSE, random.col = "lightgrey",
    add = FALSE, col = 1, randomise = "y", pch = 1,
    main = "CAT plot", ...)
}
\arguments{
  \item{x}{usually a character vectors, but could be a
  vector of distinct numbers.}

  \item{y}{usually a character vectors, but could be a
  vector of distinct numbers.}

  \item{B}{the number of permutations}

  \item{sizes}{an integer vector to use as the values for
  \dQuote{n}}

  \item{sizes.random}{an integer vector to use as the
  values for \dQuote{n}. default=\code{NULL}, which take on
  the first c(1:100,200,300,N)}

  \item{ylim}{see \code{\link{plot}}}

  \item{proportion}{logical: set x-axis to proportion in
  common}

  \item{random.col}{the background colour of the randomised
  area}

  \item{add}{see \code{\link{plot}}}

  \item{col}{the colour of the foreground points}

  \item{randomise}{which data set to randomise? \dQuote{x}
  or \dQuote{y}.}

  \item{pch}{the print character. Default: NULL = choose
  the pch based on the values for xTrue and yTrue. see
  Details}

  \item{main}{see \code{\link{plot}}}

  \item{\dots}{arguments passed to plot}
}
\description{
  Permute \code{x} or \code{y}, \code{B} times, and
  re-calculate the CAT traces of x vs y', or x' vs y. Plot
  this distribution of randomised CAT traces as a grey null
  distribution, with the unpermuted CAT trace over the top.
}
\author{
  Mark Cowley, 2008-12-08
}
\seealso{
  \code{\link{catplot}} \code{\link{calc.CAT}}
}

