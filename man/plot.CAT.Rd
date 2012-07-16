\name{plot.CAT}
\alias{plot.CAT}
\title{Correspondence at the top (CAT) plot}
\usage{
  plot.CAT(x, y, sizes = 1:length(x), sketch = 0.1,
    main = "CAT plot", ylim = c(0, 1), add = FALSE,
    proportion = FALSE, xTrue = rep(TRUE, length(x)),
    yTrue = rep(TRUE, length(y)), pch = NULL, ...)
}
\arguments{
  \item{x}{usually a character vectors, but could be a
  vector of distinct numbers.}

  \item{y}{usually a character vectors, but could be a
  vector of distinct numbers.}

  \item{sizes}{an integer vector to use as the values for
  \dQuote{n}}

  \item{sketch}{a value in (0,1.0] to be used if there are
  > 1000 datapoints}

  \item{main}{see \code{\link{plot}}}

  \item{ylim}{see \code{\link{plot}}}

  \item{add}{see \code{\link{plot}}}

  \item{proportion}{logical: set x-axis to proportion in
  common}

  \item{xTrue}{logical vector, same length as \dQuote{x} to
  indicate significance of values in \dQuote{x}}

  \item{yTrue}{logical vector, same length as \dQuote{y} to
  indicate significance of values in \dQuote{y}}

  \item{pch}{the print character. Default: NULL = choose
  the pch based on the values for xTrue and yTrue. see
  Details}

  \item{\dots}{arguments passed to plot}
}
\value{
  none. makes a plot.
}
\description{
  CAT plots are a useful way of comparing 2 lists, over a
  dynamic range of \sQuote{top N} thresholds.
}
\details{
  \code{xTrue}, \code{yTrue} and \code{pch}\cr Often, some
  of the \code{x} and \code{y} values also have a measure
  of statistical significance, or interest, and it would be
  nice to know at which point along the resulting plot that
  (a) both \code{x} and \code{y} were statistically
  significant (b), when neither are significant, and (c),
  where just x or y is significant. if \code{pch=NULL}, and
  \code{xTrue} and \code{yTrue} contain a range of
  \code{TRUE} and \code{FALSE}, then: if \code{xTrue &
  yTrue}, pch = 19 = filled circle if \code{xTrue | yTrue},
  pch = 1 = open circle if \code{!xTrue & !yTrue}, pch = 2
  = open triangle
}
\note{
  To create a CAT plot we made a list of \code{n} candidate
  genes for each of the two procedures and plotted the
  proportion of genes in common against the list size
  \code{n} (Irizarry et al, Nature Methods 2, 345 - 350
  (2005)) sketch: in [0,1]. if < 1 then only a subset of
  points will be plotted when the sets are larger than 1000
  values to compare. ie, if sketch=0.1, then only 10\% of
  the data points in 1001:end will be plotted. This
  over-rides the sizes paramter. Set it to 1.0 to ignore
  it.
}
\author{
  Mark Cowley, 29 August 2006
}
\references{
  Irizarry et al, Nature Methods 2, 345 - 350 (2005)
}
\seealso{
  \code{\link{calc.CAT}}
}

