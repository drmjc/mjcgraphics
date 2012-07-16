\name{errorbarplot}
\alias{errorbarplot}
\title{barplot with errorbars}
\usage{
  errorbarplot(height, width = 1, space = NULL,
    beside = FALSE, horiz = FALSE, ylim = NULL,
    xlim = NULL, ..., errors = NULL,
    errors.col = par("fg"), errors.width = 0.15,
    errors.type = c("+", "-"), pvalue.stars = NULL,
    pvalue.stars.pos = c("none", "above", "below", "top", "bottom", "left", "right"),
    pvalue.stars.cex = 1, pvalue.stars.col = "black",
    pvalue.stars.space = 0.05)
}
\arguments{
  \item{height}{see \code{\link{barplot}}}

  \item{width}{see \code{\link{barplot}}}

  \item{space}{see \code{\link{barplot}}}

  \item{beside}{see \code{\link{barplot}}}

  \item{horiz}{see \code{\link{barplot}}}

  \item{ylim}{see \code{\link{barplot}}}

  \item{xlim}{see \code{\link{barplot}}}

  \item{\dots}{see \code{\link{barplot}}}

  \item{errors}{the object containing errors (eg standard
  deviation) with the SAME size and dimension as
  \code{height}.}

  \item{errors.col}{the colour of the errors bars}

  \item{errors.width}{the proportion of width that the
  ticks should be at the top/bottom or lhs/rhs of the error
  tick}

  \item{errors.type}{plot the errors bars going into and
  outof the bar itself (eg just "+" will plot the tick
  pointing out of the bar)}

  \item{pvalue.stars}{stars to add above significant bars,
  or \code{NULL}.  must be same dimensions as \code{height}
  and \code{errors}}

  \item{pvalue.stars.pos}{where to put the stars? one of
  above, top, right, below, bottom, left}

  \item{pvalue.stars.cex}{size of the stars}

  \item{pvalue.stars.col}{colour of the stars}

  \item{pvalue.stars.space}{buffer between stars and the
  errorbar}
}
\description{
  'height' and 'errors' much match exactly in dimension,
  and can either be both vectors, or both matrices.
}
\author{
  Mark Cowley, 28 April 2006
}

