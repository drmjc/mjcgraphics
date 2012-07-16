\name{add.errorbars}
\alias{add.errorbars}
\title{Add errorbars to a barplot.}
\usage{
  add.errorbars(height, width = 1, space = NULL,
    beside = FALSE, horiz = FALSE, errors = NULL,
    errors.col = par("fg"), errors.width = 0.15,
    errors.type = c("+", "-"), prespace = 0)
}
\arguments{
  \item{height}{see \code{\link{errorbarplot}}}

  \item{width}{see \code{\link{errorbarplot}}}

  \item{space}{see \code{\link{errorbarplot}}}

  \item{beside}{see \code{\link{errorbarplot}}}

  \item{horiz}{see \code{\link{errorbarplot}}}

  \item{\dots}{see \code{\link{errorbarplot}}}

  \item{errors}{the object containing errors (eg standard
  deviation) with the SAME size/dimension as height.}

  \item{errors.col}{the colour of the errors bars}

  \item{errors.width}{the proportion of width that the
  ticks should be at the top/bottom or lhs/rhs of the error
  tick}

  \item{errors.type}{plot the errors bars going into and
  outof the bar itself (eg just "+" will plot the tick
  pointing out of the bar)}

  \item{prespace}{mostly used for interal processing when
  height and errors is a \code{matrix}.}
}
\description{
  Highly recommend you use \code{\link{errorbarplot}}
  instead, since you need to provide the same arguments as
  for \code{\link{errorbarplot}} that affect the bar size
  and spacing to this function since the placing of the
  error bars in the middle of the bars is impossible to
  achieve otherwise.
}
\author{
  Mark Cowley, 28 April 2006
}

