\name{barplot.gety}
\alias{barplot.gety}
\title{Function to determine the height of each bar with extra padding.}
\usage{
  barplot.gety(height, space = 0.05, ...)
}
\arguments{
  \item{height}{the same vector that you passed to barplot}

  \item{space}{the gap between top/bottom of bar and the
  point, as a proportion of the y-range}

  \item{\dots}{currently ignored}
}
\value{
  a numeric vector Todo: handle \code{beside=FALSE} and
  matrices for height
}
\description{
  Why??? you might want to add text above the positive
  bars, and below the negative bars.
}
\author{
  Mark Cowley, 2009-03-20
}

