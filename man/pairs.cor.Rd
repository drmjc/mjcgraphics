\name{pairs.cor}
\alias{pairs.cor}
\title{pairs plot with correlation stats}
\usage{
  pairs.cor(x, ..., upper.panel = panel.smooth,
    lower.panel = panel.cor)
}
\arguments{
  \item{x}{the coordinates of points given as numeric
  columns of a matrix or dataframe. Logical and factor
  columns are converted to numeric in the same way that
  data.matrix does.}

  \item{\dots}{arguments passed to
  \code{\link[graphics]{pairs}}}

  \item{upper.panel}{which function to use in the upper
  plotting panel?  default = \code{panel.smooth}}

  \item{lower.panel}{which function to use in the lower
  plotting panel?  default = \code{\link{panel.cor}}}
}
\description{
  A wrapper to pairs with a correlation value in lower
  triangle, and a smoothed dot plot in upper triangle
}
\author{
  Mark Cowley, 2008-12-15
}

