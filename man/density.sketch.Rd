\name{density.sketch}
\alias{density.sketch}
\title{density sketch}
\usage{
  density.sketch(x, sketch = 10000, ...)
}
\arguments{
  \item{x}{a numeric vector}

  \item{sketch}{the number of values to randomly sample
  from x, only if length(x) > sketch}

  \item{\dots}{additional arguments passed to
  \code{\link[stats]{density}}}
}
\value{
  see \code{\link[stats]{density}}
}
\description{
  For huge vectors, density can take a very long time.
}
\details{
  Inspired by RMA-sketch which takes a subset of data
  points to determine the reference distribution, this
  method computes the density on a subset of data points
  from the original vector.
}
\author{
  Mark Cowley, 2009-01-23
}

