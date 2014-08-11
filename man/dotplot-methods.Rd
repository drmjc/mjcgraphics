\docType{methods}
\name{dotplot}
\alias{dotplot}
\alias{dotplot,ANY,character,ANY-method}
\alias{dotplot,character,ANY,ANY-method}
\alias{dotplot,numeric,missing,logical-method}
\alias{dotplot,numeric,missing,missing-method}
\alias{dotplot,numeric,numeric,logical-method}
\alias{dotplot,numeric,numeric,missing-method}
\title{dotplot methods}
\arguments{
  \item{x}{an object to dotplot}

  \item{y}{an optional object to dotplot}

  \item{sort}{logical: sort 'x'. ignored for 2D plots.}
}
\value{
  nothing. makes a plot
}
\description{
  The S3 method \code{\link[graphics]{plot}} is undoubtedly
  a workhorse. It sometimes makes dotplots, barplots,
  boxplots\dots This S4 method makes it explicit that you
  will get a \code{dotplot}, either 1D, or 2D. It can then
  be overridden when appropriate to define dotplots for new
  data types, like dotplot.ExpressionSet from my
  microarrays library.
}
\examples{
# 1D
dotplot(1:10)
dotplot(rnorm(100))
## dotplot(letters)
# 2D
dotplot(1:100, sqrt(1:100))
a <- 1:10; names(a) <- letters[1:10]
b <- 6:15; names(b) <- letters[6:15]
dotplot(a,b)
c <- 6:16; names(c) <- letters[6:16]
dotplot(a,c)
## dotplot(letters, 1:10)
## dotplot(1:10, letters)
}
\author{
  Mark Cowley
}

