\name{calc.CAT}
\alias{calc.CAT}
\title{Calculate the overlap b/w two vectors, for various sizes.}
\usage{
  calc.CAT(x, y, sizes)
}
\arguments{
  \item{x}{usually 2 character vectors, but could be a
  vector of distinct numbers.}

  \item{y}{usually 2 character vectors, but could be a
  vector of distinct numbers.}

  \item{sizes}{a numeric vector of sizes at which to
  determine the overlap b/w x and y. eg c(1,5,10,100) will
  find out how many values in common b/w x and y for the
  top 1, 5, 10 or 1000 values in x.}
}
\value{
  a numeric vector corresponding to the proportion of
  values in common in x and y for each given size, named by
  the size used.  eg: if you take top 10 from each list,
  and 5 are in common, the proportion is 0.5
}
\description{
  see also catplot
}
\author{
  Mark Cowley, 2008-05-15
}
\seealso{
  \code{\link{catplot}} \code{\link{catplot.vs.random}}
}

