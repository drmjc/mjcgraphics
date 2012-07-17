\name{pvalue.stars}
\alias{pvalue.stars}
\title{Determine how many star's to annotate a vector of pvalues}
\usage{
  pvalue.stars(pvals, thresh = c(0.05, 0.01, 0.001))
}
\arguments{
  \item{pvals}{a vector of pvalues}

  \item{thresh}{a vector of thresholds for 1, 2, 3...
  stars. this can be as long as the user needs}
}
\value{
  A character vector of "", "*", "**", ... same length as
  the input
}
\description{
  Determine how many star's to annotate a vector of pvalues
}
\author{
  Mark Cowley, 2009-03-20
}

