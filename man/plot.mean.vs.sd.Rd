\name{plot.mean.vs.sd}
\alias{plot.mean.vs.sd}
\title{plot mean vs stdev}
\usage{
  plot.mean.vs.sd(x, xlab = NULL, ylab = NULL,
    main = "Per-ProbeSet mean vs sd", ...)
}
\arguments{
  \item{x}{a \code{matrix} or numeric \code{data.frame}}

  \item{xlab}{see par}

  \item{ylab}{see par}

  \item{main}{see par}

  \item{\dots}{arguments passed to \code{plot}}
}
\description{
  Plot the means of the rows of x (on x-axis) vs the sd of
  the rows of x (on y-axis)
}
\author{
  Mark Cowley, 3 April 2006
}

