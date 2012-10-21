\name{plot_mean_vs_var}
\alias{plot_mean_vs_var}
\title{plot mean vs variance}
\usage{
  plot_mean_vs_var(x, xlab = NULL, ylab = NULL,
    main = "Per-ProbeSet mean vs var", ...)
}
\arguments{
  \item{x}{a \code{matrix} or numeric \code{data.frame}}

  \item{xlab}{see par}

  \item{ylab}{see par}

  \item{main}{see par}

  \item{\dots}{arguments passed to \code{plot}}
}
\description{
  Plot the means of the rows of x (on x-axis) vs the
  variance of the rows of x (on y-axis)
}
\author{
  Mark Cowley, 3 April 2006
}

