\name{plot.density.matrix}
\alias{plot.density.matrix}
\title{Density plots for every column in a matrix}
\usage{
  plot.density.matrix(data, legend = FALSE, add = FALSE,
    main = "Density Plot", ylim = NULL, xlim = NULL,
    xlab = "Expression Level (Log2)",
    ylab = "Probability Density", col = NULL,
    auto.log2 = TRUE, lty = NULL, legend.cex = 0.5, ...)
}
\arguments{
  \item{data}{a \code{matrix} or \code{data.frame} of
  values}

  \item{legend}{logical: add a legend to the plot?}

  \item{add}{logical: if \code{TRUE}, add to an existing
  plotting device, else create a new plotting device}

  \item{main}{the plot title}

  \item{ylim}{the y-axis limits. if \code{NULL}, this is
  auto-calculated}

  \item{xlim}{the x-axis limits. if \code{NULL}, this is
  auto-calculated}

  \item{xlab}{the x-axis title}

  \item{ylab}{the y-axis title}

  \item{col}{the colours of each density curve, which are
  recycled if necessary. if \code{NULL}, the standard
  colours \code{1:8} are used.}

  \item{auto.log2}{logical: automatically log-base-2
  transform}

  \item{lty}{the line type for each density play. if
  \code{NULL}, line types \code{1:ncol(data)}}

  \item{legend.cex}{the character expansion of the legend.
  default=0.5}

  \item{\dots}{arguments passed to \code{\link{lines}}}
}
\description{
  Generic function to plot the columns of a
  \code{matrix}/\code{data.frame} as overlayed density
  plots.
}
\author{
  Mark Cowley, 9 May 2005
}

