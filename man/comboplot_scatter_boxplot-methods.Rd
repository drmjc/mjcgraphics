\name{comboplot_scatter_boxplot}
\alias{comboplot_scatter_boxplot}
\alias{comboplot_scatter_boxplot.ExpressionSet}
\title{A combination plot: sorted scatter plot and boxplot}
\usage{
  comboplot_scatter_boxplot(x, main, xlab, ylab,
    do.percentiles, do.layout, ylim, ...)

  comboplot_scatter_boxplot.ExpressionSet(x, main = "",
    xlab = "Index", ylab = "Expression level (log2)",
    do.percentiles = TRUE, do.layout = TRUE,
    ylim = range(exprs(x), na.rm = TRUE), ...)
}
\arguments{
  \item{x}{a \code{numeric vector}, or an
  \code{ExpressionSet} object}

  \item{main}{see \code{\link{plot}}. If \code{is(x,
  "ExpressionSet")}, then leave \code{main=""}, and the
  featureName will be used for each plot.}

  \item{xlab}{see \code{\link{plot}}}

  \item{ylab}{see \code{\link{plot}}}

  \item{do.percentiles}{logical: if \code{TRUE}, then add a
  percentiles axis and grid to the plot}

  \item{do.layout}{logical: if \code{TRUE}, then layout the
  plotting region. Set to \code{FALSE} if you'd rather do
  this once, first in a large loop.}

  \item{ylim}{numeric(2) of the min and max on the y-axis.}

  \item{\dots}{arguments passed to \code{\link{plot}}}
}
\value{
  nothing.
}
\description{
  A 2 panel plot of a sorted numeric vector (from low to
  high), and a boxplot on the right. Additionally, a
  percentiles axis is added to the top, and a grid is
  plotted if \code{do.percentiles=TRUE}. Useful for
  plotting gene expression data for determining a cutoff.
}
\section{input types}{
  \code{x} can be a \code{numeric vector}; \code{x} can be
  an \code{ExpressionSet}, with >=1 features/genes (1 plot
  will be made per feature).
}
\examples{
if( interactive() ) {
  dat <- rnorm(100)
  comboplot_scatter_boxplot(dat)
  comboplot_scatter_boxplot(dat, do.percentiles=FALSE)
}
}
\author{
  Mark Cowley, 2012-09-12
}

