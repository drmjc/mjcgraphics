\name{stripchart_annot}
\alias{stripchart_annot}
\alias{stripchart.annot}
\title{Make a normal stripchart, but below the plot add a specified value for each
set of points (such as the mean or MAD or stdev)}
\usage{
  stripchart_annot(x, values, vlab = "stat",
    val.ratio = 0.1, frame = TRUE, method = "jitter",
    jitter = 0.2, offset = 1/3, vertical = TRUE,
    group.names, ylim = NULL, ylab = NULL, xlab = NULL,
    pch = 0, do.layout = TRUE, las = 2, yaxt = "s",
    group.median = FALSE, group.mean = FALSE, ...)
}
\arguments{
  \item{values}{the vector of values to annotate with}

  \item{vlab}{the value label which is plotted on
  \code{axis(side=2)}}

  \item{val.ratio}{the heights ratio, relative to the main
  plot which has a value of 1.  default=0.1, ie the bottom
  annotation plot will be 10\% of the size of the main
  plot}

  \item{frame}{logical: add lines around each bar?}

  \item{x}{the data from which the plots are to be
  produced.  In the default method the data can be
  specified as a single numeric vector, or as list of
  numeric vectors, each corresponding to a component plot.
  In the \code{formula} method, a symbolic specification of
  the form \code{y ~ g} can be given, indicating the
  observations in the vector \code{y} are to be grouped
  according to the levels of the factor \code{g}.
  \code{NA}s are allowed in the data.}

  \item{method}{the method to be used to separate
  coincident points.  The default method \code{"overplot"}
  causes such points to be overplotted, but it is also
  possible to specify \code{"jitter"} to jitter the points,
  or \code{"stack"} have coincident points stacked.  The
  last method only makes sense for very granular data.}

  \item{jitter}{when \code{method="jitter"} is used,
  \code{jitter} gives the amount of jittering applied.}

  \item{offset}{when stacking is used, points are stacked
  this many line-heights (symbol widths) apart.}

  \item{vertical}{when vertical is \code{TRUE} the plots
  are drawn vertically rather than the default horizontal.}

  \item{group.names}{group labels which will be printed
  alongside (or underneath) each plot.}
}
\value{
  none. makes a plot.
}
\description{
  Make a normal stripchart, but below the plot add a
  specified value for each set of points (such as the mean
  or MAD or stdev)
}
\author{
  Mark Cowley, 2009-07-31
}

