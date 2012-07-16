\name{xyplot.hist}
\alias{xyplot.hist}
\title{An xyplot with histograms
An xyplot with a difference -- also show the distributions of data on the
xaxis, or yaxis.}
\usage{
  xyplot.hist(x, y, breaks = 50, xlab = "", ylab = "",
    main = "", symmetrical = "", axes = TRUE,
    xhist.plot = TRUE, yhist.plot = TRUE,
    xhist.axis = FALSE, yhist.axis = FALSE,
    xhist.lab = "Frequency", yhist.lab = "Frequency",
    hist.col = "white", xhist.col = hist.col,
    yhist.col = hist.col, ...)
}
\arguments{
  \item{x}{vectors of numbers, can contain NA's}

  \item{y}{vectors of numbers, can contain NA's}

  \item{breaks}{The number of breaks in the histograms. see
  ?hist}

  \item{xlab}{see ?par. main will be plotted in the top
  centre of the plot}

  \item{ylab}{see ?par. main will be plotted in the top
  centre of the plot}

  \item{main}{see ?par. main will be plotted in the top
  centre of the plot}

  \item{symmetrical}{make the axes symmetrical about zero.
  allowed values include \dQuote{} (no change), \dQuote{x},
  make x-axis symmetrical \dQuote{y}, make y-axis
  symmetrical \dQuote{xy}, make both x and y-axes
  symmetrical \dQuote{both}, to make the plot square and
  symmetrical}

  \item{axes}{logical: plot the x=0, y=0 axes as dashed
  lines}

  \item{xhist.plot}{plot histograms of data on the x or
  yaxis? If both are \code{FALSE}, then just use xyplot.}

  \item{yhist.plot}{plot histograms of data on the x or
  yaxis? If both are \code{FALSE}, then just use xyplot.}

  \item{xhist.axis}{logical: add an axis for the
  x-histogram}

  \item{yhist.axis}{logical: add an axis for the
  y-histogram}

  \item{hist.col}{the colour for the histograms}

  \item{xhist.col}{the colour for the histograms}

  \item{yhist.col}{the colour for the histograms}

  \item{\dots}{further arguments to \code{\link{xyplot}}.}
}
\description{
  This plot makes a normal xy plot, but also has a
  horizontal, and a vertical histogram, on the top and side
  of the plot. Original code from Addicted to R:
  \url{http://wiki.r-project.org/rwiki/doku.php?id=graph_gallery:graph78}
}
\author{
  Mark Cowley, 2008-09-02
}

