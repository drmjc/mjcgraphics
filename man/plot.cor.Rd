\name{plot.cor}
\alias{plot.cor}
\title{Correlation xy-plot}
\usage{
  plot.cor(x, y, symmetrical = TRUE, axes = TRUE,
    lowess = TRUE, leastsquares = TRUE, unity = TRUE,
    hist = TRUE, labels = NULL, cor.label = TRUE,
    r2.label = TRUE, cor.method = "spearman",
    legend = "topleft", xlim = NULL, ylim = NULL, ...)
}
\arguments{
  \item{x}{the 2 data to correlate}

  \item{y}{the 2 data to correlate}

  \item{symmetrical}{logical: make the plot symmetrical?
  (TRUE)}

  \item{axes}{logical: add internal axes at x=0, y=0}

  \item{lowess}{logical: add a lowess line through your
  data (default settings)}

  \item{leastsquares}{logical: plot the straight line of
  least squares (see lm)}

  \item{unity}{logical: plot a line through 0 with a slope
  of 1}

  \item{hist}{logical: add histograms to the sides?}

  \item{labels}{logical: label the data points with names
  to the right of them? This will also resize the axes by
  110\% to let the right most points' names be seen.}

  \item{cor.label}{logical: add a correlation label?}

  \item{r2.label}{logical: add an r2 label?}

  \item{cor.method}{the method to use to assess
  correlation. one of \dQuote{spearman} or
  \dQuote{pearson}}

  \item{legend}{position of the legend.
  default=\dQuote{topleft}, see \code{\link{legend}}}

  \item{xlim}{set the x-axis limits. leave as \code{NULL}
  to automatically determine.}

  \item{ylim}{set the y-axis limits. leave as \code{NULL}
  to automatically determine.}

  \item{\dots}{arguments passed to \code{\link{plot}}, or,
  if \code{hist=TRUE} \code{\link{xyplot.hist}}}
}
\value{
  none.
}
\description{
  Plot x vs y with a lowess line and a correlation label
  under main title, and histograms above and to the right
  of the plot.
}
\author{
  Mark Cowley, 14 Nov 2006
}

