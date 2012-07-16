\name{plot2}
\alias{plot2}
\title{plot2}
\usage{
  plot2(x, y = NULL, bg.col = "lightgrey", main = NULL,
    sub = NULL, ticks = c(1, 2, 3, 4), xlab = "",
    xlim = NULL, xlabels = NULL, ylab = "", ylim = NULL,
    ylabels = NULL, vablines = NULL,
    vabline.col = "darkgrey", vabline.lty = 1,
    hablines = NULL, habline.col = "darkgrey",
    habline.lty = 1, col = 1, lty = 1, lwd = 1, type = "l",
    axes = TRUE, tcl = 0.25, ...)
}
\arguments{
  \item{x}{see ?par or ?plot.}

  \item{y}{see ?par or ?plot.}

  \item{main}{see ?par or ?plot.}

  \item{sub}{see ?par or ?plot.}

  \item{xlab}{see ?par or ?plot.}

  \item{ylab}{see ?par or ?plot.}

  \item{xlim}{see ?par or ?plot.}

  \item{ylim}{see ?par or ?plot.}

  \item{col}{see ?par or ?plot.}

  \item{lty}{see ?par or ?plot.}

  \item{lwd}{see ?par or ?plot.}

  \item{type}{see ?par or ?plot.}

  \item{tcl}{commonly overlooked parameter which allows the
  ticks to point in (positive) or out of the plot
  (negative). set to NULL ro retain default behaviour.}

  \item{bg.col}{change the background colour. Set to NULL
  to retain default behaviour}

  \item{ticks}{which axes should have tick marks plotted?}

  \item{xlabels}{override the automatic labels added to
  x-axis. NB it's up to the user to supply either the same
  number of labels as normally computed by axTicks(side=1),
  or if more labels than spots for labels are supplied,
  then they are placed at \code{1:length(xlabels)}.}

  \item{ylabels}{as above for side=2.}
}
\value{
  none. makes a plot
}
\description{
  A versatile plotting function which allows the user to: -
  change the background colour of the plotting region - add
  ticks to all 4 axes - change the labels for the x and y
  axes in one command - point the axis ticks inwards (make
  tcl positive)
}
\details{
  NB: axes are added after plotting the data using axis and
  title commands.
}
\author{
  Mark Cowley, 28 March 2006
}

