\name{plot.matrix}
\alias{plot.matrix}
\title{Plot a matrix(-like object) row-wise.}
\usage{
  plot.matrix(x, bg.col = "lightgrey", main = NULL,
    sub = NULL, ticks = c(1, 2, 3, 4), xlab = "",
    xlim = c(1, ncol(x)), xlabels = rep("", ncol(x)),
    ylab = "", ylim = range(x, na.rm = TRUE),
    ylabels = NULL, vablines = NULL,
    vabline.col = "darkgrey", vabline.lty = 1,
    hablines = NULL, habline.col = "darkgrey",
    habline.lty = 1, col = 1, lty = 1, lwd = 1, pch = 1,
    type = "l", axes = TRUE, tcl = 0.25, add = FALSE,
    auto.log = TRUE, symmetrical = FALSE,
    unique.lines = FALSE, lty.cor = FALSE,
    legend.pos = "none", ...)
}
\arguments{
  \item{x}{a matrix-like object of data, with each row to
  be plotted in a single line (if type draws lines)}

  \item{auto.log}{if the data is in the range [0,1] then it
  can be converted to -log10. Useful if x contains
  p-values.}

  \item{symmetrical}{If the data in x is centred approx
  about 0, then setting to \code{TRUE} makes the plots a
  little more pleasing.}

  \item{unique.lines}{works out a unique combination of
  line colour and type (col and lty) such that the lines
  plotted are as unique as possible (ie 8 coloured solid
  lines, then 8 dotted etc)}

  \item{lty.cor}{If x contains records that are correlated,
  and perhaps anti- correlated, setting to TRUE will work
  out which lines are anti- correlated, and plot them as
  dashed lines, with all of the correlated lines drawn as
  solid lines.}

  \item{legend.pos}{see
  \code{\link[graphics]{legend}}(pos)}
}
\value{
  none -- used to plot.
}
\description{
  Can specify a different col, lty and/or lwd for each line
  (defaults are all 1). \code{unique.lines} will work out a
  unique combination of col and lty for each row of x (num
  rows <= 64)
}
\author{
  Mark Cowley, 29 August 2005
}

