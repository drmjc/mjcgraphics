\name{panel.lm}
\alias{panel.lm}
\title{panel containing xy plot and linear model fit line}
\usage{
  panel.lm(x, y, lty = "solid", col = par("col"), bg = NA,
    pch = par("pch"), cex = 1, col.lm = "red", ...)
}
\arguments{
  \item{col.lm}{the colour of the linear model line}

  \item{\dots}{arguments to control the appearance of the
  line, passed to \code{\link{abline}}}
}
\value{
  none.
}
\description{
  panel, for use with pairs, containing xy plot and linear
  model fit line.
}
\examples{
pairs(swiss, panel = panel.lm, pch = ".")
pairs(swiss, panel = panel.lm, lwd = 2, cex= 1.5, col="blue")# hmm...
}
\author{
  Mark Cowley, 2012-06-27
}
\seealso{
  \code{\link[graphics]{panel.smooth}}
  \code{\link[graphics]{pairs}}
}

