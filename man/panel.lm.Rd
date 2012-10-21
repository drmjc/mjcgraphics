\name{panel.lm}
\alias{panel.lm}
\title{panel containing xy plot and linear model fit line}
\usage{
  panel.lm(x, y, lty = "solid", col = par("col"), bg = NA,
    pch = par("pch"), cex = 1, col.lm = "red", ...)
}
\arguments{
  \item{x}{numeric vectors of the same length}

  \item{y}{numeric vectors of the same length}

  \item{col}{see \code{\link[graphics]{par}}}

  \item{bg}{see \code{\link[graphics]{par}}}

  \item{pch}{see \code{\link[graphics]{par}}}

  \item{cex}{see \code{\link[graphics]{par}}}

  \item{col.smooth}{color to be used by \code{lines} for
  drawing the smooths.}

  \item{span}{smoothing parameter \code{f} for
  \code{lowess}, see there.}

  \item{iter}{number of robustness iterations for
  \code{lowess}.}

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

