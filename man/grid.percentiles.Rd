\name{grid.percentiles}
\alias{grid.percentiles}
\alias{grid.quartiles}
\title{Add a grid, with lines along the percentiles or quantiles}
\usage{
  grid.percentiles(side = 1, max, col = "lightgray",
    lty = "dotted", lwd = par("lwd"), ...)

  grid.quartiles(side = 1, max, col = "lightgray",
    lty = "dotted", lwd = par("lwd"), ...)
}
\arguments{
  \item{\dots}{arguments passed to abline and grid.}

  \item{side}{which side to add the axis?}

  \item{max}{the maximum value. can't be missing}

  \item{col}{character or (integer) numeric; color of the
  grid lines.}

  \item{lty}{character or (integer) numeric; line type of
  the grid lines.}

  \item{lwd}{non-negative numeric giving line width of the
  grid lines.}
}
\value{
  nothing.
}
\description{
  Add a grid, with lines along the percentiles or quantiles
}
\examples{
\dontrun{
plot(1:100, xaxt="n")
axis.percentiles(side=1, max=100)
grid.percentiles(side=1, max=100)
}
}
\author{
  Mark Cowley, 2012-07-23
}
\seealso{
  \code{\link{axis.percentiles}}
}

