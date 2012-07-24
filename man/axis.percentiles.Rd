\name{axis.percentiles}
\alias{axis.percentiles}
\alias{axis.quartiles}
\title{axis.percentiles or axis.quartiles}
\usage{
  axis.percentiles(side = 4, max)

  axis.quartiles(side = 4, max)
}
\arguments{
  \item{side}{which side to add the axis?}

  \item{max}{the maximum value. can't be missing}
}
\value{
  none. adds an axis to the current plot.
}
\description{
  axis.percentiles or axis.quartiles
}
\examples{
\dontrun{
plot(1:100, xaxt="n")
axis.percentiles(side=1, max=100)
grid.percentiles(side=1, max=100)
}
}
\author{
  Mark Cowley, 2012-07-06
}
\seealso{
  \code{\link{grid.percentiles}}
}

