\name{colour.step}
\alias{colour.step}
\title{Make a gradient of colours.
Evenly make \code{steps} colours merging from the colour \code{from} to the colour
\code{to}, optionally via the colour \code{via}}
\usage{
  colour.step(from = "black", to = "red", via = NULL,
    steps = 10, alpha = 255)
}
\arguments{
  \item{from}{the first, or starting colour}

  \item{to}{the final colour}

  \item{via}{[optional] the middle colour. Leave as
  \code{NULL}, to ignore this.}

  \item{steps}{numeric: how many colours do you want?}

  \item{alpha}{numeric in [0,255] controlling the opacity,
  from trasparent (0) to opaque (255), or in [0.0,1.0].}
}
\description{
  Make a gradient of colours. Evenly make \code{steps}
  colours merging from the colour \code{from} to the colour
  \code{to}, optionally via the colour \code{via}
}
\examples{
colour.step(from="green", to="red", via="black", steps=3)
}
\author{
  Mark Cowley, 24 August, 2005
}
\seealso{
  \code{\link[RColorBrewer]{brewer.pal}}
}

