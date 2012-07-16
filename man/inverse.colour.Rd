\name{inverse.colour}
\alias{inverse.colour}
\title{Return the inverse of a colour in hex form so that it can be passed directly
to a plot command.}
\usage{
  inverse.colour(col)
}
\arguments{
  \item{col}{a vector of colours to be reversed}
}
\value{
  a vector of colours
}
\description{
  Return the inverse of a colour in hex form so that it can
  be passed directly to a plot command.
}
\examples{
plot(1:10, col=inverse.colour("purple"))
inverse.colour("red")
# #00FFFF
inverse.colour("green")
# #FF00FF
inverse.colour("blue")
# #FFFF00
}
\author{
  Mark Cowley, 16 May 2006
}

