\name{rgb2hex}
\alias{rgb2hex}
\title{Convert "rgb" format to "hex" format.}
\usage{
  rgb2hex(x)
}
\arguments{
  \item{x}{a vector of colours in the "rgb format" format;
  ie has three columns of [0,255] values, one colour per
  row.}
}
\description{
  ie from c(255,255,255) to "#FFFFFF"
}
\examples{
rgb2hex( matrix(c(255,255,255),1,3) )
# [1] #FFFFFF
}
\author{
  Mark Cowley, 29/3/07
}

