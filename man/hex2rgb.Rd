\name{hex2rgb}
\alias{hex2rgb}
\title{Convert "hex" format to "rgb" format.}
\usage{
  hex2rgb(x = NULL)
}
\arguments{
  \item{x}{a vector of colours in the hex format:
  \dQuote{#FF00FF}}
}
\description{
  ie from "#FF00EO" to c(256,0,224)
}
\examples{
hex2rgb( "#FF00EO" )
# [1] c(256,0,224)
}
\author{
  Mark Cowley, 29/3/07
}

