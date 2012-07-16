\name{hex2colour}
\alias{hex2colour}
\title{Convert "hex" format to "col" format.}
\usage{
  hex2colour(x, cols.rgb = colours.rgb())
}
\arguments{
  \item{x}{a vector of colours in the hex format:
  \dQuote{#FF00FF}}

  \item{cols.rgb}{the pool of colours with which to map
  \code{x} to in rgb format.  Leave as default to get the
  normal "R colours". NB: If \code{cols.rgb} doesn't have
  names, then the index into cols will be returned. If
  \code{cols.rgb} has valid names, then they will be
  returned.}
}
\value{
  a vector of the name of R colours; eg \code{c("white",
  "magenta", "lightblue")} or a vector of indices into the
  \code{cols.rgb} vector, if no valid names were found in
  the \code{cols.rgb} object.
}
\description{
  ie from "#FFFFFF" to "White" Note, there are 256^3
  possible x's but only length(colours()) possible outputs,
  so the closest colour for each x is determined. This
  function is the opposite of grDevices:col2rgb, and also
  is the opposite of colour2rgb
}
\examples{
hex2colour( "#FFFFFF" )
# [1] "white"
}
\author{
  Mark Cowley, 29/3/07
}

