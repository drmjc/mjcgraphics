\name{rgb2colour}
\alias{rgb2colour}
\title{Convert "rgb" format to "col" format.}
\usage{
  rgb2colour(x, cols.rgb = colours.rgb())
}
\arguments{
  \item{x}{a vector of colours in the "rgb format" format;
  ie has three columns of [0,255] values, one colour per
  row.}

  \item{cols.rgb}{the pool of colours with which to map x
  to in rgb format.  Leave as default to get the normal "R
  colours". NB: If \code{cols.rgb} doesn't have names, then
  the index into cols will be returned. If \code{cols.rgb}
  has valid names, then they will be returned.}
}
\value{
  a vector of the name of R colours;\cr eg c("white",
  "magenta", "lightblue"), or a vector of indices into the
  \code{cols.rgb} vector, if no valid names were found in
  the \code{cols.rgb} object.
}
\description{
  ie from "c(255,255,255)" to "White" Note,there are 256^3
  possible x's but only length(cols.rgb()) possible
  outputs, so the closest colour for each x is determined.
  This function is the opposite of grDevices:col2rgb, and
  also is the opposite of colour2rgb
}
\author{
  Mark Cowley, 29/3/07
}

