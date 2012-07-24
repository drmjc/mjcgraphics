\name{mjc_colours}
\alias{colour2hex}
\alias{colour2rgb}
\alias{colours.hex}
\alias{colours.rgb}
\alias{hex2colour}
\alias{hex2rgb}
\alias{inverse.colour}
\alias{is.colour}
\alias{is.hex.colour}
\alias{is.rgb.colour}
\alias{mjc_colours}
\alias{rgb2colour}
\alias{rgb2hex}
\title{colours, hex-colours and rgb-colours}
\usage{
  colours.hex()

  colours.rgb()

  is.colour(col)

  is.hex.colour(hex)

  is.rgb.colour(rgb)

  colour2rgb(col = NULL)

  colour2hex(col = NULL)

  hex2rgb(hex = NULL)

  rgb2hex(rgb)

  hex2colour(hex, cols.rgb = colours.rgb())

  rgb2colour(rgb, cols.rgb = colours.rgb())

  inverse.colour(col)
}
\arguments{
  \item{col}{a vector of values in the \code{colour}
  format: eg \dQuote{white}}

  \item{rgb}{a vector of colours in the \code{rgb} format;
  ie a 3-column matrix of [0,255] values, one colour per
  row.}

  \item{hex}{a vector of colours in the hex format: eg
  \dQuote{#FF00FF}}

  \item{cols.rgb}{the pool of colours with which to map
  \code{hex} to in rgb format.  Leave as default to get the
  normal "R colours". NB: If \code{cols.rgb} doesn't have
  names, then the index into cols will be returned. If
  \code{cols.rgb} has valid names, then they will be
  returned.}
}
\value{
  \code{colours}, \code{colours.hex}, \code{colours.rgb}: a
  vector of col-format colours, hex-format colours, or
  matrix of rgb-format colours, 1 row per colour.\cr \cr
  \code{is.colour}, \code{is.hex.colour},
  \code{is.rgb.colour}: logical.\cr \cr \code{colour2rgb},
  \code{colour2hex}, \code{hex2rgb}, \code{rgb2hex},
  \code{hex2colour}, \code{rgb2colour}: a vector of colour
  formats, converted from one to another.\cr \cr
  \code{hex2colour}: a vector of the name of R colours; eg
  \code{c("white", "magenta", "lightblue")} or a vector of
  indices into the \code{cols.rgb} vector, if no valid
  names were found in the \code{cols.rgb} object.
  \code{rgb2colour}: a vector of the name of R colours; eg
  \code{c("white", "magenta", "lightblue")}, or a vector of
  indices into the \code{cols.rgb} vector, if no valid
  names were found in the \code{cols.rgb} object.\cr \cr
  \code{inverse.colour}: a vector of colours\cr
}
\description{
  A collection of functions to convert colours from names
  (aka \code{colours}), hex values (aka \code{hex}) and RGB
  3 character numeric's (aka \code{rgb}). This is
  \emph{old} code, and is crying out for at least some
  S3-class love. It was written to facilitate the
  interpolation of lots of colours, which requires named
  colours to be numerically represented. This code allows
  \code{\link{colour.step}} to do its thing, though newer
  code in the \code{RColorBrewer} package may make much of
  this redundant.
}
\details{
  grDevices provides \code{\link[grDevices]{colours}}, and
  \code{\link[grDevices]{col2rgb}}. We wanted a hexadecimal
  representation of colours which included an alpha
  channel, as well as methods for checking colours,
  converting between colour types, and generating a
  spectrum of colours.
}
\section{Allowed colour Formats}{
  We've named the default colours provided by grDevices the
  \code{col} format, for want of a better name. \itemize{
  \item{col:}{"White", "white", "Black", "green"}
  \item{hex:}{"#FF00FF", "#FF00FFAE"}
  \item{rgb:}{matrix(c(0,255,0), 1, 3)} }
}

\section{colour types}{
  \code{colours}, \code{colours.hex}, \code{colours.rgb}:
  return the hex, or rgb representations of the named
  colours provided by \code{\link[grDevices]{colours}} from
  \code{grDevices}.
}

\section{colour checks}{
  \code{is.colour}, \code{is.hex.colour},
  \code{is.rgb.colour}: methods for checking whether the
  input is one of the 3 colour types, the hex colour type,
  or rgb colour type, respectively.
}

\section{colour conversion}{
  \code{colour2rgb}, \code{colour2hex}: convert from
  \code{col} format\cr \code{hex2rgb}, \code{hex2colour}:
  convert from hex format\cr \code{rgb2hex},
  \code{rgb2colour}: convert from rgb format\cr Note,
  \code{hex2colour}: there are 256^3 possible hex's but
  only \code{length(colours())} possible outputs, so the
  closest colour for each \code{hex} is determined. This
  function is the opposite of \code{grDevices:col2rgb}, and
  also is the opposite of \code{colour2rgb}\cr Note,
  \code{rgb2colour}: there are 256^3 possible x's but only
  length(cols.rgb()) possible outputs, so the closest
  colour for each x is determined. This function is the
  opposite of grDevices:col2rgb, and also is the opposite
  of colour2rgb
}

\section{\code{inverse.colour}}{
  inverse a \code{col}-format colour & return the
  \code{hex}-format colour
}
\examples{
colours()[1:5]
# [1] "white"         "aliceblue"     "antiquewhite"  "antiquewhite1"
# [5] "antiquewhite2"
colours.hex()[1:5]
#     white     aliceblue  antiquewhite antiquewhite1 antiquewhite2
# "#FFFFFF"     "#F0F8FF"     "#FAEBD7"     "#FFEFDB"     "#EEDFCC"
colours.rgb()[1:5,]
#               red green blue
# white         255   255  255
# aliceblue     240   248  255
# antiquewhite  250   235  215
# antiquewhite1 255   239  219
# antiquewhite2 238   223  204

is.colour("white") # TRUE
is.colour("#FFFFFF") # FALSE
is.colour(c(255,255,255)) # FALSE

is.hex.colour("white") # FALSE
is.hex.colour("#FFFFFF") # TRUE
is.hex.colour(c(255,255,255)) # FALSE

is.rgb.colour("white") # FALSE
is.rgb.colour("#FFFFFF") # FALSE
is.rgb.colour(c(255,255,255)) # TRUE

colour2rgb("White")
#       red green blue
# White 255   255  255
colour2hex("White")
#     White
# "#FFFFFF"
hex2rgb( "#FF00E0" )
#      red green blue
# [1,] 255     0  224
rgb2hex( matrix(c(255,128,255),1,3) )
# [1] #FF80FF
hex2colour( "#FF80FF" )
# [1] "orchid1"
rgb2colour(matrix(c(255,128,255), 1, 3))
# [1] "orchid1"

inverse.colour("red")
#       red
# "#00FFFF"
inverse.colour("orchid1")
#   orchid1
# "#007C05"
}
\author{
  Mark Cowley, 29/3/07
}

