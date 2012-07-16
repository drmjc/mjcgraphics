\name{is.rgb.colour}
\alias{is.rgb.colour}
\title{is "rgb" colour format}
\usage{
  is.rgb.colour(rgb)
}
\arguments{
  \item{col}{a vector of values}
}
\value{
  logical: \code{TRUE} if col is in "col" format,
  \code{FALSE} otherwise.
}
\description{
  Allowable colour Formats:\cr \itemize{
  \item{col}{examples: White, Black, green}
  \item{hex}{examples: "#FF00FF", "#FF00FFAE"}
  \item{rgb}{examples: c(0,255,0)} }
}
\examples{
is.rgb.colour("white")
is.rgb.colour("#FFFFFF")
is.rgb.colour(c(255,255,255))
}
\author{
  Mark Cowley, 2011-10-20
}

