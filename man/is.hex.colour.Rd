\name{is.hex.colour}
\alias{is.hex.colour}
\title{is "hex" colour format}
\usage{
  is.hex.colour(hex)
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
is.hex.colour("white")
is.hex.colour("#FFFFFF")
is.hex.colour(c(255,255,255))
}
\author{
  Mark Cowley, 2011-10-20
}

