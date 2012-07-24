\name{colours.mjc}
\alias{colours.mjc}
\title{colours.mjc}
\usage{
  colours.mjc(palette = c("excel07", "gp", "rgb", "reds", "greens", "blues", "oranges", "cyans", "purples", "red2green"),
    N = 5)
}
\arguments{
  \item{palette}{one of a number of named palette's. see
  usage.}

  \item{N}{the number of colours}
}
\value{
  a vector of N colours, in hex format
}
\description{
  Produce a vector of colours, following a number of
  colouring schemes.
}
\examples{
colours.mjc("excel07")
colours.mjc("reds", 7)
}
\seealso{
  \code{\link[RColorBrewer]{brewer.pal}}
}

