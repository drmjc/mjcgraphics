\name{panel.cor}
\alias{panel.cor}
\title{Print the correlation in a panel}
\usage{
  panel.cor(x, y, digits = 2, prefix = "", cex.cor)
}
\arguments{
  \item{x}{a numeric vector}

  \item{y}{a numeric vector}

  \item{digits}{the number of digits to display}

  \item{prefix}{the prefix to add to the correlation score.
  Default=\dQuote{}}

  \item{cex.cor}{the character expansion value.
  Default=missing == 0.8/strwidth(txt)}
}
\value{
  alters a plotting panel
}
\description{
  Print the correlation in a panel, with size proportional
  to the magnitude of correlation. Taken from the examples
  within \code{\link[graphics]{pairs}}
}
\author{
  Mark Cowley, 2008-12-15
}
\seealso{
  \code{\link{pairs.cor}}, \code{\link[graphics]{pairs}}
}

