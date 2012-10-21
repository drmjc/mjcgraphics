\name{auto.mfrow}
\alias{auto.mfrow}
\title{Automatically setup par mfrow}
\usage{
  auto.mfrow(nplots, setup = TRUE)
}
\arguments{
  \item{nplots}{an integer in [1,49]}

  \item{setup}{if \code{TRUE}, then the graphical
  parameters (par) is set-up if \code{FALSE}, and nplots <
  the number of spaces for plots in the device, then blank
  plots are added to fill in the unused spaces.}
}
\description{
  Determine how many rows and plots to make for a certain
  number of plots & make the appropriate call to
  \code{par(mfrow)}
}
\details{
  eg 25 plots will cause par(mfrow=c(5,5)) to be called.
  The function fills in any unsued plotting spots, eg:
  auto.mfrow(7, TRUE) will set up a device with 3x3 spaces,
  then when auto.mfrow(7, FALSE) is called after the plots
  have been made, 2 blank plots will then be 'printed'. NB,
  you must call auto.mfrow TWICE, once before plotting, and
  once after plotting, UNLESS you know for sure that the
  nplots specified will fill all of the spaces.
}
\author{
  Mark Cowley, 3 June 2006
}

