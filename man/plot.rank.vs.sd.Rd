\name{plot.rank.vs.sd}
\alias{plot.rank.vs.sd}
\title{plot rank vs stdev}
\usage{
  plot.rank.vs.sd(x, main = "", lowess.col = "purple")
}
\arguments{
  \item{x}{either a numeric matrix/data.frame, or an
  AffyBatch, or LumiBatch object (from lumi package)}

  \item{main}{the plot title}

  \item{lowess.col}{the colour of the lowess line}
}
\value{
  none. makes a plot
}
\description{
  For a matrix of data (eg gene expression data) for each
  row (gene), plot the rank of the mean vs the stdev of
  that row.
}
\author{
  Mark Cowley, 2009-05-27
}

