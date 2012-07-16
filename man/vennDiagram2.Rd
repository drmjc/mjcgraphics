\name{vennDiagram2}
\alias{vennDiagram2}
\title{Venn Diagrams}
\usage{
  vennDiagram2(object, include = "both", names,
    mar = par("mar"), cex = 1.5,
    circle.colours = rep("black", 10),
    text.colours = rep("black", 10), ...)
}
\arguments{
  \item{object}{either a \code{TestResults} matrix or a
  \code{VennCounts} object produced by
  \code{\link[limma]{vennCounts}}.}

  \item{include}{character string, of length one or two,
  specifying whether the diagram should give counts for
  genes up-regulated, down-regulated or both. See details.
  Choices are \dQuote{both}, \dQuote{up} or \dQuote{down}.}

  \item{names}{optional character vector giving names for
  the sets or contrasts}

  \item{mar}{numeric vector of length 4 specifying the
  width of the margins around the plot. This argument is
  passed to \code{\link{par}}. default = \code{par("mar")}}

  \item{cex}{numerical value giving the amount by which the
  contrast names should be scaled on the plot relative to
  the default.plotting text. See \code{\link{par}}. default
  = \code{cex = 1.5}}

  \item{circle.colours}{optional vector of color
  specifications defining the colors by which the circles
  should be drawn. See \code{\link{par}}. default
  =\code{rep("black", 10)}}

  \item{text.colours}{optional vector of color
  specifications, of same length as \code{include},
  defining the colors by which the counts should be drawn.
  See \code{\link{par}}. default=\code{rep("black", 10)}}

  \item{\dots}{further arguments passed to
  \code{\link[limma]{vennDiagram}}}
}
\value{
  none.
}
\description{
  Plot classification counts in Venn diagram. This is a
  replacement for \code{\link[limma]{vennDiagram}} from
  \code{limma}, with \code{xlim} and \code{ylim} changed in
  the plot call, and \code{mar=par("mar")} to get rid of
  all of the whitespace around the box
}
\author{
  Mark Cowley, 14 Dec 2005
}

