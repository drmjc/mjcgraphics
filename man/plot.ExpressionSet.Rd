\name{plot.ExpressionSet}
\alias{plot.ExpressionSet}
\title{plot a single feature from an ExpressionSet}
\usage{
  \method{plot}{ExpressionSet} (x, feature = 1,
    xlab = "Rank", ylab = "Expression Level (log2)",
    main = featureNames(x)[feature], add.mean = TRUE,
    sort = TRUE, samples = NULL)
}
\arguments{
  \item{x}{an ExpressionSet}

  \item{feature}{the index or name of the feature to plot.
  default=1}

  \item{xlab}{see par}

  \item{ylab}{see par}

  \item{main}{see par}

  \item{add.mean}{logical: add a horizontal line about the
  mean of that feature}

  \item{sort}{logical: if \code{TRUE}, then sort from low
  to high}

  \item{samples}{an optional character vector of sample
  names to highlight and label in red.}
}
\value{
  nothing
}
\description{
  Plot a single probe, aka feature from an ExpressionSet
  object, optionally highlighting certain samples, and
  adding a mean expression line.
}
\examples{
\dontrun{
	hent3 <- x["SLC29A3", ]
	plot(hent3, sort=T, samples="APGI_1966")
	plot(x, feature="SLC29A3", sort=T, samples="APGI_1966")
}
}
\author{
  Mark Cowley, 2012-10-16
}

