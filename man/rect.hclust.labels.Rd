\name{rect.hclust.labels}
\alias{rect.hclust.labels}
\title{Annotate the rectangles produced by rect.hclust using numbers.}
\usage{
  rect.hclust.labels(tree, k = NULL, which = NULL,
    x = NULL, h = NULL, border = 2, cluster = NULL)
}
\arguments{
  \item{tree}{see rect.hclust. NB this must be identical to
  those used in your call to rect.hclust}

  \item{k}{see rect.hclust. NB this must be identical to
  those used in your call to rect.hclust}

  \item{which}{see rect.hclust. NB this must be identical
  to those used in your call to rect.hclust}

  \item{x}{see rect.hclust. NB this must be identical to
  those used in your call to rect.hclust}

  \item{h}{see rect.hclust. NB this must be identical to
  those used in your call to rect.hclust}

  \item{border}{see rect.hclust. NB this must be identical
  to those used in your call to rect.hclust}

  \item{cluster}{see rect.hclust. NB this must be identical
  to those used in your call to rect.hclust}
}
\value{
  annotates a plot with indices
}
\description{
  Why? plclust is great for plotting heirarchical clusters;
  rect.hclust is great for selecting clusters at a given
  distance threshold; what is lacking is knowing which
  clusters are which. This is espescially useful if you
  then do some downstream analysis on each cluster, if you
  end up referring to your clusters numerically.
}
\examples{
m <- matrix(rnorm(200), nrow=20)
hc <- hclust(dist(m))
plot(hc)
rect.hclust(hc, k=5)
rect.hclust.labels(hc, k=5)
}
\author{
  Mark Cowley, 2009-10-30
}

