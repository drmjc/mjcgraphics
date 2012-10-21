\name{plot_rho}
\alias{plot_rho}
\title{rank-based correlation heatmap}
\usage{
  plot_rho(x, do.mfrow = TRUE,
    name = paste(ncol(x), "arrays"), col = 1)
}
\arguments{
  \item{x}{A \code{matrix} or \code{data.frame} of data,
  with samples in column (eg gene expression data),
  \emph{or} a pre-computed correlation matrix (which must
  be square with a diagonal of 1's)}

  \item{name}{Used for naming the plots appropriately}

  \item{do.mfrow}{logical: \code{TRUE} means call
  \code{par(mfrow=c(1, 2))}; \code{FALSE} means don't set
  the graphical layout}

  \item{col}{the colour to use. It will be recycled if
  necessary}
}
\value{
  invisibly returns the average correlation of each column
}
\description{
  Work out the correlation of N samples using Spearmans
  rho, then plot these correlations as a heatmap, and
  return the average correlation of each sample
}
\author{
  Mark Cowley, 9 November 2005
}

