\name{plot_blank}
\alias{plot_blank}
\title{Make a blank plot.}
\usage{
  plot_blank(main = NULL, sub = NULL, message = NULL,
    box = FALSE)
}
\arguments{
  \item{main}{you can optionally write a main title, and a
  sub-title}

  \item{sub}{you can optionally write a main title, and a
  sub-title}

  \item{message}{message to write in the middle of the
  plotting device}

  \item{box}{logical: draw a box around the plot?}
}
\description{
  This is useful if you have an automate plotting pipeline
  that occasionally can't make the intended plot but you
  still want the place where the plot is supposed to go to
  be filled.
}
\author{
  Mark Cowley, long time ago!
}

