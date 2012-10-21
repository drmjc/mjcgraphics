\name{Customized plotting devices}
\alias{Customized plotting devices}
\alias{dev.off}
\alias{jpeg}
\alias{jpeg.Apple27}
\alias{jpeg.QSXGA}
\alias{jpeg.QUXGA}
\alias{jpeg.QVGA}
\alias{jpeg.QXGA}
\alias{jpeg.SVGA}
\alias{jpeg.SXGA}
\alias{jpeg.UXGA}
\alias{jpeg.VGA}
\alias{jpeg.XGA}
\alias{pdf}
\alias{pdf.A4}
\alias{pdf.A4.portrait}
\alias{pdf.A5}
\alias{pdf.Apple27}
\alias{plotting.devices}
\alias{png}
\alias{png.Apple27}
\alias{png.QSXGA}
\alias{png.QUXGA}
\alias{png.QVGA}
\alias{png.QXGA}
\alias{png.SVGA}
\alias{png.SXGA}
\alias{png.UXGA}
\alias{png.VGA}
\alias{png.XGA}
\title{Customized plotting devices}
\usage{
  png(filename = "Rplot\%03d.png", width = 1200,
    height = 800, ...)

  jpeg(filename = "Rplot\%03d.jpeg", width = 1200,
    height = 800, ...)

  pdf(filename, width = 11.69, height = 8.27,
    onefile = TRUE, version = "1.4", paper = "special",
    do.par = TRUE, bg = "white", ...)

  dev.off(open = capabilities("aqua"))

  png.VGA(filename = "Rplot\%03d.png", width = 640,
    height = 480, pointsize = 12, bg = "white", res = NA,
    ...)

  png.SVGA(filename = "Rplot\%03d.png", width = 800,
    height = 600, pointsize = 12, bg = "white", res = NA,
    ...)

  png.XGA(filename = "Rplot\%03d.png", width = 1024,
    height = 768, pointsize = 12, bg = "white", res = NA,
    ...)

  png.QVGA(filename = "Rplot\%03d.png", width = 1280,
    height = 960, pointsize = 12, bg = "white", res = NA,
    ...)

  png.SXGA(filename = "Rplot\%03d.png", width = 1400,
    height = 1050, pointsize = 12, bg = "white", res = NA,
    ...)

  png.UXGA(filename = "Rplot\%03d.png", width = 1600,
    height = 1200, pointsize = 12, bg = "white", res = NA,
    ...)

  png.QSXGA(filename = "Rplot\%03d.png", width = 2048,
    height = 1536, pointsize = 12, bg = "white", res = NA,
    ...)

  png.QXGA(filename = "Rplot\%03d.png", width = 2800,
    height = 2100, pointsize = 12, bg = "white", res = NA,
    ...)

  png.QUXGA(filename = "Rplot\%03d.png", width = 3200,
    height = 2400, pointsize = 12, bg = "white", res = NA,
    ...)

  png.Apple27(filename = "Rplot\%03d.png", width = 2520,
    height = 1340, pointsize = 12, bg = "white", res = NA,
    ...)

  jpeg.VGA(filename = "Rplot\%03d.jpeg", width = 640,
    height = 480, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.SVGA(filename = "Rplot\%03d.jpeg", width = 800,
    height = 600, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.XGA(filename = "Rplot\%03d.jpeg", width = 1024,
    height = 768, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.QVGA(filename = "Rplot\%03d.jpeg", width = 1280,
    height = 960, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.SXGA(filename = "Rplot\%03d.jpeg", width = 1400,
    height = 1050, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.UXGA(filename = "Rplot\%03d.jpeg", width = 1600,
    height = 1200, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.QXGA(filename = "Rplot\%03d.jpeg", width = 2048,
    height = 1536, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.QSXGA(filename = "Rplot\%03d.jpeg", width = 2800,
    height = 2100, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.Apple27(filename = "Rplot\%03d.jpeg", width = 2520,
    height = 1340, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  jpeg.QUXGA(filename = "Rplot\%03d.jpeg", width = 3200,
    height = 2400, pointsize = 12, quality = 75,
    bg = "white", res = NA, ...)

  pdf.A4(file, onefile = TRUE, version = "1.4", ...)

  pdf.A4.portrait(file, onefile = TRUE, version = "1.4",
    ...)

  pdf.A5(file, onefile = TRUE, version = "1.4", ...)

  pdf.Apple27(file, onefile = TRUE, version = "1.4", ...)
}
\arguments{
  \item{filename}{the name of the output file}

  \item{width}{the width of the device in pixels for jpeg
  or png, and inches for pdf}

  \item{height}{the height of the device in pixels for jpeg
  or png, and inches for pdf}

  \item{\dots}{passed to \code{\link[grDevices]{png}},
  \code{\link[grDevices]{jpeg}},
  \code{\link[grDevices]{pdf}} from \code{grDevices}}

  \item{onefile}{logical: if \code{TRUE} (the default)
  allow multiple figures in one file. If \code{FALSE},
  generate a file with name containing the page number for
  each page.}

  \item{version}{a string describing the PDF version that
  will be required to view the output. This is a minimum,
  and will be increased (with a warning) if necessary.
  Defaults to \dQuote{1.4}, but see \sQuote{Details} in
  \code{\link[grDevices]{pdf}}}

  \item{paper}{the target paper size. The choices are
  \dQuote{a4}, \dQuote{letter}, \dQuote{legal} (or
  \dQuote{us}) and \dQuote{executive} (and these can be
  capitalized), or \dQuote{a4r} and \dQuote{USr} for
  rotated (\sQuote{landscape}). The default is
  \dQuote{special}, which means that the \sQuote{width} and
  \sQuote{height} specify the paper size. A further choice
  is \dQuote{default}; if this is selected, the papersize
  is taken from the option \dQuote{papersize} if that is
  set and as \dQuote{a4} if it is unset or empty. Defaults
  \dQuote{special}.}

  \item{do.par}{logical: setup the plotting parameters?}

  \item{bg}{the background color. default = \dQuote{white},
  which overrides the default setting of transparent.}

  \item{open}{logical: open the recent file? Defaults to
  \code{TRUE} if in an interactive session (see
  \code{\link{interactive}})}
}
\value{
  device opening functions return nothing. \code{dev.off}
  prints the file path and opens the previously opened file
  if in an interactive session.
}
\description{
  The \code{jpeg}, \code{png} or \code{pdf} functions are
  redefined versions of the standard ones implemented in
  grDevices: - changes the default size rotates the axis
  labels (las=1)\cr - pushes the label out an extra line\cr
  - saves the filename to an environment variable, so that
  dev.off() can find it, and open it.\cr \code{dev.off}
  closes the current device, and opens the newly created
  image file. The mechanism behind this is via the setting
  of an environment variable \code{CURRENT.PLOT.FILENAME},
  which gets set by one of the \code{jpeg}, \code{png} or
  \code{pdf} functions.
}
\details{
  In addition, there are predefined versions for creating
  standard sized jpeg / png devices:\cr \tabular{ll}{ VGA
  \tab 640x480 \cr SVGA \tab 800x600 \cr XGA \tab 1024x768
  \cr QVGA \tab 1280x960 \cr SXGA+ \tab 1400x1050 \cr UXGA
  \tab 1600x1200 \cr QXGA \tab 2048x1536 \cr QSXGA+ \tab
  2800x2100 \cr QUXGA \tab 3200x2400 \cr WXGA \tab 1280x800
  \cr \cr SXGA \tab 1280x1024 \cr WSXGA+ \tab 1680x1050 \cr
  WUXGA \tab 1920x1200 \cr QSXGA \tab 2560x2048 \cr } See
  \url{http://www.i386.info/vidres.htm} for more info.

  In addition there are routines for opening A4 & A5
  portrait/landscape pdf & CairoPDF devices.
}
\note{
  i've made the Cairo* etc plots defunct, since i never use
  them & it's not worth adding the libcairo dependency.

  \code{dev.off}: default for open is
  \code{capabilities("aqua")}, since this is only true when
  running locally on a mac. There must be a more precise
  way of doing this. Note \code{capabilities("X11")} seems
  a logical choice, but this is \code{TRUE} with X11
  forwarding, when issuing a \code{system("open ...")}
  command will fail.
}
\section{Apple27}{
  The \code{\link{png.Apple27}},
  \code{\link{jpeg.Apple27}}, \code{\link{pdf.Apple27}}
  functions are designed to create images that can be
  opened in Preview, and will be close to the maximum
  possible size on an Apple 27" LED Cinema/Thunderbolt
  Display. Note, Preview doesn't seem to want to
  automatically fill the entire screen from edge to edge,
  thus these are the largest sizes that preview is
  comforable opening at using \sQuote{View > Actual Size}.
}
\author{
  Mark Cowley, 2009-06-10
}

