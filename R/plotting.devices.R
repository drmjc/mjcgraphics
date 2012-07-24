#' Customized plotting devices
#' 
#' The \code{jpeg}, \code{png} or \code{pdf} functions are redefined versions
#' of the standard ones implemented in grDevices:
#' - changes the default size rotates the axis labels (las=1)\cr
#' - pushes the label out an extra line\cr
#' - saves the filename to an environment variable, so that dev.off() can 
#' find it, and open it.\cr
#' \code{dev.off} closes the current device, and opens the newly created
#' image file. The mechanism behind this is via the setting of an
#' environment variable \code{CURRENT.PLOT.FILENAME}, which gets set
#' by one of the \code{jpeg}, \code{png} or \code{pdf} functions.
#' 
#' In addition, there are predefined versions for creating standard sized 
#' jpeg / png devices:\cr
#' \tabular{ll}{
#'	 VGA \tab 640x480 \cr
#'	 SVGA \tab 800x600 \cr
#'	 XGA \tab 1024x768 \cr
#'	 QVGA \tab 1280x960 \cr
#'	 SXGA+ \tab 1400x1050 \cr
#'	 UXGA \tab 1600x1200 \cr
#'	 QXGA \tab 2048x1536 \cr
#'	 QSXGA+ \tab 2800x2100 \cr
#'	 QUXGA \tab 3200x2400 \cr
#'	 WXGA \tab 1280x800 \cr \cr
#'	 SXGA \tab 1280x1024 \cr
#'	 WSXGA+ \tab 1680x1050 \cr
#'	 WUXGA \tab 1920x1200 \cr
#'	 QSXGA \tab 2560x2048 \cr
#' }
#' See \url{http://www.i386.info/vidres.htm} for more info.
#' 
#' In addition there are routines for opening A4 & A5 portrait/landscape pdf 
#' & CairoPDF devices.
#' 
#' @note i've made the CairoPDF.A4 etc plots defunct, since i never use them &
#' it's not worth adding the Cairo depedency
#' 
#' @return device opening functions return nothing. \code{dev.off} prints the
#'	file path and opens the previously opened file if in an interactive session.
#' 
#' @param filename the name of the output file
#' @param width the width of the device in pixels for jpeg or png, and inches for pdf
#' @param height the height of the device in pixels for jpeg or png, and inches for pdf
#' @param \dots passed to \code{\link[grDevices]{png}}, \code{\link[grDevices]{jpeg}}, 
#'		\code{\link[grDevices]{pdf}} from \code{grDevices}
#' @param onefile logical: if \code{TRUE} (the default) allow multiple figures in one
#'				file.	If \code{FALSE}, generate a file with name containing the
#'				page number for each page.
#' @param version a string describing the PDF version that will be required to
#'				view the output.	This is a minimum, and will be increased
#'				(with a warning) if necessary.	Defaults to \dQuote{1.4}, but see
#'				\sQuote{Details} in \code{\link[grDevices]{pdf}}
#' @param paper the target paper size.	The choices are \dQuote{a4}, \dQuote{letter},
#'		\dQuote{legal} (or \dQuote{us}) and \dQuote{executive} (and these can be
#'		capitalized), or \dQuote{a4r} and \dQuote{USr} for rotated
#'		(\sQuote{landscape}).	The default is \dQuote{special}, which means that
#'		the \sQuote{width} and \sQuote{height} specify the paper size.	A further
#'		choice is \dQuote{default}; if this is selected, the papersize is
#'		taken from the option \dQuote{papersize} if that is set and as
#'		\dQuote{a4} if it is unset or empty.	Defaults \dQuote{special}.
#' @param do.par logical: setup the plotting parameters?
#' @param bg the background color. default = \dQuote{white}, which overrides the 
#'		default setting of transparent.
#' @param open logical: open the recent file? Defaults to \code{TRUE} 
#'	 if in an interactive session (see \code{\link{interactive}})
#' @inheritParams png
#' 
#' @author Mark Cowley, 2009-06-10
#' @rdname plotting.devices
#' @aliases plotting.devices
#' @name Customized plotting devices
#' @importFrom grDevices png jpeg pdf dev.off
NULL

#' @export
#' @rdname plotting.devices
png <- function(filename = "Rplot%03d.png", width = 1200, height = 800, ...) {
	grDevices::png(filename=filename, width=width, height=height, ...)
	par(las=1, mgp=c(4,1,0), mar=par()$mar + c(1,1,0,0))
	options(CURRENT.PLOT.FILENAME=filename)
	# assign("CURRENT.PLOT.FILENAME", filename, pos=grep("pwbc", searchpaths()))
}


#' @export
#' @rdname plotting.devices
jpeg <- function(filename = "Rplot%03d.jpeg", width = 1200, height = 800, ...) {
	grDevices::jpeg(filename=filename, width=width, height=height, ...)
	par(las=1, mgp=c(4,1,0), mar=par()$mar + c(1,1,0,0))
	options(CURRENT.PLOT.FILENAME=filename)
	# assign("CURRENT.PLOT.FILENAME", filename, pos=grep("pwbc", searchpaths()))
}


#' @export
#' @rdname plotting.devices
pdf <- function (filename, width=11.69, height=8.27, onefile = TRUE, version = "1.4", paper="special", do.par=TRUE, bg="white", ...) {
	grDevices::pdf(filename, width = width, height = height, onefile = onefile, 
		version = version, paper = paper, bg=bg, ...)
	if( do.par ) par(las=1, mgp=c(4,1,0), mar=par()$mar + c(1,1,0,0))
	options(CURRENT.PLOT.FILENAME=filename)
	# assign("CURRENT.PLOT.FILENAME", filename, pos=grep("pwbc", searchpaths()))
}


#' @note \code{dev.off}: default for open is \code{capabilities("X11")}, since when running
#' on a server, without X11 forwarding, you can't open the newly
#' created file. There may be a more precise way of doing this.
#' 
#' @export
#' @rdname plotting.devices
dev.off <- function(open=capabilities("X11")) {
	dv <- dev.cur()
	grDevices::dev.off()
	if( open && (! dv %in% c("quartz", "X11", "X11cairo") ) ) {
		f <- getOption("CURRENT.PLOT.FILENAME")
		if( !is.na(f) && !is.null(f) && file.exists(f) ) {
			# f <- get.full.path(f)
			f <- normalizePath(f)
			system(paste("open", shQuote(f)))
			# f <- sub("/Volumes/PWBC/private", "/pwbc", f)
			cat(paste("# ", f, "\n", sep=""))
		}
		options(CURRENT.PLOT.FILENAME=NA)
	}
}


#############	 PNG	###################

#' @export
#' @rdname plotting.devices
png.VGA <- function(filename="Rplot%03d.png", width=640, height=480,
					 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.SVGA <- function(filename="Rplot%03d.png", width=800, height=600,
					 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.XGA <- function(filename="Rplot%03d.png", width=1024, height=768,
					 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.QVGA <- function(filename="Rplot%03d.png", width=1280, height=960,
					 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.SXGA <- function(filename="Rplot%03d.png", width=1400, height=1050,
					 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.UXGA <- function(filename="Rplot%03d.png", width=1600, height=1200,
					 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.QSXGA <- function(filename="Rplot%03d.png", width=2048, height=1536,
						pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.QXGA <- function(filename="Rplot%03d.png", width=2800, height=2100,
									pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
png.QUXGA <- function(filename="Rplot%03d.png", width=3200, height=2400,
						 pointsize = 12, bg = "white", res = NA,...) {
	mjcgraphics::png(filename=filename, width=width, height=height, pointsize=pointsize, bg=bg, res=res, ...)
}



###################		JPEG	 #############################

#' @export
#' @rdname plotting.devices
jpeg.VGA <- function(filename="Rplot%03d.jpeg", width=640, height=480,
					 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.SVGA <- function(filename="Rplot%03d.jpeg", width=800, height=600,
					 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.XGA <- function(filename="Rplot%03d.jpeg", width=1024, height=768,
					 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.QVGA <- function(filename="Rplot%03d.jpeg", width=1280, height=960,
					 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.SXGA <- function(filename="Rplot%03d.jpeg", width=1400, height=1050,
					 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.UXGA <- function(filename="Rplot%03d.jpeg", width=1600, height=1200,
					 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.QXGA <- function(filename="Rplot%03d.jpeg", width=2048, height=1536,
						pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.QSXGA <- function(filename="Rplot%03d.jpeg", width=2800, height=2100,
									pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}


#' @export
#' @rdname plotting.devices
jpeg.QUXGA <- function(filename="Rplot%03d.jpeg", width=3200, height=2400,
						 pointsize = 12, quality = 75, bg = "white", res = NA,...) {
	mjcgraphics::jpeg(filename=filename, width=width, height=height, pointsize=pointsize, quality=quality, bg=bg, res=res, ...)
}

#' @export
#' @rdname plotting.devices
pdf.A4 <- function(file, onefile=TRUE, version="1.4", ...) {
	mjcgraphics::pdf(file, , onefile=onefile, version=version, paper="a4r", ...)
}


#' @export
#' @rdname plotting.devices
pdf.A4.portrait <- function(file, onefile=TRUE, version="1.4", ...) {
	mjcgraphics::pdf(file, width=8.27, height=11.69, onefile=onefile, version=version, paper="a4", ...)
}

#' @export
#' @rdname plotting.devices
pdf.A5 <- function(file, onefile=TRUE, version="1.4", ...) {
	mjcgraphics::pdf(file, width=5.845, height=4.135, onefile=onefile, version=version, ...)
}


# #' @export
# #' @rdname plotting.devices
# CairoPDF.A4 <- function(file, onefile=TRUE, version="1.4", ...) {
# 	require(Cairo)
# 	CairoPDF(file, width=11.69, height=8.27, onefile=onefile, version=version, paper="a4r", ...)
# }
# 
# #' @export
# #' @rdname plotting.devices
# CairoPDF.A4.portrait <- function(file, onefile=TRUE, version="1.4", ...) {
# 	require(Cairo)
# 	CairoPDF(file, width=8.27, height=11.69, onefile=onefile, version=version, paper="a4", ...)
# }
# 
# #' @export
# #' @rdname plotting.devices
# CairoPDF.A5 <- function(file, onefile=TRUE, version=version, ...) {
# 	require(Cairo)
# 	CairoPDF(file, width=5.845, height=4.135, onefile=onefile, version=version, ...)
# }
