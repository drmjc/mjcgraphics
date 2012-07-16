#' Venn Diagrams
#' 
#' Plot classification counts in Venn diagram.
#' This is a replacement for \code{\link[limma]{vennDiagram}} from \code{limma},
#' with \code{xlim} and \code{ylim} changed in the plot call, and
#' \code{mar=par("mar")} to get rid of all of the whitespace around the box
#' 
#' @param object either a \code{TestResults} matrix or a \code{VennCounts}
#'  object produced by \code{\link[limma]{vennCounts}}.
#' @param include character string, of length one or two, specifying whether
#'    the diagram should give counts for genes up-regulated,
#'    down-regulated or both. See details.  Choices are \dQuote{both},
#'    \dQuote{up} or \dQuote{down}.
#' @param names optional character vector giving names for the sets or contrasts
#' @param mar numeric vector of length 4 specifying the width of the
#'      margins around the plot. This argument is passed to \code{\link{par}}. default = \code{par("mar")}
#' @param cex numerical value giving the amount by which the contrast names
#'    should be scaled on the plot relative to the default.plotting
#'    text. See \code{\link{par}}. default = \code{cex = 1.5}
#' @param circle.colours optional vector of color specifications defining the colors
#'         by which the circles should be drawn. See \code{\link{par}}. default =\code{rep("black", 10)}
#' @param text.colours optional vector of color specifications, of same length as
#'    \code{include}, defining the colors by which the counts should be
#'     drawn. See \code{\link{par}}. default=\code{rep("black", 10)}
#' @param \dots further arguments passed to \code{\link[limma]{vennDiagram}}
#' 
#' @return none.
#' 
#' @author Mark Cowley, 14 Dec 2005
#' 
#' @export
#' @importFrom limma vennCounts
#' 
vennDiagram2 <- function (object, include = "both", names, mar = par("mar"), cex = 1.5, circle.colours=rep("black", 10), text.colours=rep("black", 10), ...) {
	
    if (!is(object, "VennCounts"))
        object <- vennCounts(object, include = include)
    nsets <- ncol(object) - 1
    if (nsets > 3)
        stop("Can't plot Venn diagram for more than 3 sets") #'
    if (missing(names))
        names <- colnames(object)[1:nsets]
    counts <- object[, "Counts"]
    theta <- 2 * pi * (0:360)/360 # 1:360 leaves a small gap at 0 degrees, thus 0:360.
    xcentres <- list(0, c(-1, 1), c(-1, 1, 0))[[nsets]]
    ycentres <- list(0, c(0, 0), c(1/sqrt(3), 1/sqrt(3), -2/sqrt(3)))[[nsets]]
    r <- c(1.5, 1.5, 1.5)[nsets]
    xtext <- list(-1.2, c(-1.2, 1.2), c(-1.2, 1.2, 0))[[nsets]]
    ytext <- list(1.8, c(1.8, 1.8), c(2.4, 2.4, -3))[[nsets]]
    old.par <- par(mar = mar)
    on.exit(par(old.par))
    plot(x = 0, y = 0, type = "n",
         xlim = c(-3, 3),
         ylim = c(-2.5-(nsets==3), 2.5+(nsets==3)), xlab = "", ylab = "", axes = FALSE, ...)
    for (circle in 1:nsets) {
        lines(xcentres[circle] + r * cos(theta), ycentres[circle] +
            r * sin(theta), col=circle.colours[circle])
        text(xtext[circle], ytext[circle], names[circle], cex = cex, col=text.colours[circle])
    }
    switch(nsets, {
        rect(-3, -2.5, 3, 2.5)# xleft, ybottom, xright, ytop
        text(2.3, -2.1, counts[1], cex = cex)
        text(0, 0, counts[2], cex = cex)
    }, {
        rect(-3, -2.5, 3, 2.5)
        text(2.3, -2.1, counts[1], cex = cex)
        text(1.5, 0.1, counts[2], cex = cex)
        text(-1.5, 0.1, counts[3], cex = cex)
        text(0, 0.1, counts[4], cex = cex)
    }, {
        rect(-3, -3.5, 3, 3.5)
        text(2.5, -3, counts[1], cex = cex)
        text(0, -1.7, counts[2], cex = cex)
        text(1.5, 1, counts[3], cex = cex)
        text(0.75, -0.35, counts[4], cex = cex)
        text(-1.5, 1, counts[5], cex = cex)
        text(-0.75, -0.35, counts[6], cex = cex)
        text(0, 0.9, counts[7], cex = cex)
        text(0, 0, counts[8], cex = cex)
    })
    invisible()
}

#' Plot a Venn Diagram
#' 
#' Plot the overlap of 2 or 3 character vectors in a vennDiagram
#' 
#' x,y,z can be character vectors, or numeric vectors, but should all be of the
#' same type. note that decimal numbers are unsuitable.
#' 
#' @param x vectors of numerics or characters
#' @param y vectors of numerics or characters
#' @param z vectors of numerics or characters (can be NULL for a 2-way venn).
#' @param names The names with which to label each of the circles
#' @param mar the margin around the plot
#' @param population The total set of values from which, x, y, and z are a
#'   subset if left null, then the population is assumed to be the set {x,y,z}
#' @param \dots args passed onto vennDiagram2
#' @author Mark Cowley, 20 June 2005
#' @export
plot.venn <- function(x, y=NULL, z=NULL, names=NULL, mar=par("mar"), population=NULL, ...) {
    if( is.null(z) )
        plot.venn2D(x, y, names, mar, population, ...)
    else
        plot.venn3D(x, y, z, names, mar, population, ...)
}


#' Plot a 2 way venn diagram
#' 
#' Function to plot the overlap of 2  vectors in a vennDiagram
#' \code{x},\code{y} can be character vectors, or numeric vectors, but should all be of the
#' same type. note that decimal numbers are unsuitable.
#' 
#' @param x vectors of numerics or characters
#' @param y vectors of numerics or characters
#' @param names The names with which to label each of the circles
#' @param mar the margin around the plot
#' @param population The total set of values from which, \code{x}, and \code{y} are a
#'   subset. default=\code{NULL}, where the population is assumed to be the set \code{{x,y}}
#' @param \dots args passed onto vennDiagram2
#' @author Mark Cowley
#' 
#' @export
#' @importFrom limma vennCounts
#' 
plot.venn2D <- function(x, y=NULL, names=NULL, mar=c(2,2,2,2)+0.2, population=NULL, ...) {
    if( is.null(y) & is.list(x) & length(x) == 2) {
    	if( is.null(names) ) names <- names(x)
    	y <- x[[2]]
    	x <- x[[1]]
    }

    if(is.numeric(x)) x <- as.character(x)
    if(is.numeric(y)) y <- as.character(y)

    elements <- union(x, y)
    if(!is.null(population))
        elements <- union(elements, population)

    res <- matrix(rep(0,length(elements)*2), length(elements), 2)
    rownames(res) <- elements
    if( is.null(names) ) names <- c("x", "y")
    colnames(res) <- names
    res[match(x,rownames(res)), 1] <- 1
    res[match(y,rownames(res)), 2] <- 1

##     par(family="serif")
    vennDiagram2(vennCounts(res), mar=mar, cex.main=1.3, ...)
}

#' Plot a 3 way venn diagram
#' 
#' Function to plot the overlap of 3 vectors in a vennDiagram
#' \code{x},\code{y}, \code{z} can be character vectors, or numeric vectors, but should all be of the
#' same type. note that decimal numbers are unsuitable.
#' 
#' @param x vectors of numerics or characters
#' @param y vectors of numerics or characters
#' @param z vectors of numerics or characters
#' @param names The names with which to label each of the circles
#' @param mar the margin around the plot
#' @param population The total set of values from which, \code{x}, \code{y}, and \code{z} are a
#'   subset. default=\code{NULL}, where the population is assumed to be the set \code{{x,y,z}}
#' @param \dots args passed onto vennDiagram2
#' @author Mark Cowley
#' 
#' @export
#' @importFrom limma vennCounts
#' 
plot.venn3D <- function(x, y=NULL, z=NULL, names=NULL, mar=c(2,2,2,2)+0.2, population=NULL, ...) {
    if( is.null(y) & is.null(z) & is.list(x) & length(x) == 3) {
    	if( is.null(names) ) names <- names(x)
    	z <- x[[3]]
    	y <- x[[2]]
    	x <- x[[1]]
    }
    if(is.numeric(x)) x <- as.character(x)
    if(is.numeric(y)) y <- as.character(y)
    if(is.numeric(z)) z <- as.character(z)

    elements <- unionN(x,y,z)#(x, union(y, z))
    if(!is.null(population))
        elements <- union(elements, population)

    res <- matrix(rep(0,length(elements)*3), length(elements), 3)
    rownames(res) <- elements
    if( is.null(names) ) names <- c("x", "y", "z")
    colnames(res) <- names
    res[match(x,rownames(res)), 1] <- 1
    res[match(y,rownames(res)), 2] <- 1
    res[match(z,rownames(res)), 3] <- 1
##     par(family="serif")
    vennDiagram2(vennCounts(res), mar=mar, cex.main=1.3, ...)
}
