#' plot the print characters
#'
#' @param file.name the path to a pdf destination file, or \code{NULL}
#' @return none. makes a plot
#' @author Mark Cowley, 2012-07-16
#' @export
plot.pch <- function(file.name=NULL) {
    if(!is.null(file.name))
        pdf.A4(file.name)

    plot(1:10,rep(5,10), ylim=c(0,5), pch=c(1:10), xaxt="n", yaxt="n",xlab="",ylab="", main="Print Characters")
    points(1:10,rep(4,10), pch=c(11:20))
    points(1:10,rep(3,10), pch=c(21:30))
    points(1:10,rep(2,10), pch=c(31:40))
    points(1:10,rep(1,10), pch=c(41:50))
    points(1:10,rep(0,10), pch=c(51:60))
    par(col="red")
    text(1:10,rep(5,10),labels=as.character(c(1:10)), pos=2)
    text(1:10,rep(4,10),labels=as.character(c(11:20)), pos=2)
    text(1:10,rep(3,10),labels=as.character(c(21:30)), pos=2)
    text(1:10,rep(2,10),labels=as.character(c(31:40)), pos=2)
    text(1:10,rep(1,10),labels=as.character(c(41:50)), pos=2)
    text(1:10,rep(0,10),labels=as.character(c(51:60)), pos=2)

    if(!is.null(file.name))
        dev.off()
}
