#' Function to wrap long titles (from plots) by splitting at 'limit' characters
#' and inserting new lines
#' 
#' @author Mark Cowley, 2009-03-23
#' @export
wrap.title <- function(text, limit=28, maxrow=3) {
	lens <- nchar(text)
	if( all(lens < limit) )
		text
	else {
		text <- str_left(text, limit*maxrow)
		a <- str_left(text, limit)
		b <- substring(text, limit+1, limit*2)
		c <- substring(text, limit*2+1, limit*3)
		res <- paste(a,b,c,collapse="\\n")
	}
	res
}
