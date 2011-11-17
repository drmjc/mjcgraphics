## Plot a gamma distribution parameterised by shape and scale (or shape and rate)
##
## Mark Cowley, 19 July 2005
##
plot.gamma <- function(shape=1, rate=1, scale=1/rate, shift=0, add=F, col=1, n=10000, ...) {
	if( !add )
		plot(density(rgamma(n, shape=shape, rate=rate, scale=scale) + shift),
			 main=p("Gamma Distribution (shape, scale): (", shape, ", ", scale, ")"), ...)
	else
		lines(density(rgamma(n, shape=shape, rate=rate, scale=scale)+shift), col=col, ...)
}

## Plot a gamma distribution parameterised by mean and var.
##
## 'shift' is value which will shift the entire gamma distribution left or right
## along the x-axis.
## 'col' is for line colour
##
## Mark Cowley, 19 July 2005
##
plot.gamma2 <- function(mean=5, var=1, shift=0, add=F, col=1, n=10000, ...) {
	scale <- var/mean
	shape <- mean/scale

	if( !add )
		plot(density(rgamma2(n, mean=mean, var=var) + shift),
			 main=p("Gamma Distribution (mean, var, shape, scale):
(", paste(mean, var, shape, scale, sep=", "), ")"), ...)
	else
		lines(density(rgamma2(n, mean=mean, var=var)+shift), col=col, ...)
}


