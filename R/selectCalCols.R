

selectCalCols <- function(wedge, nDiv = 10, pcpd = 1, divMode = "linear") {
	
	# Bryan Hanson, DePauw University, May 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Function to process CIE xy colors selected by the user and
	# reduce them in a sensible way to a limited no. of values for
	# use as calibration colors
	
	# wedge is the data coming from selectCIExy
	# nDiv is the no. of divisions to create
	# pcpd is the no. of paint chips per division
	# total paint chips = nDiv * pcpd
	# divMode = the scheme used to divide up the wedge into parts
	
	# consider adding a plot showing the results

	### Helper function

	ang0to2pi <- function(segment) {
		# segment given as c(x1, y1, x2, y2)
		# compute angle relative to horiz axis
		# using the usual unit circle conventions
		# the segments need not intersect using this method
		seg1 <- c(segment[3]-segment[1], segment[4]-segment[2])
		theta <- atan2(seg1[2], seg1[1])
		if (theta < 0) theta <- theta + 2*pi
		return(theta)
		}

	# Assemble a list for return
	# wedge <- vector("list")
	# wedge$CIEcols <- bgr
	# wedge$verts <- verts
	# wedge$wavelength <- c(L1, L2)
	# wedge$p4 <- p4
	# wedge$p5 <- p5
	# wedge$p6 <- p6
	# wedge$colSpace <- colSpace
	# wedge$ff <- ff

	# Prepare the starting data
	D65 <- unlist(getWhiteValues("D65"))
	p4 <- unlist(wedge$p4)
	p5 <- unlist(wedge$p5)
	p6 <- unlist(wedge$p6)
	
	### Divide the wedge up into nDiv bands using divMode
	
	if (divMode == "linear") { # divide the radius up into equal parts
		
		# First stab, use midpoint p5 (won't work in many cases)
		# Get distance along radius for each band
		div <- (sqrt((p5[1] - D65[1])^2 + (p5[2] - D65[2])^2))/nDiv
		rad <- seq(0, nDiv*div, by = div)
		rad <- rad[-1]
		} # end of divMode linear
	
	### Figure out band vertices

	ang1 <- ang0to2pi(c(D65[1], D65[2], p4[1], p4[2]))
	ang2 <- ang0to2pi(c(D65[1], D65[2], p6[1], p6[2]))
	# cat("angle 1 =", ang1, "\n")
	# cat("angle 2 =", ang2, "\n")
	angs <- sort(c(ang1, ang2))
	angs <- seq(angs[1], angs[2], by = 2*pi/360)
	# angles along arc every 1 degree
	# ang2 always > ang1

	bands <- vector(mode = "list", length = nDiv)
	for (i in 1:nDiv) {
		bands[[i]]$x <- D65[1] + rad[i]*cos(angs)
		bands[[i]]$y <- D65[2] + rad[i]*sin(angs)
		}
	# At this point, we have the coords of each arc
	# Now append the coords needed to complete each polygon
	# describing the band; this is the i-1 arc
	for (i in nDiv:2) {
		bands[[i]]$x <- c(bands[[i]]$x, rev(bands[[i-1]]$x))
		bands[[i]]$y <- c(bands[[i]]$y, rev(bands[[i-1]]$y))
		}
	bands[[1]]$x <- c(bands[[1]]$x, D65[1]) # fix 1st one
	bands[[1]]$y <- c(bands[[1]]$y, D65[2])
	
#	str(bands)	

	bands

	# Next: Sample colors within each band

	} # end of function
