

selectCalCols <- function(wedge, nDiv = 10, pcpd = 1, divMode = "linear") {
	
	# Bryan Hanson, DePauw University, May 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Function to process CIE xy colors selected by the user and
	# reduce them in a sensible way to a limited no. of values for
	# use as calibration colors
	
	# wedge is the data coming from selectCIExy (see below)
	# nDiv is the no. of divisions to create
	# pcpd is the no. of paint chips per division
	# total paint chips = nDiv * pcpd
	# divMode = the scheme used to divide up the wedge into parts
	
	# consider adding a plot showing the results

##### Helper functions

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

	dAB <- function(A, B) { # distance between pts A & B
		# A, B each as c(x, y)
		dAB <- sqrt((B[2]-A[2])^2 + (B[1]-A[1])^2)
		}

##### End of helper functions

	# wedge <- vector("list")
	# wedge$wavelength <- c(L1, L2) # input wavelengths
	# wedge$Case <- Case
	# wedge$verts <- verts # vertices of wedge
	# wedge$CIEcols <- bgr # colors in wedge
	# wedge$colSpace <- colSpace
	# wedge$ff <- ff
	# wedge$p4 <- p4 # see above for defs of these pts
	# wedge$p5 <- p5
	# wedge$p6 <- p6
	# wedge$xPt <- xPt

	### Prepare the starting data
	D65 <- unlist(getWhiteValues("D65"))
	p4 <- unlist(wedge$p4)
	p5 <- unlist(wedge$p5)
	p6 <- unlist(wedge$p6)
	xPt <- unlist(wedge$xPt)
	
	### Divide the wedge up into nDiv bands using divMode
	
	if (divMode == "linear") { # divide the radius up into equal parts
		
		# Divide up the distance from D65 to xPt equally
		div <- (sqrt((xPt[1] - D65[1])^2 + (xPt[2] - D65[2])^2))/nDiv
		rad <- seq(0, nDiv*div, by = div)
		rad <- rad[-1]
		} # end of divMode linear
	
	### Figure out band vertices

	ang1 <- ang0to2pi(c(D65[1], D65[2], p4[1], p4[2]))
	ang2 <- ang0to2pi(c(D65[1], D65[2], p6[1], p6[2]))
	angs <- sort(c(ang1, ang2))
	myby <- 2*pi/360	
	if ((ang2-ang1) > pi) {
		cat("weird\n")
		angs[2] <- -1*(2*pi - angs[2])
		myby <- -1*myby
		}
	angs <- seq(angs[1], angs[2], by = myby)
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
	bands[[1]]$x <- c(bands[[1]]$x, D65[1]) # fix 1st band
	bands[[1]]$y <- c(bands[[1]]$y, D65[2])
	
	bands

	### Next: Sample colors within each band

	} # end of function
