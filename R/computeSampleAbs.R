

computeSampleAbs <- function(calCols, sampCol, PC1, calVals) {
	
	# Function to interpolate a sample color on the paint chip scale

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

##### Helper Functions

pointOnLineNearestPoint <- function(P1, P2, P3) {
	
	# This works in any dimension/space
	# Based on
	# stackoverflow.com/questions/9368960/perpendicular-point-on-line-from-3d-point?lq=1
	
	# P1, P2 are two points on the line.  P3 is the other point.
	# P4 will be the answer; all points given as c(x, y, z, ...)
	
	a <- (P2 - P3) %*% (P2 - P1)
	b <- -1 * (P1 - P3) %*% (P2 - P1)
	P4 <- a*P1 + b*P2
	P4
	}

dAB <- function(A, B) { # distance between pts A & B in 3d
	# A, B each as c(x, y, z)
	dAB <- sqrt((B[3] - A[3])^2 + (B[2]-A[2])^2 + (B[1]-A[1])^2)
	}

##### End of Helper Functions

	# Version 0.2 Approach:
	# Sort the calCols, then map the 1st and last colors
	# onto PC1, then interpolate the sampCol
		
	# Find the distances between the calCols and pure white
	# Based upon color.id {plotrix}
    sc <- col2rgb(sampCol)
    cc <- col2rgb(calCols)
    whdist <- apply(cc, 2, function(z) {sqrt(sum((z - 255)^2))})
	whdist <- data.frame(dist = whdist,
		red = cc[1,], green = cc[2,], blue = cc[3,])
	# Sort from lightest to darkest:
	whdist <- arrange(whdist, dist)
	whdist[,2:4] <- whdist[,2:4]/255
	# Everything is in 0...1 units
	
	# Find the projection of the lightest & darkest points
	#  as well as the sampCol onto PC1:
	ncc <- nrow(whdist)
	whtpt <- as.numeric(whdist[1,2:4])
	dkpt <- as.numeric(whdist[ncc,2:4])
	sampt <- as.numeric(sc/255)
	lite <- pointOnLineNearestPoint(PC1[1,], PC1[2,], whtpt)
	dark <- pointOnLineNearestPoint(PC1[1,], PC1[2,], dkpt)
	samp <- pointOnLineNearestPoint(PC1[1,], PC1[2,], sampt)
	# Because PCs are abstract, the coord of lite & dark
	# might be out of the rgb color space (?)

	distld <- dAB(dark, lite)
	distls <- dAB(samp, lite)
	D <- diff(calVals)*distls/distld
	return(D)
	
	}
