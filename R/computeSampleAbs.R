

computeSampleAbs <- function(calCols, sampCol, sampName) {
	
	# Function to interpolate a sample color on the paint chip scale

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Version 0.1 Approach:
	# Of all the calibration paint chips, find the two that
	# are closest to the sample color, and then interpolate
	# between these two points.
	
	# Find the distances between the calCols and sampCol
	# Based upon color.id {plotrix}
    sc <- col2rgb(sampCol)
    coltab <- col2rgb(calCols)
    # distances between each paint chip and the sample:
    cdist <- apply(coltab, 2, function(z) {sqrt(sum((z - sc)^2))})
	cdist2 <- data.frame(entry = 1:ncol(coltab), dist = cdist,
		red = coltab[1,], green = coltab[2,], blue = coltab[3,])
	# sorted from closest to furthest:
	cdist2 <- arrange(cdist2, dist) # 
	# Now some trigonometry
	
	# P1, P2, the two closest paint chips; Ps the sample point
	# A is the distance between P1 & Ps
	# B is the distance between P2 & Ps
	# C is the distance between P1 & P2
	A <- cdist2[1,2]
	B <- cdist2[2,2]
	cdist3 <- cdist2[c(1,2),c(3,4,5)] # just the two closest points
	C <- sqrt((cdist3[1,1] - cdist3[2,1])^2 + (cdist3[1,2] - cdist3[2,2])^2
		+ (cdist3[1,3] - cdist3[2,3])^2)
		
	# a will be cos of the angle Ps P2 P1
	# b will be cos of the angle Ps P1 P2
	a <- (A^2 - B^2 - C^2)/(-2*B*C)
	b <- (B^2 - A^2 - C^2)/(-2*A*C)

	# C1 & C2 will be the two pieces of C where the normal dropped from Ps hits
	C1 <- A*b
	C2 <- B*a

	# Compute some measures of the quality of the fit
	# sin of angle a should be between 0 and 1 with smaller numbers being
	# better fit
	s <- sin(acos(a))
	# Length of D from the P1-P2 line
	# This is based upon a cube with sides 255; diagonal is 1.73*255
	D <- s*B*100/(1.73*255)
	message("Smaller is better (%):")
	print(D)


	res <- getInterRGB(C1/C, calCols[cdist2[1,1]], calCols[cdist2[1,2]])
	res # returns hex code, need to put answer into some kind of quantitative form 
}
