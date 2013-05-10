

generateCalCols <- function(wedge, nDiv = 10, pcpd = 1, divMode = "linear") {
	
	# Bryan Hanson, DePauw University, May 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Function to process colors selected by the user and
	# reduce them in a sensible way to a limited no. of values for calibration
	
	# wedge is the data coming from selectCIExy
	# nDiv is the no. of divisions to create
	# pcpd is the no. of paint chips per division
	# total paint chips = nDiv * pcpd
	# divMode = the scheme used to divide up the wedge into parts
	
	# consider adding a plot showing the results
	
	# Divide the wedge up: 1. get length of radius from D65 -> L1-L2
	data(CIExyz)
	D65 <- getWhiteValues("D65")
	
	
	}