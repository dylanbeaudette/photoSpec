
showCalColSpace <- function(calCols, sampCol = NULL, sampName = "Demo") {
		
	# Function to make a 3D plot of a color sample,
	# along with a set of calibration colors.
	
	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	open3d(windowRect = c(0, 0, 500, 500))
	bg3d("black")
	
	axes3d(box = TRUE, expand = 0.9, col = "white", lwd = 3)
	points3d(x = 0.0, y = 0.0, z = 0.0, col = "white",
		size = 8, smooth = FALSE)
	points3d(x = 1.0, y = 1.0, z = 1.0, col = "white",
		size = 8, smooth = FALSE)
	title3d(xlab = "red", ylab = "green", zlab = "blue", col = "white")
	points3d(calCols$rgb, col = calCols$hexcol, size = 5, point_antialias = TRUE)

	# Compute and show the principal curve
	fitpts <- computeSampleAbs(calCols, sampCol)
	lines3d(fitpts$s[fitpts$tag,], col = "white", lwd = 3)
	invisible(fitpts)
	}