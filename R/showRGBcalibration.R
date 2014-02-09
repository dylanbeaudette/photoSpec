
showRGBcalibration <- function(calCols = NULL , sampCols = NULL, title = NULL, ...) {
		
	# Function to make a 3D plot of a color sample,
	# along with a set of calibration colors.
	
	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	if (is.null(calCols)) stop("calCols must be provided")
	
	open3d(windowRect = c(0, 0, 500, 500))
	bg3d("black")
	
	axes3d(box = TRUE, expand = 0.9, col = "white", lwd = 3)
	points3d(x = 0.0, y = 0.0, z = 0.0, col = "white",
		size = 5, smooth = FALSE)
	points3d(x = 1.0, y = 1.0, z = 1.0, col = "white",
		size = 5, smooth = FALSE)
	title3d(main = title, xlab = "red", ylab = "green", zlab = "blue", col = "white")
	points3d(calCols$rgb, col = calCols$hexcol, size = 3, point_antialias = TRUE)

	# Compute and show the principal curve/pts, calibrate, if sampCols given
	
	if (!is.null(sampCols)) {
		# decorate the plot
		rgb <- t(col2rgb(sampCols$hex)/255)
		points3d(rgb[,1], rgb[,2], rgb[,3], col = sampCols$hex, size = 10)
		
		# calibrate and show principal curve
		pts <- calcSampleValue(calCols, sampCols)
		lines3d(pts$pcfit$s[pts$pcfit$tag,], col = "white", lwd = 3)
		return(pts$sampCols)
		}
		
	invisible()
	}