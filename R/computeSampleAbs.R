

computeSampleAbs <- function(calCols, sampCol = NULL) {
	
	# Function to interpolate a sample color on the paint chip scale

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Carry out a principal.curve fit
	# Add pure black and pure white to the points to be fit
	calCols$rgb <- rbind(calCols$rgb, c(0.0, 0.0, 0.0), c(1, 1, 1))	
	fit <- principal.curve(as.matrix(calCols$rgb))
	
	if (!is.null(sampCol)) {
		# Convert to rgb, then fit back onto existing principal curve
		rgb <- col2rgb(sampCol)/255
		tm <- matrix(c(0.0, 0.0, 0.0, rgb, 1.0, 1.0, 1.0), ncol = 3, byrow = TRUE)
		fit2 <- get.lam(tm, fit$s, tag = fit$tag)
		print(str(fit2))
		abs <- round(fit2$lambda[2]/fit2$lambda[3], 2)
		# fs <- fit$s
		# fs <- rbind(fit$s, rgb)	
		# fit2 <- get.lam(x = fs, s = fit$s, tag = fit$tag)
		# abs.i <- which(fit2$tag == nrow(fs)) # the test color is in the last row
		# abs <- round(fit2$lambda[abs.i]/max(fit2$lambda), 2)
		message("The sample absorbance is ", abs)
		message("The sample is ", round(fit2$dist, 2), " from the calibration curve")
		}
		
	invisible(fit)
	}
