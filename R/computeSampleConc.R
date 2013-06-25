

computeSampleConc <- function(calCols, sampCol = NULL) {
	
	# Function to interpolate sample color(s) on the paint chip scale

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Carry out a principal.curve fit
	# Add pure black and pure white to the points to be fit
	calCols$rgb <- rbind(calCols$rgb, c(0.0, 0.0, 0.0), c(1, 1, 1))	
	fit <- principal.curve(as.matrix(calCols$rgb))
	
	if (!is.null(sampCol)) {
		# Convert to rgb, then fit back onto existing principal curve
		sampCol$conc <- NA
		sampCol$dist <- NA
		rgb <- t(col2rgb(sampCol$cols)/255)
		
		for (i in 1:nrow(rgb)) { # fit each sample separately
			print(rgb[i,])
			tm <- matrix(c(0.0, 0.0, 0.0, as.vector(rgb[i,]), 1.0, 1.0, 1.0), ncol = 3, byrow = TRUE)
			fit2 <- get.lam(tm, fit$s, tag = fit$tag)
			conc <- 1 - fit2$lambda[2]/fit2$lambda[3]
			conc <- round(conc, 2)
			sampCol$conc[i] <- conc
			sampCol$dist[i] <- fit2$dist
			message("Sample ", sampCol$id[i], " is ", round(fit2$dist, 2), " from the calibration curve")
			message("Sample ", sampCol$id[i], " has absorbance ", conc, "\n")
			}
		}
	
	if (!is.null(sampCol)) return(sampCol)
	invisible(fit)
	}
