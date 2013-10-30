

calcSampleValue <- function(calCols, sampCols = NULL) {
	
	# Function to project sample color(s) onto the principal
	# curve formed by the calCols along with pure black and white.
	# Experimentation shows that this is approximately proportional
	# to the Munsell value

	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	# Carry out a principal.curve fit
	# Add pure black and pure white to the points to be fit
	calCols$rgb <- rbind(calCols$rgb, c(0.0, 0.0, 0.0), c(1, 1, 1))	
	fit <- principal.curve(as.matrix(calCols$rgb))
	
	if (!is.null(sampCols)) {
		# Convert to rgb, then fit back onto existing principal curve
		rgb <- t(col2rgb(sampCols$hex)/255)
		
		for (i in 1:nrow(rgb)) { # fit each sample separately
			tm <- matrix(c(0.0, 0.0, 0.0, as.vector(rgb[i,]), 1.0, 1.0, 1.0), ncol = 3, byrow = TRUE)
			fit2 <- get.lam(tm, fit$s, tag = fit$tag)
			conc <- 1 - fit2$lambda[2]/fit2$lambda[3]
			conc <- round(conc, 2)
			sampCols$value[i] <- conc
			sampCols$residual[i] <- fit2$dist
			# message("Sample ", sampCols$id[i], " is ", round(fit2$dist, 2), " from the calibration curve")
			# message("Sample ", sampCols$id[i], " has absorbance ", conc, "\n")
			}
		}
	
	# assemble everything that might be useful
	L <- list(pcfit = fit)
	if (!is.null(sampCols)) L$sampCols <- sampCols
	invisible(L)
	}
