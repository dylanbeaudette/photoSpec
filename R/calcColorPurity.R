

calcColorPurity <- function(sampCol = NULL, gamut = "sRGB", lambdas = NULL,
	plotPts = TRUE, plotLambdas = FALSE, ...) {

	# Bryan Hanson, DePauw University, July 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# This method does not require calibration, and depends 100%
	# on the accuracy of the hexadecimal codes provided.

	if (is.null(sampCol)) stop("No sample colors provided")
	ns <- nrow(sampCol) # no. of samples
	
	# sampCol should have columns hex and id - backward compatability
	# if ("cols" %in% names(sampCol)) sampCol$hex <- sampCol$cols
	
	# Convert to CIE xy  * this approach ignores brightness*
	rgb <- t(col2rgb(sampCol$hex)/255)
	XYZ <- convertColor(rgb, from = "sRGB", to = "XYZ")
	x <- XYZ[,1]/rowSums(XYZ)
	y <- XYZ[,2]/rowSums(XYZ)
	cie <- cbind(x, y)

	D65 <- getWhiteValues("D65")
	data(CIExyz) # these are needed several places

	if (plotPts) { # plot the points if requested (plot is already open/active)
		grid.points(D65[1], D65[2], size = unit(0.5, "char"))
		grid.text("  D65", D65[1], D65[2], hjust = 0, default.units = "native",
			gp = gpar(cex = 0.75))
		
		grid.points(cie[,1], cie[,2], size = unit(0.5, "char"))
		grid.text(label = sampCol$id, cie[,1], cie[,2], default.units = "native",
			hjust = 0, gp = gpar(cex = 0.75))
		}

	# Compute color purity (ratio of D65 to color / D65 to gamut) @ apparent lambda max

	cp <- sampCol # also need to record which gamut was in use
	cp$purity <- ccp(cie, gamut)
	
	# Compute apparent lambda max for each sample (appLmax)
	# Find where the D65 -> cie segment hits the spectral locus
	# on the *far* side (so rotate 180 deg)
	
	cie2 <- extendAndRotateAroundD65(cie, ang = pi)
	pg <- CIExyz[,c(2,3)] # 4400 rows
	pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
	hits2 <- findPolygonIntersection(XY = cie2, xy = pg) # indices of intersections
	if (length(hits2) != ns) stop("Wrong number of hits (spectral locus)")
	cp$appLmax <- round(CIExyz[hits2,1]) # If appLmax == 830, that's the line of purples, set to NA
	for (i in 1:ns) {if (cp$appLmax[i] == 830) cp$appLmax[i] <- NA}
		
	if (!is.null(lambdas)) {
		# If lambdas are provided, compute the projections of each sample color
		# onto lines from a given wavelength on the spectral locus through D65 & across
		nl <- length(lambdas)
		projVals <- matrix(NA, nrow = ns, ncol = length(lambdas))
		colnames(projVals) <- paste("Proj", lambdas, sep = "")
		projVals <- as.data.frame(projVals)
		projPts <- data.frame(x = NA, y = NA)
		
		for (i in 1:ns) { # loop over samples
			int <- c()
			slope <- c()
			intercept <- c()
			for (n in 1:nl) {
				int <- c(int, findCIEindex(lambdas[n]))
				slope <- c(slope, as.numeric((CIExyz[int[n],3] - D65[2])/(CIExyz[int[n],2] - D65[1])))
				intercept <- c(intercept, as.numeric(D65[2] - slope[n]*D65[1]))
				}
			if (length(int) != length(lambdas)) stop("Did not find every wavelength requested")
	
			lpt <- pointOnLineNearPoint(as.vector(cie[i,1]), as.vector(cie[i,2]), slope, intercept)
			cplpt <- ccp(lpt, gamut) # compute vector of color purity at each supplied lambda
			projVals[i,] <- cplpt
			projPts <- rbind(projPts, lpt)
			# for (n in 1:nl) { # save the projected points for plotting later
				
				# }
				
			} # end of looping over samples
			
		projPts <- projPts[-1,] # remove NAs
		cp <- cbind(cp, projVals) # return value
		
		if (plotLambdas) {
			# Draw a reference line from each lambda through D65 to the spectral locus on the other side
			# First, get the CIE index of the end points
			idx1 <- c()
			idx2 <- c()
			for (i in 1:length(lambdas)) { # loop over lambdas
				i1 <- findCIEindex(lambdas[i])
				idx1 <- c(idx1, i1)
				Lpts <- extendAndRotateAroundD65(CIExyz[i1,c(2,3)], ang = pi)
				i2 <- findPolygonIntersection(XY = Lpts, xy = pg) # indices of intersections
				idx2 <- c(idx2, i2)
				}

			# Find the segment end points as x, y
			# Must manually fix when idx2 = 4400, the line of purples
			if (all(idx2 != 4400)) {
				x0 = CIExyz[idx1,2]
				y0 = CIExyz[idx1,3]
				x1 = CIExyz[idx2,2]
				y1 = CIExyz[idx2,3]
				}

			if (any(idx2 == 4400)) {
				# cat("idx1 = ", idx1, "\n")
				# cat("idx2 = ", idx2, "\n")
				prb <- which(idx2 == 4400)
				# Remove that entry
				# idx1a  <- idx1[-c(prb, prb-1)]
				# idx2a  <- idx2[-c(prb, prb-1)]
				idx1a  <- idx1[-prb]
				idx2a  <- idx2[-prb]
				x0 = CIExyz[idx1a,2]
				y0 = CIExyz[idx1a,3]
				x1 = CIExyz[idx2a,2]
				y1 = CIExyz[idx2a,3]
				# cat("x0 = ", x0, "\n")
				# cat("y0 = ", y0, "\n")
				# cat("x1 = ", x1, "\n")
				# cat("y1 = ", y1, "\n")
				# Manually find the intersection
				for (i in 1:length(prb)) {
					pp <- findCIEindex(lambdas[prb[i]])
					loc <- lineIntersection(CIExyz[1,2], CIExyz[1,3], CIExyz[4400,2], CIExyz[4400,3],
						D65[1], D65[2],
						CIExyz[pp,2], CIExyz[pp,3])
					x0 <- c(x0, CIExyz[pp,2])
					y0 <- c(y0, CIExyz[pp,3])
					x1 <- c(x1, loc[1])
					y1 <- c(y1, loc[2])
					}
				}
			
			# Now we have everything ready to plot	
			grid.segments(x0, y0, x1, y1, gp = gpar(col = "red"), default.units = "native")

			# Plot the projected points with an X				
			# Note: this code draws the projection even if it out of gamut.
			# If out of gamut, cp returns NA which could be checked here and the drawing skipped
			grid.points(projPts[,1], projPts[,2], size = unit(0.5, "char"), pch = 4,
				gp = gpar(col = "red"), default.units = "native")

			# Label 'em
			lab.pos <- extendAndRotateAroundD65(pts = CIExyz[idx1,c(2,3)], ang = 0, fac = 1.2)
			for (i in 1:nl) {
				grid.text(label = bquote(lambda[.(lambdas[i])]), lab.pos[i,1], lab.pos[i,2],
					default.units = "native", gp = gpar(col = "red"))
				}
			} # end of plotLambdas
			
		} # end of !is.null(lambdas)
	cp

	} # end of function