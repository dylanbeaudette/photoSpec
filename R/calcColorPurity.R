

calcColorPurity <- function(sampCols = NULL, gamut = "sRGB", lambdas = NULL,
	plotPts = TRUE, plotLambdas = FALSE, ...) {

	# Bryan Hanson, DePauw University, July 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# This method does not require calibration, and the results depend
	# 100% on the accuracy of the hexadecimal codes provided.

	if (is.null(sampCols)) stop("No sample colors provided")
	ns <- nrow(sampCols) # no. of samples
		
	# Convert to CIE xy  * this approach ignores brightness*
	rgb <- t(col2rgb(sampCols$hex)/255)
	XYZ <- convertColor(rgb, from = "sRGB", to = "XYZ")
	x <- XYZ[,1]/rowSums(XYZ)
	y <- XYZ[,2]/rowSums(XYZ)
	cie <- cbind(x, y)

	D65 <- getWhiteValues("D65")
	dx <- as.numeric(D65[1])
	dy <- as.numeric(D65[2])
	data(CIExyz) # these are needed several places

	if (plotPts) { # plot the points if requested (plot is already open/active)
		grid.points(dx, dy, size = unit(0.5, "char"))
		grid.text("  D65", dx, dy, hjust = 0, default.units = "native",
			gp = gpar(cex = 0.75))
		
		grid.points(cie[,1], cie[,2], size = unit(0.5, "char"))
		# May want an option to not label the points here
		grid.text(label = sampCols$id, cie[,1], cie[,2], default.units = "native",
			hjust = 0, gp = gpar(cex = 0.75))
		}

	# Compute color purity (ratio of D65 to color / D65 to gamut) @ apparent lambda max

	cp <- sampCols # also should record which gamut was in use
	cp$purity <- ccp(cie, gamut) # color purity by definition; always (+)
	
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
		# onto lines passing through a given wavelength on the spectral locus & D65
		# and compute the corresponding color purity
		
		# Initialize data structures
		nl <- length(lambdas)

		projPts <- matrix(NA, nrow = ns*nl, ncol = 2) # will hold the projections
		colnames(projPts) <- c("x", "y")
		projPts <- as.data.frame(projPts)

		projVals <- matrix(NA, nrow = ns, ncol = nl) # will hold the purity values
		colnames(projVals) <- paste("Proj", lambdas, sep = "")
		projVals <- as.data.frame(projVals) # ns rows x nl columns
				
		ind <- findCIEindex2(lambdas)
		if (length(ind) != nl) stop("Did not find every wavelength requested")
		
		s1 <- CIExyz[ind,3] - rep(dy, length(ind))
		s2 <- CIExyz[ind,2] - rep(dx, length(ind))
		slope <- s1/s2 # nl values of slope
		intercept <- dy - slope*dx # nl values of intercept
		
		# Fill in the data frames
		ind2 <- expand.grid(1:ns, 1:nl) # index combos needed next
		sign <- rep(1.0, ns*nl)
		for (n in 1:(ns*nl)) {
			projPts[n,] <- pointOnLineNearPoint(as.vector(cie[ind2[n,1],1]), as.vector(cie[ind2[n,1],2]),
				slope[ind2[n,2]], intercept[ind2[n,2]])
			# Projection onto an arbitrary line may produce (-) color purities.
			# Need to check for this possibility.  We will use just the x coord
			# (+)-ive values of color purity will be opposite Lmax
			sx <- projPts[n,1] # x coord of projection
			lx <- CIExyz[ind[ind2[n,2]],2]# x coord of lambda (note use of ind from above)
			if ((sx > dx) & (lx > dx)) sign[n] <- -1.0
			if ((sx < dx) & (lx < dx)) sign[n] <- -1.0
			}
			
		cpPts <- sign*ccp(projPts, gamut) # results in a vector ns * nl long, grouped first by ns

		for (n in 1:nl) { # unstack the data
			projVals[,n] <- cpPts[((ns*n)-ns+1):(ns*n)] # yikes!
			}
		
		cp <- cbind(cp, projVals) # add to original sampCols for return value
		
		if (plotLambdas) {
			# Draw a reference line from each lambda through D65 to the spectral locus on the other side
			# First, get the CIE index of the end points
			
			idx1 <- ind # index of lambdas from before
			Lpts <- extendAndRotateAroundD65(CIExyz[idx1,c(2,3)], ang = pi)
			idx2 <- findPolygonIntersection(XY = Lpts, xy = pg) # indices of intersections
			
			# Find the segment end points as x, y
			# Must manually fix when idx2 = 4400, the line of purples
			if (all(idx2 != 4400)) {
				x0 = CIExyz[idx1,2]
				y0 = CIExyz[idx1,3]
				x1 = CIExyz[idx2,2]
				y1 = CIExyz[idx2,3]
				}

			if (any(idx2 == 4400)) {
				#cat("idx1 = ", idx1, "\n")
				#cat("idx2 = ", idx2, "\n")
				prb <- which(idx2 == 4400) # Remove that entry
				idx1a  <- idx1[-prb]
				idx2a  <- idx2[-prb]
				x0 = CIExyz[idx1a,2]
				y0 = CIExyz[idx1a,3]
				x1 = CIExyz[idx2a,2]
				y1 = CIExyz[idx2a,3] # These are the  non-4400 coords, ready to use
				
				# Manually find the intersection between the line of purples and
				# the lambda of interest.  Original idx1[prb] is correct & can be used
				# idx2 is not-usable
				
				# cat("idx1[prb] = ", idx1[prb], "\n")
				# cat("CIExyz[idx1[prb],2] = ", CIExyz[idx1[prb],2], "\n")
				# cat("CIExyz[idx1[prb],3] = ", CIExyz[idx1[prb],3], "\n")
				loc <- lineIntersection(CIExyz[1,2], CIExyz[1,3], CIExyz[4400,2], CIExyz[4400,3],
					D65[1], D65[2],
					CIExyz[idx1[prb],2], CIExyz[idx1[prb],3])
				# lp <- length(prb)
# #				print(CIExyz[1,2])
				# # loc <- lineIntersection(x1 = rep(CIExyz[1,2], lp), y1 = rep(CIExyz[1,3], lp),
					# # x2 = rep(CIExyz[4400,2],lp), y2 = rep(CIExyz[4400,3], lp),
					# # x3 = rep(D65[1], lp), y3 = rep(D65[2], lp),
					# # x4 = CIExyz[idx1[prb],2], y4 = CIExyz[idx1[prb],3])
				# print(loc)
				x0 <- c(x0, CIExyz[idx1[prb],2])
				y0 <- c(y0, CIExyz[idx1[prb],3])
				x1 <- c(x1, loc[,1])
				y1 <- c(y1, loc[,2])
				}
			
			# Now we have everything ready to plot	
			grid.segments(x0, y0, x1, y1, gp = gpar(col = "red"), default.units = "native")

			# Plot the projected points				
			# Note: this code draws the projection even if it out of gamut.
			# If out of gamut, cp returns NA which COULD be checked here and the drawing skipped
			grid.points(projPts[,1], projPts[,2], size = unit(0.5, "char"),
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