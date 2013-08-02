

calcColorPurity <- function(sampCol = NULL, gamut = "sRGB", plotPts = TRUE, ...) {

	# Bryan Hanson, DePauw University, July 2013 hanson@depauw.edu
	# Part of the photoSpec package
	
	# This method does not require calibration, but depends 100%
	# on the accuracy of the hexadecimal codes provided.
		
	if (is.null(sampCol)) stop("No sample colors provided")
	ns <- nrow(sampCol) # no. of samples
	
	# sampCol should have columns hex and id - backward compatability
	if ("cols" %in% names(sampCol)) sampCol$hex <- sampCol$cols
	
	# Convert to CIE xy  * this approach ignores brightness*
	rgb <- t(col2rgb(sampCol$hex)/255)
	XYZ <- convertColor(rgb, from = "sRGB", to = "XYZ")
	x <- XYZ[,1]/rowSums(XYZ)
	y <- XYZ[,2]/rowSums(XYZ)
	cie <- cbind(x, y)

	# Load the requested gamut
	
	if (gamut == "sl") { # spectral locus data (shark fin)
		data(CIExyz)
		pg <- CIExyz[,c(2,3)] # 4400 rows
		pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
		}

	if (gamut == "sRGB") { # device color space
		pg <- getGamutValues("sRGB")
		pg <- rbind(pg, pg[1,]) # repeat row so that polygon can close
		}

	# Find where the D65 -> cie segment hits the gamut polygon (pg)
	# Extend the segment first to ensure it is long enough
	cie2 <- cie
	for (i in 1:ns) {
		cie2[i,] <- extendFromD65(cie[i,])
		}

	D65 <- getWhiteValues("D65")

	if (plotPts) { # plot the points if requested
#		plotCIEchrom(opts = "sRGB", ...)
		grid.points(D65[1], D65[2], size = unit(0.5, "char"))
		grid.text("  D65", D65[1], D65[2], hjust = 0, default.units = "native",
			gp = gpar(cex = 0.75))
		
		grid.points(cie[,1], cie[,2], size = unit(0.5, "char"))
		grid.text(label = sampCol$id, cie[,1], cie[,2], default.units = "native",
			hjust = 0, gp = gpar(cex = 0.75))
		}

	hits <- findPolygonIntersection(XY = cie2, xy = pg) # indices of intersections
	if (length(hits) != ns) stop("Wrong number of hits")

	# Compute color purity (ratio of D65 to color / D65 to gamut)

	cp <- sampCol # also need to record which gamut was in use
	cp$purity <- rep(0.0, ns)

	for (i in 1:ns) {
		dc <- dAB(D65, cie[i,]) # distance from D65 to color
		ndx <- hits[i]
		where <- lineIntersection(D65[1], D65[2], cie2[i,1], cie2[i,2],
			pg[ndx, 1], pg[ndx, 2], pg[ndx + 1, 1], pg[ndx + 1, 2])
		dg <- dAB(D65, where) # distance from D65 to gamut
		cp$purity[i] <- dc*100/dg
		}
	
	cp

	} # end of function