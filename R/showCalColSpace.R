
showCalColSpace <- function(calCols, sampCol, sampName = "Demo",
	calVals = c(1, 10), ellipsoid = TRUE, space = "rgb") {
		
	# Function to make a 3D plot of a color sample,
	# along with a set of calibration colors.
	
	# Bryan Hanson, DePauw University, March 2013 hanson@depauw.edu
	# Part of the photoSpec package

	require("rgl")
	
	rgb <- col2rgb(calCols)
	rgb <- t(rgb/255)
	rgb <- as.matrix(rgb)
	
	# Run PCA to find the line of best fit
	# This is unscaled PCA
	pca <- prcomp(rgb)
	
	# Calc PC1-PC3 (used several places, needed now)
	PC1 <- rbind(pca$rotation[,1], - pca$rotation[,1]) * 0.5*diff(range(pca$x))
	PC2 <- rbind(pca$rotation[,2], - pca$rotation[,2]) * 0.5*diff(range(pca$x))
	PC3 <- rbind(pca$rotation[,3], - pca$rotation[,3]) * 0.5*diff(range(pca$x))

	open3d()
	
	if (space == "rgb") { # plot in original rgb space
		
		# Plot pure white & pure black to anchor the space (but hide them);
		# serves to establish the overall scale/space
		
		axes3d(box = TRUE, expand = 0.9)
		points3d(x = 0.0, y = 0.0, z = 0.0, col = "transparent",
			size = 8, smooth = FALSE)
		points3d(x = 1.0, y = 1.0, z = 1.0, col = "transparent",
			size = 8, smooth = FALSE)
		text3d(x = 1.05, y = 1.05, z = 1.05,
			texts = "white", adj = c(0, 0))
		text3d(x = -0.05, y = -0.05, z = -0.05,
			texts = "black", adj = c(1, 1))
		title3d(xlab = "red", ylab = "green", zlab = "blue", main = sampName)
	
		# Now add in the sample data
		points3d(rgb, col = calCols, size = 5, point_antialias = TRUE)
		
		if (ellipsoid) {
			ell <- makeEllipsoid(rgb)
			points3d(ell, col = "gray", size = 1)
			}

		xyz <- col2rgb(sampCol)
		xyz <- as.matrix(t(xyz/255))
		points3d(xyz, size = 10, col = sampCol,
			point_antialias = TRUE)

		} # end of space == rgb
	
	if (space == "PCA") { # plot everything in PC space
		# This is unscaled PCA
		points3d(pca$x, size = 5, col = calCols)
		segments3d(PC1, size = 10) # these look OK
		segments3d(PC2, size = 10)
		segments3d(PC3, size = 10)
		text3d(PC1, texts = "1", adj = c(1,1))
		text3d(PC2, texts = "2", adj = c(1,1))
		text3d(PC3, texts = "3", adj = c(1,1))
#		points3d(0.0, 0.0, 0.0, size = 10, col = "black")
		# PCs should pass thru centroid
		
		# Project sample point into PC space, then plot
		Srgb <- col2rgb(sampCol)
		Srgb <- t(Srgb/255)
		Srgb <- as.matrix(Srgb)
		names(Srgb) <- c("x", "y", "z")
		nPt <- (Srgb - colMeans(rgb)) %*% pca$rotation		
		points3d(nPt, col = sampCol, size = 10, point_antialias = TRUE)
		if (ellipsoid) {
			ell <- makeEllipsoid(pca$x)
			points3d(ell, col = "gray", size = 1)
			}
		} # end of space == PCA

	# Report on "goodness of fit" metrics
	message("\nGoodness of fit metrics\n")
	
	# Report percent variance explained
	pcs <- data.frame(component = c("PC 1","PC 2", "PC 3"),
		percent = round(cumsum(pca$sdev)*100/sum(pca$sdev), 1))
	message("Cumulative variance:")
	print(pcs) # by definition should add to 100%
	message("Note: the more variance captured by PC1 the better\n")
		
	# Report on fit using projection onto 1st PC
	Srgb <- col2rgb(sampCol)
	Srgb <- t(Srgb/255)
	Srgb <- as.matrix(Srgb)
	names(Srgb) <- c("x", "y", "z")
	sc <- abs(as.numeric((Srgb - colMeans(rgb)) %*% pca$rotation[,1]))		
	message("Better is smaller for the next two metrics")
	msg <- paste(">>> Distance from 1st PC to sampCol:", round(sc, 2), "(RGB units)")
	message(msg)

	# Report a combined metric (the smaller the better)
	nm <- round(100*sc/pcs[1,2], 2)		
	msg <- paste(">>> 100*Sample Distance/Var Explained by 1st PC:", round(nm, 2))
	message(msg)

	res <- computeSampleAbs(calCols, sampCol, PC1, calVals)
	msg <- paste("\nThe sample measured", round(res, 2), "on a scale of", calVals[1], "to", calVals[2], sep = " ")
	message(msg)
	invisible(rgb)
	}