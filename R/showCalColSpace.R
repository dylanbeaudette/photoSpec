

# 10 colors along one radius of D65, the 10 colors used if no colors
# given in plotSampleCard()

tst1 <- c("#FF00FF", "#FF48FF", "#FF69FF", "#FF83FF", "#FF9AFF", "#FFB0FF", "#FFC4FF", "#FFD8FF", "#FFECFF", "#FEFFFF")

tst2 <- c("#a52a2a", "#ff4040", "#8b0000", "#ff1493", "#cd1076", "#B22222", "#ff69b4", "#8b3a62", "#ff00ff", "#ff34b3") # several colors in the red/pink/magenta range

demo <- data.frame(x = runif(1), y = runif(1), z = runif(1))
dc <- paste("#", as.hexmode(floor(demo$x*255)),
	as.hexmode(floor(demo$y*255)), as.hexmode(floor(demo$z*255)), sep = "")

showCalColSpace(calCols = tst1, sampCol = dc)



showCalColSpace <- function(calCols, sampCol = NULL, sampName = "Demo",
	ellipsoid = TRUE) {

	rgb <- col2rgb(calCols)
	rgb <- t(rgb/255)
	rgb <- as.data.frame(rgb)
	names(rgb) <- c("x", "y", "z")
	
	rgbCent <- as.matrix(rgb - colMeans(rgb))
	eig <- eigen(cov(rgbCent))
	eigVal <- eig$values
	# Run PCA to find the line of best fit
	# eigVec <- eig$vectors # these are the loadings
	# scores <- rgbCent %*% eigVec
	# PC1 <- rbind(eigVec[,1], -eigVec[,1]) * 0.5*diff(range(scores))
	# PC1R <- PC1 %*% t(eigVec) + colMeans(rgb)
	# PC2 <- rbind(eigVec[,2], -eigVec[,2]) * 0.5*diff(range(scores))
	# PC2R <- PC2 %*% t(eigVec) + colMeans(rgb)
	
	# Print percent variance explained
	pcs <- data.frame(component = c("PC 1","PC 2", "PC 3"),
		percent = round(cumsum(eigVal)*100/sum(eigVal), 1))
	message("Cumulative variance explained:")
	print(pcs)
	
	# Plot pure white & pure black as reference points (but hide them)
	# also serves to establish the overall scale/space
	
	points3d(x = 0.0, y = 0.0, z = 0.0, col = "transparent",
		size = 8, alpha = 0.0, smooth = FALSE)
	points3d(x = 1.0, y = 1.0, z = 1.0, col = "transparent",
		size = 8, alpha = 0.0, smooth = FALSE)
	axes3d(box = TRUE, expand = 0.9)
	text3d(x = 1.05, y = 1.05, z = 1.05,
		texts = "white", adj = c(0, 0))
	text3d(x = -0.05, y = -0.05, z = -0.05,
		texts = "black", adj = c(1, 1))
	title3d(xlab = "red", ylab = "green", zlab = "blue", main = sampName)
	points3d(rgb, col = calCols, size = 5, point_antialias = TRUE)
	
	if (ellipsoid) {
		ell <- makeEllipsoid(rgb)
		points3d(ell, col = "gray", size = 1)
		}
	
#	segments3d(PC1R, col = "green", size = 10)
#	segments3d(PC2R, col = "green", size = 10)
	
	if (!is.null(sampCol)) {
		xyz <- hex2RGB(sampCol)@coords[1,]
		points3d(xyz[1], xyz[2], xyz[3], size = 10, col = sampCol, point_antialias = TRUE)
		}	
	
	invisible(rgb)
	}