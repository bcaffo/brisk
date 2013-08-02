##a function to display a graph on a set of 3D spheres
graphVis3DSphere <- function(graph, 
                             roiNames, 
                             cutoff = NULL,
                             expansion = 1.2, 
                             bend = .5, 
                             lwd = 2, 
                             col = "black"){
    ##two data formats are accepted, either a matrix or the upper triangle of a matrix
    if (is.matrix(graph)) graph <- graph[upper.tri(graph)]  
    if (!is.null(cutoff)) graph <- (graph < cutoff) * 1

    ##apply the cutoff if it hasn't be done already since this package
    ##only displays binary graphs
    if (!is.null(cutoff)) graph <- (graph == 1) * 1
    
    ##the number of rois
    roiCount <- length(roiNames)
    idxs <- expand.grid(1 : roiCount, 1 : roiCount)
    idxs <- idxs[idxs[,1] < idxs[,2],]
    nonzero <- (1 : choose(roiCount, 2))[graph == 1]
    
    ##from here http://blog.marmakoide.org/?p=1
    theta <- (1 : roiCount) * pi * (3 - sqrt(5))
    z = seq(from = 1 - 1.0 / roiCount, to = 1.0 / roiCount - 1, length = roiCount)
    radius <- sqrt(1 - z^2)
    x <- radius * cos(theta)
    y <- radius * sin(theta)

    ##initialize the rgl window
    open3d()
    
    ##plot the labels
    for (i in 1 : length(x)) {
        text3d(x[i] * expansion, y[i] * expansion, z[i], text = roiNames[i], cex = .75, add = TRUE)
    }

    ##connect points with lines
#     for (i in 1 : length(nonzero)){
#         p <- unlist(idxs[nonzero[i],])
#         lines3d(x[p], y[p], z[p],lwd = lwd)               
#     }
#     spheres3d(x, y, z, radius = .05, color = "blue", add = TRUE)

    ##connect points with lines
    for (i in 1 : length(nonzero)){
        p <- unlist(idxs[nonzero[i],])
        p0 <- c(x[p[1]], y[p[1]], z[p[[1]]])
        p1 <- c(x[p[2]], y[p[2]], z[p[[2]]])
        p2 <- bend * c(0, 0, 0) + (1 - bend) * (.5 * p0 + .5 * p1)
        xvals <- c(p0[1], p1[1], p2[1])
        yvals <- c(p0[2], p1[2], p2[2])
        zvals <- c(p0[3], p1[3], p2[3])
        tvals <- c(0, 1, .5)
        t <- seq(0, 1, length = 100)
        lines3d(predict(lm(xvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)), 
                predict(lm(yvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)),
                predict(lm(zvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)),
                lwd = lwd)               
    }
    spheres3d(x = x, y = y, z = z, radius = .025, color = "blue", add = TRUE)
}
