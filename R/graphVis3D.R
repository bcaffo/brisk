##function creates a graph based on roinames for graph < cutoff
##if the cutoff isn't specified it simply assumes a binary graph
##assumes that the values are put is as matrix[upper.tri[matrix]] of 
##a data matrix where the column and row orders correspond to roiNames
##bend is how much the lines bend toward the origin (between 0 and 1)
##if it's input as a matrix, then it does this automatically
graphVis3D <- function(graph, roiNames, cutoff = NULL, expansion = 1.2, bend = .5, lwd = 2, col = "black"){
    if (is.matrix(graph)) graph <- graph[upper.tri(graph)]  
    if (!is.null(cutoff)) graph <- (graph < cutoff) * 1
     
    roiCount <- length(roiNames)
    idxs <- expand.grid(1 : roiCount, 1 : roiCount)
    idxs <- idxs[idxs[,1] < idxs[,2],]
    nonzero <- (1 : choose(roiCount, 2))[graph == 1]
    
    ##create the graph of labels
    ##angle values in radians
    angles <- seq(0, 2 * pi, length = roiCount + 1)[-(roiCount + 1)]
    x <- cos(angles)
    y <- sin(angles)
    
    open3d()
    
    for (i in 1 : length(x)){
        text3d(x[i] * expansion, y[i] * expansion, 0, text = roiNames[i], cex = .75, srt = angles[i] * 180 / pi, add = TRUE)
    }
    
    ##connect points with lines
    for (i in 1 : length(nonzero)){
        p <- unlist(idxs[nonzero[i],])
        p0 <- c(x[p[1]], y[p[1]])
        p1 <- c(x[p[2]], y[p[2]])
        p2 <- bend * c(0, 0) + (1 - bend) * (.5 * p0 + .5 * p1)
        
        xvals <- c(p0[1], p1[1], p2[1])
        yvals <- c(p0[2], p1[2], p2[2])
        
        tvals <- c(0, 1, .5)
        t <- seq(0, 1, length = 100)
        lines3d(predict(lm(xvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)), 
                predict(lm(yvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)),
                z = 0,
                lwd = lwd)               
    }
    spheres3d(x = x, y = y, z = 0, radius = .025, color = "blue", add = TRUE)
    
}
