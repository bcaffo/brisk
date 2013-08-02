##function creates a graph based on roinames for graph < cutoff
##assumes that the values are put is as matrix[upper.tri[matrix]] of 
##a data matrix where the column and row orders correspond to roiNames
##bend is how much the lines bend toward the origin (between 0 and 1)
##if this isn't the case, 
graphVis <- function(graph, 
                     roiNames, 
                     cutoff = NULL, 
                     expansion = 1.2, 
                     bend = .5, 
                     lwd = 2, 
                     col = "black"){
    if (is.matrix(graph)) graph <- graph[upper.tri(graph)]  
    if (!is.null(cutoff)) graph <- (graph < cutoff) * 1
     
    roiCount <- length(roiNames)
    idxs <- expand.grid(1 : roiCount, 1 : roiCount)
    idxs <- idxs[idxs[,1] < idxs[,2],]
    nonzero <- (1 : choose(roiCount, 2))[graph == 1]
    
    ##create the graph of labels
    ##angle values in radians
    angles <- seq(0, 2 * pi, length = roiCount + 1)[-1]
    x <- cos(angles)
    y <- sin(angles)
    
    plot(c(-1, 1) * expansion, c(-1, 1) * expansion, type = "n", axes = FALSE, xlab = "", ylab = "")
    for (i in 1 : length(x)){
        text(x[i] * expansion, y[i] * expansion, labels = roiNames[i], cex = .75, srt = angles[i] * 180 / pi)
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
        t <- seq(0, 1, length = 1000)
        lines(predict(lm(xvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)), 
              predict(lm(yvals ~ tvals + I(tvals ^ 2)), newdata = data.frame(tvals = t)),
              col = col,
              lwd = lwd)
    }
    points(x, y, pch = 21, col = "black", bg = "lightblue")
    
}
