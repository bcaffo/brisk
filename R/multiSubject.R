multiSubjectGraphVis <- function(graphList, roiNames, expansion = 1.2, bend = .5, lwd = 2, col = "black"){
    roiCount <- length(roiNames)
    
    ##this is just a list of rows and column indices in a particular order
    idxs <- expand.grid(1 : roiCount, 1 : roiCount)
    idxs <- idxs[idxs[,1] < idxs[,2],]

    ##create the graph of labels
    ##angle values in radians
    angles <- seq(0, 2 * pi, length = roiCount + 1)[-1]
    x <- cos(angles)
    y <- sin(angles)
    noGraphs <- length(graphList)

    z <- seq(-noGraphs/2, noGraphs/2, length = noGraphs)
    
    open3d()
    for (i in 1 : noGraphs){
        graph <- graphList[[i]]
        graph <- graph[upper.tri(graph)]        
        for (j in 1 : length(x)){
            text3d(x[j] * expansion, y[j], z[i], text = roiNames[j], cex = .75, srt = angles[i] * 180 / pi, add = TRUE)
        }
        
        ##find the elements of idxs that have corresponding non zero graph entries
        nonzero <- (1 : choose(roiCount, 2))[graph == 1]
        ##connect points with lines
        for (j in 1 : length(nonzero)){
            p <- unlist(idxs[nonzero[j],])            
            lines3d(x[p], y[p], z[i])
        }
#        points3d(x = x, y = y, z = z[i], , color = "blue", add = TRUE)
        spheres3d(x = x, y = y, z = z[i], radius = .025, color = "blue", add = TRUE)
        
    }
}








