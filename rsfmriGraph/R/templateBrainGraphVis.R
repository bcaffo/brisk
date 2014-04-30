#' @title Template Brain Graph visualization
#' 
#' @author Brian Caffo
#' 
#' @description function to plot a graph on a template brain
#' roiNumbers should be in unique(roiImage) are the integer image values to draw the
#' graph at. roiNumbers should be the same length as pvalues.


templateBrainGraphVis <- function(pvalues, 
                     cutoff,
                     templateImage,
                     roiImage,
                     roiNumbers,
                     roiNames, 
                     lwd = 2, 
                     col = "lightblue"){
    ##convert index to subscripts
    imageDim <- dim(roiImage)

    ##get the centers of the rois
    roiCentroids <- sapply(roiNumbers, function(i){
        imageSubscripts <- ind2sub(imageDim, (1 : prod(imageDim))[roiImage == i])
        apply(imageSubscripts, 2, mean)
    })
    
    contour3d( templateImage, level= 1, smooth = 20, fill = TRUE, mask = array(TRUE, dim(templateImage)), alpha = .2, add = FALSE)

    ##apply the lines 
    idxs <- expand.grid(1 : roiCount, 1 : roiCount)
    idxs <- idxs[idxs[,1] < idxs[,2],]
    nonzero <- (1 : choose(roiCount, 2))[pvalues < cutoff]
    for (i in 1 : length(nonzero)){
        subscripts <- roiCentroids[, unlist(idxs[nonzero[i],])]
        lines3d(subscripts[1,], subscripts[2,], subscripts[3,], lwd = lwd)
    }
    spheres3d(roiCentroids[1, ], roiCentroids[2, ], roiCentroids[3, ], radius = 2, color = col)
}
