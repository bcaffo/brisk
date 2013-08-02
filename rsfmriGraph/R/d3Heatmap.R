d3Heatmap <- function(mat,
                      roiNames, 
                         filename = 'd3ForceGraph.html', 
                         w = 500, 
                         h = 500,
                         barPadding = 1){
     
    if (!is.null(cutoff)) graph <- (graph < cutoff) * 1
    
    cat(preamble, 
        'var w = ', w, '\n',
        'var h = ', h, '\n',
        'var barPadding = ', barPadding, '\n',
        'var dataset = { \n nodes: [ \n',
        nodes,
        '],\n edges: [',
        edges,
        ']\n}; \n',
        d3ForceGraphGuts,
        postamble
    
}