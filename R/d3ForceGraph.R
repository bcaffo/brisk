
##working resource locations, remove after creating package
preamble <- paste(scan('~/sandboxes/brisk/data/preamble.html', what = character(), sep = '\n'), "\n")
postamble <- paste(scan('~/sandboxes/brisk/data/postamble.html', what = character(), sep = '\n'), "\n")
d3ForceGraphGuts <- paste(scan('~/sandboxes/brisk/data/d3ForceGraphGuts.html', what = character(), sep = '\n'), '\n')

d3ForceGraph <- function(graph, 
                         roiNames, 
                         cutoff = NULL, 
                         filename = 'd3ForceGraph.html', 
                         w = 500, 
                         h = 500,
                         barPadding = 1){
    if (is.matrix(graph)) graph <- graph[upper.tri(graph)]  
    if (!is.null(cutoff)) graph <- (graph < cutoff) * 1
   
    roiCount <- length(roiNames)
    idxs <- expand.grid(1 : roiCount, 1 : roiCount)
    idxs <- idxs[idxs[,1] < idxs[,2],]
    nonzero <- (1 : choose(roiCount, 2))[graph == 1]
    
    nodes <- sapply(roiNames, function(r) paste('{ name: "', r, '" }', sep = ""))
    
    ##remember that JS starts counting at 0
    edges <- apply(idxs[nonzero,], 1, 
        function(p){
            paste("{ source: ", p[1] - 1 , ", target: ", p[2] - 1, " }", sep = "")    
        }
    )

    ##add a newline and common after everything except the last one
    temp <- c(rep(", \n", length(nodes) - 1), "\n")
    nodes <- paste(nodes, temp, sep = "")
    edges <- paste(edges, temp, sep = "")
    rm(temp)

    
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
        postamble,
        file = filename)
 }



