##performs permutation tests for paired and groupd data in the form of a matrix
##groups must be aligned by columns. Multiplicity accounted for by taking the max
##larger values of the statistic points to the alternative
groupMatrixPermutationTest <- function(group1MatrixData, group2MatrixData, 
                            paired = FALSE, 
                            cores = 1, 
                            permutations = 1000, 
                            stat = function(x, y) abs(mean(x) - mean(y))
                            ){
    ##some quick dimension checks
    stopifnot(ncol(group1MatrixData) == ncol(group2MatrixData))

    ##if paired, they have to have the same number of subjects in the same order
    if (paired) stopifnot(nrow(group1MatrixData) == nrow(group2MatrixData))

    v <- ncol(group1MatrixData)
    n <- nrow(group1MatrixData)
    m <- nrow(group2MatrixData)

    ##the observed statistics value
    observedStat <- sapply(1 : ncol(group1MatrixData),
            function(i) {
                x <- group1MatrixData[,i]
                y <- group2MatrixData[,i]
                stat(x, y)
            }
    )
    
    ##the two strategies for generating permutations (paired and unpaired case). Note all of the
    ##permutations are generated at first
    if (paired){
        ##each column is an indicator of whether that pair should switch groups
        permIDXs <- matrix(sample(c(1, 0), size = n * permutations, replace = TRUE), n, permutations)
    }
    else {
        ##each column is a reshuffling of the subject IDs
        permIDXs <- sapply(1 : permutations, function(x) sample(1 : (m + n)))
    }
        
    ##a parallel function
    myApply <- function(X, FUN){
        if (cores > 1){
            parApply(cl, X, 2, FUN)
        }
        else{
            apply(X, 2, FUN)
        }
    }

    if (cores == 1) cl <- NULL
    else cl <-makeCluster(getOption("cl.cores", cores))

    permutationDistribution <- myApply(permIDXs, 
        function(perm){
            ##for each permutation take the maximum statistic value
            max(
                ##loop over all v
                sapply(1 : v, 
                    function(i) {
                        x <- group1MatrixData[,i]
                        y <- group2MatrixData[,i]
                        if (paired){
                            x <- x * perm + y * (1 - perm)
                            y <- x * (1 - perm) + y * perm
                        }
                        else {
                            temp <- c(x, y)
                            x <- temp[perm[1 : n]]
                            y <- temp[perm[(n + 1) : (n + m)]]
                        }
                        return(stat(x, y))
                    }
                )
            )
        }
    )
    if (cores > 1) stopCluster(cl)

    out <- list(pvalues = sapply(observedStat, function(x) mean(permutationDistribution > x)),
                observedStat = observedStat,
                permutationDistribution = permutationDistribution)
    
    return(out)
}