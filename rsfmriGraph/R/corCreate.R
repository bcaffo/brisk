#' @title corCreate loops over a collection of csv files and creates correlations
#' 
#' @description Reads a collection of csv files and puts them into a list of either 
#' correlations or covariances
#' 
#' @details This function loops over a collection of csv files and then creates a 
#' matrix of vectorized covariance or correlation functions or a list of them.
#'
#' @author Brian Caffo
#' @param filelist chararcter list of csv files
#' @param loadFunction the function used to load in the files (defaults to read.csv)
#' @param transpose whether or not to transpose the date before creating the correlations
#' @param what function that is used to create matrix, obvious candidates are \code{\link{cor}} \code{{link(cov)}}
#' @param asMatrix each correlation run through cor2vec and stacked up as a matrix? Otheriwse dump everything into a list
#' @param columns use only specific columns of the csv files
#' @param columnNames character vectror of names 
#' @param cores parallelize the computation, only needed if \code{what} is something computationally intensive, otherwise it will slow things down
#' @export
#' @seealso cor2vec
#' @return returns a matrix or list of output from what
#' @keywords cor

corCreate <- function(fileList, path = "./", 
                      loadFunction = read.csv, 
                      transpose = FALSE,
                      what = cor, 
                      asMatrix = TRUE, 
                      columns = NULL, 
                      columnNames = NULL,
                      cores = 1, 
                      ...){

    ##the fileList with the paths appended
    filesFullPath <- paste(path, "/", fileList, sep = "")
    
    ##stop if the files don't all exist
    stopifnot(all(sapply(filesFullPath, file.exists)))
    
    ##read in the data
    dat <- lapply(filesFullPath, function(filename) loadFunction(filename, ...)) 
    
    if (transpose) dat <- lapply(dat, t)
    
    ##check to make sure everything has the same number of columns
    cols <- sapply(dat, ncol)
    if (length(unique(cols)) > 1) {
        print(data.frame(file = filesFullPath, columns = cols))
        stop("Files do not all have the same number of columns")
    }
    
    
    ##if no columns are specified use all
    if (!is.null(columns)) {
        dat <- lapply(dat, function(datEl) datEl[, columns])
    }
    else columns <- 1 : cols[1]

    ##apply the column names if asked
    if (!is.null(columnNames)){
        if (length(columnNames) != length(columns)){
            stop("columnNames of a different length than the number of specified columns")
        }
        dat <- lapply(dat, function(datEl) {colnames(datEl) <- columnNames; datEl})
    }
    
    if (cores > 1) {
        cl <- makeCluster(getOption("cl.cores", cores))
        out <- parLapply(cl, dat, what)
    }
    else if (cores == 1) {
        out <- lapply(dat, what)
    }
    names(out) <- fileList    
    
    if (asMatrix) out <- t(sapply(out, cor2vec))
    
    if (cores > 1) stopCluster(cl)
    
    return(out)
}
