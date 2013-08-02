##reads a collection of csv files and puts them into a list of either correlations or covariances
corCreate <- function(fileList, path = "./", loadFunction = read.csv, what = "cor", asMatrix = TRUE, columns = NULL, columnNames = NULL, ...){
    ##the fileList with the paths appended
    filesFullPath <- paste(path, "/", fileList, sep = "")
    
    ##stop if the files don't all exist
    stopifnot(all(sapply(filesFullPath, file.exists)))
    
    ##read in the data
    dat <- lapply(filesFullPath, function(filename) loadFunction(filename, ...)) 
    
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

    if (what == "cor") out <- lapply(dat, cor)
    else if (what == "cov") out <- lapply(dat, cov)
    else stop(paste("what = ", what, " is not suppported"))
    names(out) <- fileList    
    
    if (asMatrix) out <- t(sapply(out, cor2vec))
    return(out)
}
