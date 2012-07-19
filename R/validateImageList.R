    pipelineNames <- sapply(imageList, function(x) x$pipelineName)
    if (is.null(pipelineNames)) warning("Checking pipeline and no subjects have named pipelines")
    else if (length(unique(pipelineNames)) != 1) warning("Some subjects have different pipelineNames")
    templateNames <- sapply(imageList, function(x) x$templateFilename)
    if (is.null(templateNames)) warning("Checking template and no subjects have template file names")
    else if (length(unique(templateNames)) != 1) warning("Some subjects have different templates")

