#' Gets binary annotation data in fragments.
#' 
#' In some annotation layers, the annotations have not only a textual label, but also
#' binary data associated with it; e.g. an image or a data file. In these cases, the 'type'
#' of the layer is a MIME type, e.g. 'image/png'.
#' This function gets annotations between given start/end times on the given MIME-typed layer,
#' and retrieves the binary data as files, whose names are returned by the function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcript.id The transcript ID (transcript name) of the sound recording, or
#'   a vector of transcript IDs. 
#' @param participant.id The participant ID of the annotations, or a vector of participant IDs.  
#' @param start The start time in seconds, or a vector of start times.
#' @param end The end time in seconds, or a vector of end times.
#' @param layer.id The ID of the MIME-typed layer.
#' @param path Optional path to directory where the files should be saved.
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return The names of the files.
#' 
#' @seealso
#'   - [getFragmentAnnotations]
#'   - [getFragments]
#'   - [getSoundFragments]
#' @examples
#' \dontrun{
#' ## Get mediapipe image annotations for the eleventh second of a transcript
#' png.files <- getFragmentAnnotationData(
#'     labbcat.url, c("AP511_MikeThorpe.eaf"), c(10), c(11), c("mediapipe"), path = "png")
#' }
#' @keywords sample fragment TextGrid
#' 
getFragmentAnnotationData <- function(labbcat.url, transcript.id,
                                      start, end, layer.id, path="", no.progress=FALSE) {

    layer <- getLayer(labbcat.url, layer.id)
    if (is.null(layer)) {
        print(paste("Not a valid layer ID:", layer.id))
        return(NULL)
    } else if (!grepl("/",layer$type)) {
        print(paste("Layer does not contain binary data:", layer.id))
        return(NULL)
    }

    dir = path
    if (stringr::str_length(dir) == 0) dir <- "fragments"
    ## if it wasn't explicitly specified...
    if (file.exists(dir) && stringr::str_length(path) == 0) { 
        ## ensure it's a new directory by adding a number
        n <- 1
        new.dir = paste(dir,"(",n,")", sep="")
        while (file.exists(new.dir)) {
            n <- n + 1
            new.dir = paste(dir,"(",n,")", sep="")
        } # next try
        dir <- new.dir
    }
    if (!file.exists(dir)) dir.create(dir)
    ## add trailing slash if there isn't one
    if (!grepl(paste("\\", .Platform$file.sep, "$", sep=""), dir)) {
        dir <- paste(dir, .Platform$file.sep, sep="")
    }

    file.names = c()
    
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = length(transcript.id), style = 3)
    }

    ## we process the intervals one at a time
    for (i in 1:length(transcript.id)) {
        graph.id <- transcript.id[[i]]
        start.offset <- start[[i]]
        end.offset <- end[[i]]        
        expression <- paste0("layer.id == '", layer.id, "'",
                             " && graph.id == '", graph.id, "'",
                             " && start.offset >= ", start.offset,
                             " && end.offset < ", end.offset)
        interval.files <- getMatchingAnnotationData(labbcat.url, expression, path = dir)
        file.names <- c(file.names, interval.files)
        
        if (!is.null(pb)) setTxtProgressBar(pb, i)
    } # next interval
    
    if (!is.null(pb)) { ## if there was a progress bar, 
        close(pb)
    }    
    
    return(file.names)   
}
