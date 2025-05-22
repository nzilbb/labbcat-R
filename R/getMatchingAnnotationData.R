#' Gets binary data for annotations that match a particular pattern.
#' 
#' In some annotation layers, the annotations have not only a textual label, but also
#' binary data associated with it; e.g. an image or a data file. In these cases, the 'type'
#' of the layer is a MIME type, e.g. 'image/png'.
#' This function gets annotations that match the given expression on a MIME-typed layer,
#' and retrieves the binary data as files, whose names are returned by the function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which annotations match. This must
#'   match by either id or layer.id.
#'   The expression language is currently not well defined, but is based on JavaScript
#'   syntax. e.g.
#'   - id == 'e_144_17346'
#'   - ['e_144_17346', 'e_144_17347', 'e_144_17348'].includes(id)
#'   - layer.id == 'mediapipe' && graph.id == 'AdaAicheson-01.trs'
#' @param path Optional path to directory where the files should be saved.
#' @return The names of the files.
#' 
#' @seealso
#'   - [getMatchingAnnotations]
#'   - [getFragmentAnnotationData]
#' @examples
#' \dontrun{
#' ## Get mediapipe image annotations for the eleventh second of a transcript
#' expression = paste(sep="&&",
#'                "layer.id == 'mediapipe'",
#'                "graph.id == 'AP511_MikeThorpe.eaf'",
#'                "start.offset >= 10",
#'                "end.offset < 11")
#' png.files <- getMatchingAnnotationData(labbcat.url, expression, path="png")
#' }
#' @keywords sample fragment TextGrid
#' 
getMatchingAnnotationData <- function(labbcat.url, expression, path="") {

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

    parameters <- list(expression=expression)
    file.name <- paste0(dir, "data.zip")
    tryCatch({
        resp <- http.get(labbcat.url, "api/annotation/data", parameters, file.name = file.name)
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
            print(readLines(file.name))
            file.remove(file.name)
            file.name <<- NULL
        } else {
            content.disposition.filename <- fileNameFromContentDisposition(
                as.character(httr::headers(resp)["content-disposition"]))
            if (!is.null(content.disposition.filename)
                && file.name != content.disposition.filename) {
                ## file name is specified, so use it
                final.file.name <- paste(dir, content.disposition.filename, sep="")
                if (final.file.name != file.name) {
                    file.rename(file.name, final.file.name)
                    file.name <- final.file.name
                }
            }
        }
        if (endsWith(file.name, ".zip")) {
            ## list the files
            file.names <- paste(dir, unzip(file.name, list=T)$Name, sep="")
            
            ## unzip result
            unzip(file.name, exdir=dir)
            
            ## remove zip file
            file.remove(file.name)
        } else { ## a single file returned
            file.names = file.name
        }
    }, error = function(e) {
        print(paste("ERROR:", e))
        file.name <<- NULL
    })

    return(file.names)   
}
