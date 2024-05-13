#' Downloads a given media track for a given transcript
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param id A transcript ID (i.e. transcript name).
#' @param track.suffix The track suffix of the media.
#' @param mime.type The MIME type of the media, e.g. "audio/wav" or "application/f0".
#' @param path Optional path to directory where the file should be saved.
#' @return The name of the file, which is saved in the current directory, or the given
#'         path if specified
#' @seealso \link{getTranscriptIds}
#' @seealso \link{getMediaUrl}
#' @examples 
#' \dontrun{
#' ## Download the WAV file for BR2044_OllyOhlson.eaf
#' wav <- getMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
#' 
#' ## Download the 'QuakeFace' video file for BR2044_OllyOhlson.eaf
#' quakeFaceMp4 <- getMedia(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4")
#' }
#' 
#' @keywords media audio
#' 
getMedia <- function(labbcat.url, id, track.suffix = "", mime.type = "audio/wav", path="") {
    dir = path
    if (length(id) > 1) { ## multiple fragments
        ## save fragments into their own directory
        if (stringr::str_length(dir) == 0) dir <- "transcript"
    }
    if (stringr::str_length(dir) > 0) { ## directory is specified
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
    }

    url <- getMediaUrl(labbcat.url, id, track.suffix, mime.type)
    if (is.null(url)) return(NULL)
    file.name <- paste(dir, gsub(".*/", "", url), sep="")
    tryCatch({
        resp <- http.get(labbcat.url, gsub(labbcat.url, "", url), NULL, mime.type, file.name)
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
            if (httr::status_code(resp) != 404) { # 404 means the audio wasn't on the server
                ## some other error occurred so print what we got from the server
                print(readLines(file.name))
            }
            file.remove(file.name)
            file.name <<- NULL
        } else {
            content.disposition <- as.character(httr::headers(resp)["content-disposition"])
            content.disposition.parts <- strsplit(content.disposition, "=")
            if (length(content.disposition.parts[[1]]) > 1
                && file.name != content.disposition.parts[[1]][2]) {
                ## file name is specified, so use it
                final.file.name <- paste(dir, content.disposition.parts[[1]][2], sep="")
                if (final.file.name != file.name) {
                    file.rename(file.name, final.file.name)
                    file.name <- final.file.name
                }
            }            
        }
    }, error = function(e) {
        print(paste("ERROR:", e))
        file.name <<- NULL
    })
    return(file.name)   

    
}
