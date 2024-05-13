#' Gets transcript(s) in a given format
#' 
#' This function gets whole transcripts from 'LaBB-CAT', 
#' converted to a given format (by default, Praat TextGrid).
#'
#' \emph{NB} Although many formats will generate exactly one file for each interval
#'      (e.g. mime.type=text/praat-textgrid), this is not guaranted; some formats generate
#'      a single file or a fixed collection of files regardless of how many IDs there are.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The transcript ID (transcript name) of the sound recording, or
#'     a vector of transcript IDs. If the same ID appears more than one, the formatted
#'     file is downloaded only once.
#' @param layer.ids A vector of layer IDs.
#' @param mime.type Optional content-type - "text/praat-textgrid" is the default, but your
#'     LaBB-CAT installation may support other formats, which can be discovered using
#'     \link{getSerializerDescriptors}.
#' @param path Optional path to directory where the files should be saved.
#' @return The name of the file, which is saved in the current directory, or the given
#' path, or a list of names of files, if multiple id's were specified. 
#'
#' If a list of files is returned, they are in the order that they
#'     were returned by the server, which *should* be the order that
#'     they were specified in the id list.
#' 
#' @seealso \link{getSerializerDescriptors}
#' @examples
#' \dontrun{
#' ## Get the TextGrid of a recording
#' textgrid.file <- formatTranscript(labbcat.url, "AP2505_Nelson.eaf",
#'     c("word", "segment"), path="textgrids") 
#' 
#' ## Get all the transcripts of a given participant
#' transcript.ids <- getTranscriptIdsWithParticipant(labbcat.url, "AP2505_Nelson")
#' 
#' ## Download all the TextGrids, including the utterances, transcript, and segment layers
#' textgrid.files <- formatTranscript(
#'     labbcat.url, transcript.ids, c("utterance", "word", "segment"))
#' 
#' }
#' @keywords transcript TextGrid
#' 
formatTranscript <- function(labbcat.url, id, layer.ids, mime.type = "text/praat-textgrid", path="") {

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
    
    ## create list of repeated parameters
    layerParameters <- list()
    mapply(function(l) { layerParameters <<- c(layerParameters, list(layerId=l)) }, layer.ids)
    idParameters <- list()
    mapply(function(l) { idParameters <<- c(idParameters, list(id=l)) }, unique(id))

    parameters <- list(mimeType=mime.type)
    ## add list parameters
    parameters <- c(parameters, layerParameters)
    parameters <- c(parameters, idParameters)
    file.name <- paste(dir, "transcript.zip", sep="")

    file.names = c()
    tryCatch({
        resp <- http.post(labbcat.url, "api/serialize/graphs", parameters, file.name)
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
                file.rename(file.name, final.file.name)
                file.name <- final.file.name
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
        }
    }, error = function(e) {
        print(paste("ERROR:", e))
        file.name <<- NULL
    })
    return(file.names)   
}
