#' Gets fragments transcript in a given format.
#' 
#' This function gets fragments of transcripts from 'LaBB-CAT', 
#' converted to a given format (by default, Praat TextGrid).
#'
#' \emph{NB} Although many formats will generate exactly one file for each interval
#'      (e.g. mimeType=text/praat-textgrid), this is not guaranted; some formats generate
#'      a single file or a fixed collection of files regardless of how many fragments there are.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The transcript ID (transcript name) of the sound recording, or
#'     a vector of transcript IDs. 
#' @param start The start time in seconds, or a vector of start times.
#' @param end The end time in seconds, or a vector of end times.
#' @param layerIds A vector of layer IDs.
#' @param mimeType Optional content-type - "text/praat-textgrid" is the default, but your
#'     LaBB-CAT installation may support other formats, which can be discovered using
#'     \link{getSerializerDescriptors}.
#' @param path Optional path to directory where the files should be saved.
#' @return The name of the file, which is saved in the current
#'     directory, or a list of names of files, if multiple
#'     id's/start's/end's were specified 
#'
#' If a list of files is returned, they are in the order that they
#'     were returned by the server, which *should* be the order that
#'     they were specified in the id/start/end lists.
#' 
#' @seealso \link{getSerializerDescriptors}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get the 5 seconds starting from 10s after the beginning of a recording
#' textgrid.file <- getFragments(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0,
#'     c("transcript", "phonemes"), path="samples") 
#' 
#' ## Load some search results previously exported from LaBB-CAT
#' results <- read.csv("results.csv", header=T)
#' 
#' ## Get a list of fragment TextGrids, including the utterances, transcript, and phonemes layers
#' textgrid.files <- getFragments(
#'     labbcat.url, results$Transcript, results$Line, results$LineEnd,
#'     c("utterance", "word", "phonemes"))
#' 
#' ## Get a list of fragment TextGrids
#' textgrid.files <- getFragments(
#'     labbcat.url, results$Transcript, results$Line, results$LineEnd)
#' }
#' @keywords sample sound fragment wav
#' 
getFragments <- function(labbcat.url, id, start, end, layerIds, mimeType = "text/praat-textgrid", path="") {

    dir = path
    if (length(id) > 1) { ## multiple fragments
        ## save fragments into their own directory
        if (stringr::str_length(dir) == 0) dir <- "fragments"
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
    mapply(function(l) { layerParameters <<- c(layerParameters, list(layerId=l)) }, layerIds)
    idParameters <- list()
    mapply(function(l) { idParameters <<- c(idParameters, list(id=l)) }, id)
    startParameters <- list()
    mapply(function(l) { startParameters <<- c(startParameters, list(start=l)) }, start)
    endParameters <- list()
    mapply(function(l) { endParameters <<- c(endParameters, list(end=l)) }, end)

    parameters <- list(mimeType=mimeType)
    ## add list parameters
    parameters <- c(parameters, layerParameters)
    parameters <- c(parameters, idParameters)
    parameters <- c(parameters, startParameters)
    parameters <- c(parameters, endParameters)
    file.name <- paste(dir, "fragments.zip", sep="")

    file.names = c()
    tryCatch({
        resp <- http.post(labbcat.url, "api/serialize/fragment", parameters, file.name)
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

            ## list the files
            file.names <- paste(dir, unzip(file.name, list=T)$Name, sep="")

            ## unzip result
            unzip(file.name, exdir=dir)

            ## remove zip file
            file.remove(file.name)
        }
    }, error = function(e) {
        print(paste("ERROR:", e))
        file.name <<- NULL
    })
    return(file.names)   
}
