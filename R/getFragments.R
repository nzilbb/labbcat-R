#' Gets transcript fragments in a given format
#' 
#' This function gets fragments of transcripts from 'LaBB-CAT', 
#' converted to a given format (by default, Praat TextGrid).
#'
#' **NB** Although many formats will generate exactly one file for each interval
#'      (e.g. mime.type=text/praat-textgrid), this is not guaranteed; some formats generate
#'      a single file or a fixed collection of files regardless of how many fragments there are.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The transcript ID (transcript name) of the sound recording, or
#'   a vector of transcript IDs. 
#' @param start The start time in seconds, or a vector of start times.
#' @param end The end time in seconds, or a vector of end times.
#' @param layer.ids A vector of layer IDs.
#' @param mime.type Optional content-type - "text/praat-textgrid" is the default, but your
#'   LaBB-CAT installation may support other formats, which can be discovered using
#'   [getSerializerDescriptors].
#' @param path Optional path to directory where the files should be saved.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return The name of the file, which is saved in the current
#'   directory, or a list of names of files, if multiple
#'   id's/start's/end's were specified 
#'
#'   If a list of files is returned, they are in the order that they
#'   were returned by the server, which *should* be the order that
#'   they were specified in the id/start/end lists.
#' 
#' @seealso [getSerializerDescriptors]
#' @examples
#' \dontrun{
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
#' @keywords sample fragment TextGrid
#' 
getFragments <- function(labbcat.url, id, start, end, layer.ids, mime.type = "text/praat-textgrid", path="", no.progress=FALSE) {

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
    
    pb <- NULL
    if (interactive() && !no.progress && length(id) > 1) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)        
    }

    ## loop through each tuple, getting fragments individually
    ## (we could actually pass the lot to LaBB-CAT in one go and get a ZIP file back
    ##  but then we can't be sure the results contain a row for every fragment specified
    ##  and we can't display a progress bar)
    file.names = c()
    r <- 1
    names(layer.ids) <- rep_len("layerId", length(layer.ids))
    base.params <- c(list(mimeType=mime.type), layer.ids)
    
    for (graph.id in id) {
        parameters <- c(base.params, id=graph.id, start=start[r], end=end[r])
        
        file.name <- paste(dir, stringr::str_replace(graph.id, "\\.[^.]+$",""), "__", start[r], "-", end[r], ".TextGrid", sep="")
        tryCatch({
            resp <- http.post(labbcat.url, "api/serialize/fragments", parameters, file.name)
            if (httr::status_code(resp) != 200) { # 200 = OK
                print(paste("ERROR: ", httr::http_status(resp)$message))
                if (httr::status_code(resp) != 404) { # 404 means the audio wasn't on the server
                    ## some other error occurred so print what we got from the server
                    print(readLines(file.name))
                }
                file.remove(file.name)
                file.name <<- NULL
            } else {
                content.disposition.filename <- fileNameFromContentDisposition(
                    as.character(httr::headers(resp)["content-disposition"]))
                if (!is.null(content.disposition.filename)
                    && file.name != content.disposition.filename) {
                    ## file name is specified, so use it
                    final.file.name <- paste(dir, content.disposition.filename, sep="")
                    file.rename(file.name, final.file.name)
                    file.name <- final.file.name
                }
            }
        }, error = function(e) {
            print(paste("ERROR:", e))
            file.name <<- NULL
        })
        file.names <- append(file.names, file.name)
        
        if (!is.null(pb)) setTxtProgressBar(pb, r)
        r <- r+1
    } ## next row
    
    if (!is.null(pb)) close(pb)
    return(file.names)
}
