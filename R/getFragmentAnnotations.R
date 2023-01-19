#' Gets annotations in fragments.
#' 
#' This function gets annotations between given start/end times on given layers. If more
#' than one annotation matches, labels are concatentated together.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcript.id The transcript ID (transcript name) of the sound recording, or
#'     a vector of transcript IDs. 
#' @param participant.id The participant ID of the annotations, or a vector of participant IDs.  
#' @param start The start time in seconds, or a vector of start times.
#' @param end The end time in seconds, or a vector of end times.
#' @param layer.ids A vector of layer IDs.
#' @param sep The separator to use when concatenating labels when multiple annotations are
#'     in the given interval.
#' @param partial.containment Whether to include annotations that are only partially
#'     contained in the given interval.
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'     shown when interactive().
#' @return A data frame with three columns for each layer in layer.ids:
#'
#' \itemize{
#'  \item{The annotation labels concatenated together}
#'  \item{The start time of the first annotation}
#'  \item{The end time of the last annotation}
#' }
#' 
#' @seealso \link{getFragments}
#' @seealso \link{getSoundFragments}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get some span-layer intervales
#' topics <- getMatches(labbcat.url, list(topic = ".*quake.*"))
#' 
#' ## Get concantenated word tokens for each topic annotation
#' topic.tokens <- getFragmentAnnotations(
#'     labbcat.url, topics$Transcript, topics$Participant, topics$topic.start, topics$topic.end,
#'     c("word"))
#' }
#' @keywords sample fragment TextGrid
#' 
getFragmentAnnotations <- function(labbcat.url, transcript.id, participant.id, start, end,
                                   layer.ids, sep=" ", partial.containment=FALSE,
                                   no.progress=FALSE) {

    ## save keys to a CSV file
    upload.file = tempfile(pattern="getFragmentAnnotations.", fileext=".csv")
    write.table(data.frame(transcript.id, participant.id, start, end),
                upload.file, sep=",", row.names=FALSE, col.names=TRUE)

    ## upload CSV
    containment <- "entire"
    if (partial.containment) containment <- "partial"
    parameters <- list(
        transcript="0",
        participant="1",
        starttime="2", endtime="3",
        containment=containment,
        labelDelimiter=sep,
        csvFieldDelimiter=",",
        copyColumns="false",
        csv=httr::upload_file(upload.file))
    layerParameters <- list()
    mapply(function(l) { layerParameters <<- c(layerParameters, list(layerId=l)) }, layer.ids)
    parameters <- c(parameters, layerParameters)
    resp <- http.post.multipart(labbcat.url, "extractIntervals", parameters)

    ## tidily remove the uploaded file
    file.remove(upload.file)

    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)

    ## we get a task ID back
    threadId <- resp.json$model$threadId

    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)        
    }

    ## monitor the task until it finishes
    thread <- thread.get(labbcat.url, threadId)
    if (is.null(thread)) {
        return()
    }
    while (thread$running) {
        if (!is.null(pb) && !is.null(thread$percentComplete)) {
            setTxtProgressBar(pb, thread$percentComplete)
        }
        Sys.sleep(1)
        thread <- thread.get(labbcat.url, threadId)
        if (is.null(thread)) {
            return()
        }
    } # poll until finished
    if (!is.null(pb)) {
        if (!is.null(thread$percentComplete)) {
            setTxtProgressBar(pb, thread$percentComplete)
            close(pb)
        }
        if (!is.null(thread$status)) {
            cat(paste(thread$status, "\n", sep=""))
        }
    }

    ## download resulting CSV
    download.file = tempfile(pattern="annotations.", fileext=".csv")
    resp <- httr::GET(thread$resultUrl,
                      httr::write_disk(download.file, overwrite=TRUE),
                      httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)))
    ## load data into frame
    ## (if we don't skip blank lines we get a row too many)
    results <- read.csv(download.file, header=T, blank.lines.skip=T)

    ## tidy up
    file.remove(download.file)
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))
    
    return(results)
}
