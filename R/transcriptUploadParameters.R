#' Set the parameters of a transcript already uploaded with transcriptUpload.
#'
#' The second part of a transcript upload process started by a call to transcriptUpload(),
#' which specifies values for the parameters required to save the uploaded transcript to
#' LaBB-CAT's database.
#'
#' If the response includes more parameters, then this method should be called again
#' to supply their values.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id Upload ID returned by the prior call to transcriptUpload().
#' @param parameters A named list where each name is the name of a parameter returned by
#'                   transcriptUpload(), and the value is the parameters value.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return The ID of the new transcript in the corpus
#' @seealso
#' - [transcriptUpload]
#' - [transcriptUploadDelete]
#' - [newTranscript]
#' - [updateTranscript]
#' @examples
#' \dontrun{
#' ## Get attributes for new transcript
#' corpus <- getCorpusIds(labbcat.url)[1]
#' transcript.type.layer <- getLayer(labbcat.url, "transcript_type")
#' transcript.type <- transcript.type.layer$validLabels[[1]]
#' 
#' ## upload transcript and its media
#' result <- transcriptUpload(labbcat.url, "my-transcript.eaf", "my-transcript.wav", FALSE)
#' 
#' ## use the default parameter values
#' parameterValues <- list()
#' for(p in 1:length(parameters$name)) parameterValues[parameters$name[p]] <- parameters$value[p]
#'
#' ## set the upload parameters to finalise the upload
#' transcript.id <- transcriptUploadParameters(labbcat.url, result$id, parameterValues)
#' }
#' @keywords transcript management
#' 
transcriptUploadParameters <- function(labbcat.url, id, parameters, no.progress=FALSE) {
    
    ## upload file(s)
    resp <- http.put(labbcat.url, paste0("api/edit/transcript/upload/", id), parameters)
    
    ## check response
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    if (length(resp.json$errors)) return()

    ## wait until the task is finished
    transcript.id  <- names(resp.json$model$transcripts[1])
    threadId <- resp.json$model$transcripts[[1]]
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)        
    }
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
        }
        close(pb)
        if (!is.null(thread$status)) {
            cat(paste(thread$status, "\n", sep=""))
        }
    }
    
    ## free the upload thread so it's not using server resources
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))
    
    return(transcript.id)
}
