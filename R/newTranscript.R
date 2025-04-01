#' Upload a new transcript
#'
#' This function adds a transcript and optionally a media file to the corpus.
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcript The path to the transcript to upload.
#' @param media The path to the media to upload, if any.
#' @param transcript.type The transcript type.
#' @param corpus The corpus to add the transcript to.
#' @param episode The transcript's episode.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return The ID of the new transcript in the corpus
#' 
#' @examples
#' \dontrun{
#' ## Get attributes for new transcript
#' corpus <- getCorpusIds(labbcat.url)[1]
#' transcript.type.layer <- getLayer(labbcat.url, "transcript_type")
#' transcript.type <- transcript.type.layer$validLabels[[1]]
#' 
#' ## upload transcript
#' newTranscript(
#'     labbcat.url, "my-transcript.eaf", "my-transcript.wav",
#'     "", transcript.type, corpus, "episode-1")
#' }
#' @keywords transcript management
#' 
newTranscript <- function(labbcat.url, transcript, media=NULL, 
                          transcript.type=NULL, corpus=NULL, episode=NULL, no.progress=FALSE) {
    
    ## upload file(s)
    parameters <- list(
        todo="new",
        auto="true",
        transcript_type=transcript.type,
        transcript_type=transcript.type,
        corpus=corpus,
        episode=episode,
        uploadfile1_0=httr::upload_file(transcript))
    if (!is.null(media)) {
        parameters$uploadmedia1 <- httr::upload_file(media) # TODO media.suffix
    }
    resp <- http.post.multipart(labbcat.url, "edit/transcript/new", parameters)
    
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
    transcript.id  <- names(resp.json$model$result[1])
    threadId <- resp.json$model$result[[1]]
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
