#' Update an existing transcript.
#'
#' This function uploads a new version of an existing transcript.
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcript.path The path to the transcript to upload.
#' @return The ID of the updated transcript in the corpus
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## upload new verison of transcript transcript
#' updateTranscript(labbcat.url, "my-transcript.eaf")
#' }
#' @keywords transcript management
#' 
updateTranscript <- function(labbcat.url, transcript.path) {
    
    ## upload file(s)
    parameters <- list(
        todo="update",
        auto="true",
        uploadfile1_0=httr::upload_file(transcript.path))
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
    if (interactive()) {
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
        if (!is.null(thread$status)) {
            cat(paste("\n", thread$status, "\n", sep=""))
        }
    }
    
    ## free the upload thread so it's not using server resources
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))
    
    return(transcript.id)
}
