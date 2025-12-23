#' Generates a layer
#'
#' Generates annotations on a given layer for all transcripts in the corpus.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param layer.id The ID of the layer to generate.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return The final status of the layer generation task.
#' 
#' @family Annotation layer functions
#' @seealso [getAllUtterances]
#' @examples
#' \dontrun{
#' ## Generate all phonemic transcription annotations
#' generateLayer(labbcat.url, "phonemes")
#' }
#' 
#' @keywords layer annotation label
#' 
generateLayer <- function(labbcat.url, layer.id, no.progress=FALSE) {    

    ## make request
    parameters <- list(
        layerId=layer.id,
        sure="true")
    resp <- http.post(labbcat.url, "admin/layers/regenerate", parameters)
    
    ## check the reponse
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

    ## wait for task to complete
    threadId <- resp.json$model$threadId
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

    ## free the thread so it's not using server resources
    thread.release(labbcat.url, threadId)

    if (!is.null(pb)) { ## if there was a progress bar, 
        close(pb)
    }    
    return(thread$status)
}
