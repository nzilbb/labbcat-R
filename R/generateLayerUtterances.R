#' Generates a layer for a given set of utterances
#'
#' Generates annotations on a given layer for a given set of utterances, e.g. force-align
#' selected utterances of a participant.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param match.ids A vector of annotation IDs, e.g. the MatchId column, or the URL column,
#'     of a results set. 
#' @param layer.id The ID of the layer to generate.
#' @param collection.name An optional name for the collection, e.g. the participant ID.
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'     shown when interactive().
#' @return The final status of the layer generation task.
#' 
#' @seealso
#' \code{\link{getAllUtterances}}
#' @examples
#' \dontrun{
#' ## Get all utterances of a participant
#' allUtterances <- getAllUtterances(labbcat.url, "AP2505_Nelson")
#' 
#' ## Force-align the participant's utterances
#' generateLayerUtterances(labbcat.url, allUtterances$MatchId, "htk", "AP2505_Nelson")
#' }
#' 
#' @keywords layer annotation label
#' 
generateLayerUtterances <- function(labbcat.url, match.ids, layer.id, collection.name=NULL, no.progress=FALSE) {    

    ## make request
    parameters <- list(
        generate_layer=layer.id,
        collection_name=collection.name,
        todo="generate-now",
        utterances=paste(match.ids,collapse="\n"))
    resp <- http.post(labbcat.url, "generateLayerUtterances", parameters)
    
    ## check the response
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
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))

    if (!is.null(pb)) { ## if there was a progress bar, 
        close(pb)
    }    
    return(thread$status)
}
