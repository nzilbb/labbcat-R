#' Cancel a transcript upload started by a previous call to transcriptUpload().
#'
#' This cancels a transcript upload started by a previous call to transcriptUpload()
#' deleting any uploaded files from the server.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id Upload ID returned by the prior call to transcriptUpload().
#' @seealso
#' - [transcriptUpload]
#' - [transcriptUploadParameters]
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
#' ## Changed our mind, cancel this upload
#' transcriptUploadDelete(labbcat.url, result$id)
#' }
#' @keywords transcript management
#' 
transcriptUploadDelete <- function(labbcat.url, id) {
    
    ## upload file(s)
    resp <- http.delete(labbcat.url, paste0("api/edit/transcript/upload/", id))
    
    ## check response
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    return()
}
