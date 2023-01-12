#' Gets a list of transcript IDs.
#'
#' Returns a list of transcript IDs (i.e. transcript names).
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of transcript IDs
#' 
#' @examples 
#' \dontrun{
#' ## List all transcripts
#' transcripts <- getTranscriptIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords transcript
#' 
getTranscriptIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getTranscriptIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    if (length(resp.json$model) > 0) {
        return(resp.json$model)
    } else { # ensure return type is the same as it would have been with elements
        return(character(0L))
    }
}
