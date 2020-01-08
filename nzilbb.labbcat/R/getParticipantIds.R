#' Gets a list of participant IDs.
#'
#' Returns a list of participant IDs.
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of participant IDs
#' 
#' @examples
#' \dontrun{
#' ## List all speakers
#' speakers <- getParticipantIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords speaker participant
#' 
getParticipantIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getParticipantIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model$result)
}
