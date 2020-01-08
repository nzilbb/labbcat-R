#' Gets a list of graph IDs.
#'
#' Returns a list of graph IDs (i.e. transcript names).
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of graph IDs
#' 
#' @examples 
#' \dontrun{
#' ## List all transcripts
#' transcripts <- getGraphIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords graph transcript
#' 
getGraphIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getGraphIds")
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
