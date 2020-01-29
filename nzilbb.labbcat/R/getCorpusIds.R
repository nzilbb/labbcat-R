#' Gets a list of corpus IDs.
#' 
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of corpus IDs
#' 
#' @examples
#' \dontrun{
#' ## List corpora
#' corpora <- getCorpusIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords corpora
#' 
getCorpusIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getCorpusIds")
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model)
}
