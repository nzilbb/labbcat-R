#' Gets a list of corpus IDs.
#'
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The ID (name) of the corpus
#' @return A list of corpus IDs
#' 
#' @examples 
#' \dontrun{
#' ## List transcripts in the QB corpus
#' transcripts <- getGraphIdsInCorpus("https://labbcat.canterbury.ac.nz/demo/", "QB")
#' }
#' 
#' @keywords corpora corpus
#' 
getGraphIdsInCorpus <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getGraphIdsInCorpus", list(id=id))
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
