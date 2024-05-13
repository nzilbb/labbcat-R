#' Gets a list of transcript in a corpus
#'
#' Returns a list of transcript IDs in the given corpus.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The ID (name) of the corpus
#' @return A list of transcript IDs
#' 
#' @examples 
#' \dontrun{
#' ## List transcripts in the QB corpus
#' transcripts <- getTranscriptIdsInCorpus("https://labbcat.canterbury.ac.nz/demo/", "QB")
#' }
#' 
#' @keywords corpora corpus
#' 
getTranscriptIdsInCorpus <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getTranscriptIdsInCorpus", list(id=id))
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
