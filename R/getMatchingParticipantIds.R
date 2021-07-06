#' Gets a list of IDs of participants that match a particular pattern.
#'
#' Gets a list of IDs of participants that match a particular pattern.
#'
#' The results can be exhaustive, by omitting page.length and
#' page.number, or they  can be a subset (a 'page') of results, by
#' given page.length and page.number values.
#'
#' The expression language is currently not well defined, but
#' expressions such as those in the examples can be used.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which participants match
#' @param page.length The maximum number of IDs to return, or null to return all
#' @param page.number The zero-based page number to return, or null to return the first page
#' @return A list of paricipant IDs
#' 
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all participants whose IDs start with "BR"
#' participants <- getMatchingParticipantIds(labbcat.url, "/^BR.+/.test(id)")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' participants <- getMatchingParticipantIds(
#'         labbcat.url, "labels('corpus').includes('QB')", 20, 0)
#' 
#' ## Get all participants in the "QB" corpus that have "Jacqui" as part of the ID
#' participants <- getMatchingTranscriptParticipantIds(
#'         labbcat.url, "labels('corpus').includes('QB') && /^BR.+/.test(id)", 1, 1)
#' }
#' 
#' @keywords transcript expression
#' 
getMatchingParticipantIds <- function(labbcat.url, expression, page.length = NULL, page.number = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(page.length)) parameters <- append(parameters, list(pageLength=page.length))
    if (!is.null(page.number)) parameters <- append(parameters, list(pageNumber=page.number))
    resp <- store.get(labbcat.url, "getMatchingParticipantIds", parameters)
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
