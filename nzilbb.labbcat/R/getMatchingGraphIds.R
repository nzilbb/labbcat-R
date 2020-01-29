#' Gets a list of IDs of graphs that match a particular pattern.
#'
#' Gets a list of IDs of graphs (i.e. transcript names) that match a
#' particular pattern.
#'
#' The results can be exhaustive, by omitting pageLength and
#' pageNumber, or they  can be a subset (a 'page') of results, by
#' given pageLength and pageNumber values.
#'
#' The order of the list can be specified.  If ommitted, the graphs
#' are listed in ID order.
#'
#' The expression language is currently not well defined, but
#' expressions such as those in the examples can be used.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which graphs match
#' @param pageLength The maximum number of IDs to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @param order An expression that determines the order the graphs are
#' listed in - if specified, this must include the keyword 'ASC' for ascending or 'DESC'
#' for descending order.
#' @return A list of graph IDs (i.e. transcript names)
#' 
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all transcripts whose names start with "BR"
#' transcripts <- getMatchingGraphIds(labbcat.url, "id MATCHES 'BR.+'")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' transcripts <- getMatchingGraphIds(
#'         labbcat.url, "my('corpus').label = 'QB'", 20, 0)
#' 
#' ## Get the second transcript that has "QB247_Jacqui" as a speaker
#' transcripts <- getMatchingGraphIds(
#'         labbcat.url, "'QB247_Jacqui' IN labels('who')", 1, 1)
#' 
#' ## Get all transcripts whose names start with "BR" and have "QB247_Jacqui" as a speaker,
#' ## in word-count order 
#' transcripts <- getMatchingGraphIds(
#'         labbcat.url, "my('corpus').label = 'QB' AND 'QB247_Jacqui' IN labels('who')", 1, 1,
#'         "my('transcript_word_count').label ASC")
#' }
#' 
#' @keywords graph transcript expression
#' 
getMatchingGraphIds <- function(labbcat.url, expression, pageLength = NULL, pageNumber = NULL, order = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    if (!is.null(order)) parameters <- append(parameters, list(order=order))
    resp <- store.get(labbcat.url, "getMatchingGraphIds", parameters)
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
