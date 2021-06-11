#' Gets a list of IDs of transcripts that match a particular pattern.
#'
#' Gets a list of IDs of transcripts (i.e. transcript names) that match a
#' particular pattern.
#'
#' The results can be exhaustive, by omitting page.length and
#' page.number, or they  can be a subset (a 'page') of results, by
#' given page.length and page.number values.
#'
#' The order of the list can be specified.  If ommitted, the transcripts
#' are listed in ID order.
#'
#' The expression language is currently not well defined, but
#' expressions such as those in the examples can be used.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which transcripts match
#' @param page.length The maximum number of IDs to return, or null to return all
#' @param page.number The zero-based page number to return, or null to return the first page
#' @param order An expression that determines the order the transcripts are
#' listed in - if specified, this must include the keyword 'ASC' for ascending or 'DESC'
#' for descending order.
#' @return A list of transcript IDs (i.e. transcript names)
#' 
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all transcripts whose names start with "BR"
#' transcripts <- getMatchingTranscriptIds(labbcat.url, "id MATCHES 'BR.+'")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' transcripts <- getMatchingTranscriptIds(
#'         labbcat.url, "my('corpus').label = 'QB'", 20, 0)
#' 
#' ## Get the second transcript that has "QB247_Jacqui" as a speaker
#' transcripts <- getMatchingTranscriptIds(
#'         labbcat.url, "'QB247_Jacqui' IN labels('participant')", 1, 1)
#' 
#' ## Get all transcripts whose names start with "BR" and have "QB247_Jacqui" as a speaker,
#' ## in word-count order 
#' transcripts <- getMatchingTranscriptIds(
#'         labbcat.url, "my('corpus').label = 'QB' AND 'QB247_Jacqui' IN labels('participant')", 1, 1,
#'         "my('transcript_word_count').label ASC")
#' }
#' 
#' @keywords transcript expression
#' 
getMatchingTranscriptIds <- function(labbcat.url, expression, page.length = NULL, page.number = NULL, order = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(page.length)) parameters <- append(parameters, list(pageLength=page.length))
    if (!is.null(page.number)) parameters <- append(parameters, list(pageNumber=page.number))
    if (!is.null(order)) parameters <- append(parameters, list(order=order))
    resp <- store.get(labbcat.url, "getMatchingTranscriptIds", parameters)
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
