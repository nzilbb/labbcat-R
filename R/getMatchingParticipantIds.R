#' Gets a list of IDs of participants that match a particular pattern.
#'
#' Gets a list of IDs of participants that match a particular pattern.
#'
#' The results can be exhaustive, by omitting page.length and
#' page.number, or they  can be a subset (a 'page') of results, by
#' given page.length and page.number values.
#'
#' The expression language is currently not well defined, but is based on JavaScript
#' syntax.
#' 
#'  - The *labels* function can be used to represent a list of all the annotation
#'        labels on a given layer. For example, each participant can have multiple
#'        corpora, so the corpus labels (names) are represented by:  
#'        `labels('corpus')`
#'  - Use the *includes* function on a list to test whether the list contains a
#'        given element. e.g. to match participants that include the corpus 'QB' use:  
#'        `labels('corpus').includes('QB')`
#'  - Use the *first* function to identify the first (or the only) annotation on
#'        a given layer. e.g. the annotation representing the participant's gender is:  
#'        `first('participant_gender')`
#'  - Single annotations have various attributes, including 'id', 'label', 'ordinal', etc.
#'        e.g. the label of the participant's gender is:  
#'        `first('participant_gender').label`
#'  - Regular expressions can be matched by using expressions like
#'        '/regex/.test(str)', e.g. to test if the ID starts with 'BR' use:  
#'        `/^BR.+/.test(id)`  
#'        or to test if the participant's gender includes 'binary' use:  
#'        `/.*binary.*/.test(first('participant_gender').label)`
#' 
#' Expressions such as those in the examples can be used.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which participants match
#' @param page.length The maximum number of IDs to return, or null to return all
#' @param page.number The zero-based page number to return, or null to return the first page
#' @return A list of paricipant IDs
#' 
#' @examples 
#' \dontrun{
#' ## Get all participants whose IDs start with "BR"
#' participants <- getMatchingParticipantIds(labbcat.url, "/^BR.+/.test(id)")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' participants <- getMatchingParticipantIds(
#'         labbcat.url, "labels('corpus').includes('QB')", 20, 0)
#' 
#' ## Get all participants in the "QB" corpus that have "Jacqui" as part of the ID
#' participants <- getMatchingTranscriptParticipantIds(
#'         labbcat.url, "labels('corpus').includes('QB') && /^BR.+/.test(id)")
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
