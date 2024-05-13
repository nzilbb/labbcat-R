#' Gets a list of annotations that match a particular pattern
#'
#' Returns the annotations in the corpus that match the given expression.
#'
#' The results can be exhaustive, by omitting page.length and
#' page.number, or they  can be a subset (a 'page') of results, by
#' given page.length and page.number values.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which annotations match. This must
#'   match by either id or layer.id.
#'   The expression language is currently not well defined, but is based on JavaScript
#'   syntax. e.g.
#'   - id == 'ew_0_456'
#'   - ['ew_2_456', 'ew_2_789', 'ew_2_101112'].includes(id)
#'   - layerId == 'orthography' && !/th[aeiou].+/.test(label)
#'   - graph.id == 'AdaAicheson-01.trs' && layer.id == 'orthography' &&
#'     start.offset &gt; 10.5
#'   - layer.id == 'utterance' && all('word').includes('ew_0_456')
#'   - layerId = 'utterance' && labels('orthography').includes('foo')
#'   - layerId = 'utterance' && labels('participant').includes('Ada')
#' @param page.length The maximum number of IDs to return, or null to return all
#' @param page.number The zero-based page number to return, or null to return the first page
#' @return A list of annotations.
#' 
#' @seealso [countMatchingAnnotations]
#' @examples 
#' \dontrun{
#' ## get all topic annotations whose label includes the word 'quake'
#' quake.topics <- getMatchingAnnotations(
#'                    labbcat.url, "layer.id == 'topic' && /.*quake.*/.test(label)")
#' }
#' 
#' @keywords annotation expression
#' 
getMatchingAnnotations <- function(labbcat.url, expression, page.length = NULL, page.number = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(page.length)) parameters <- append(parameters, list(pageLength=page.length))
    if (!is.null(page.number)) parameters <- append(parameters, list(pageNumber=page.number))
    resp <- store.get(labbcat.url, "getMatchingAnnotations", parameters)
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
