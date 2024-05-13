#' Gets the number of annotations matching a particular pattern.
#'
#' Returns the number of annotations in the corpus that match the given expression.
#' 
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which annotations match. This must
#' match by either id or layer.id.
#' The expression language is currently not well defined, but is based on JavaScript
#' syntax. e.g.:
#' - `id == 'ew_0_456'`
#' - `['ew_2_456', 'ew_2_789', 'ew_2_101112'].includes(id)`
#' - `layerId == 'orthography' && !/th[aeiou].+/.test(label)`
#' - `graph.id == 'AdaAicheson-01.trs' && layer.id == 'orthography' &&
#'    start.offset &gt; 10.5` 
#' - `layer.id == 'utterance' && all('word').includes('ew_0_456')`
#' - `layerId = 'utterance' && labels('orthography').includes('foo')`
#' - `layerId = 'utterance' && labels('participant').includes('Ada')`
#' 
#' @return The number of annotations that match the expression.
#' 
#' @seealso
#' \code{\link{getMatchingAnnotations}}
#' @examples 
#' \dontrun{
#' ## count the number of topic tags that include the word 'quake'
#' countMatchingAnnotations(labbcat.url, "layer.id == 'topic' && /.*quake.*/.test(label)")
#' }
#' 
#' @keywords annotation expression
#' 
countMatchingAnnotations <- function(labbcat.url, expression) {
    parameters <- list(expression=expression)
    resp <- store.get(labbcat.url, "countMatchingAnnotations", parameters)
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
