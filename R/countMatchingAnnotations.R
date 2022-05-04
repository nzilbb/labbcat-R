#' Gets the number of annotations matching a particular pattern.
#'
#' The expression language is currently not well defined, but is based on JavaScript
#' syntax. e.g.
#' 
#' \itemize{
#'  \item{id == 'ew_0_456'}
#'  \item{['ew_2_456', 'ew_2_789', 'ew_2_101112'].includes(id)}
#'  \item{layerId == 'orthography' && !/th[aeiou].+/.test(label)}
#'  \item{layer.id == 'orthography' && first('participant').label == 'Robert'
#'    && first('utterances').start.offset = 12.345</code> - TODO</li> 
#'  \item{graph.id == 'AdaAicheson-01.trs' && layer.id == 'orthography' &&
#'    start.offset &gt; 10.5} 
#'  \item{layer.id == 'utterance' && all('word').includes('ew_0_456')}
#'  \item{previous.id = 'ew_0_456'</code> - TODO</li>
#'  \item{layerId = 'utterance' && labels('orthography').includes('foo')}
#'  \item{layerId = 'utterance' && labels('participant').includes('Ada')}
#' }
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param expression An expression that determines which annotations match. This must
#' match by either id or layer.id. 
#' @return The number of annotations that match the expression.
#' 
#' @seealso
#' \code{\link{getMatchingAnnotations}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' }
#' 
#' @keywords transcript
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
