#' Gets a layer definition
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id ID of the layer to get the definition for
#' @return The definition of the given layer, with members:
#'  - *id* The layer's unique ID
#'  - *parentId* The layer's parent layer ID
#'  - *description* The description of the layer
#'  - *alignment* The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment
#'  - *peers* Whether children have peers or not
#'  - *peersOverlap* Whether child peers can overlap or not
#'  - *parentIncludes* Whether the parent t-includes the child
#'  - *saturated* Whether children must temporally fill the entire parent duration (true) or not (false)
#'  - *parentIncludes* Whether the parent t-includes the child
#'  - *type* The type for labels on this layer
#'  - *validLabels* List of valid label values for this layer
#' 
#' @family Annotation layer functions
#' @examples
#' \dontrun{
#' ## Get the definition of the orthography layer
#' orthography.layer <- getLayer("https://labbcat.canterbury.ac.nz/demo/", "orthography")
#' }
#'
#' @keywords layer
#' 
getLayer <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getLayer", list(id=id))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 400) { # 400 = Bad request
            ## the content should be valid JSON with informative errors
            resp.json <- jsonlite::fromJSON(resp.content)
            for (error in resp.json$errors) print(paste("ERROR:", error))
        } else {
            print(paste("ERROR:", httr::http_status(resp)$message))
            print(resp.content)
        }
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model)
}
