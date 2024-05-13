#' Gets a list of layer definitions
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of layer definitions, with members:
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
#' @seealso \code{\link{getLayerIds}}
#' @examples
#' \dontrun{
#' ## Get definitions of all layers
#' layers <- getLayers("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords layer
#' 
getLayers <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getLayers")
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
