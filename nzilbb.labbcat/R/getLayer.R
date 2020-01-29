#' Gets a layer definition.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id ID of the layer to get the definition for
#' @return The definition of the given layer, with members:
#' \itemize{
#'  \item{\emph{id} The layer's unique ID}
#'  \item{\emph{parentId} The layer's parent layer ID}
#'  \item{\emph{description} The description of the layer}
#'  \item{\emph{alignment} The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment}
#'  \item{\emph{peers} Whether children have peers or not}
#'  \item{\emph{peersOverlap} Whether child peers can overlap or not}
#'  \item{\emph{parentIncludes} Whether the parent t-includes the child}
#'  \item{\emph{saturated} Whether children must temporally fill the entire parent duration (true) or not (false)}
#'  \item{\emph{parentIncludes} Whether the parent t-includes the child}
#'  \item{\emph{type} The type for labels on this layer}
#'  \item{\emph{validLabels} List of valid label values for this layer}
#' }
#' 
#' @seealso \code{\link{getLayerIds}}
#' \code{\link{getLayers}}
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
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model)
}
