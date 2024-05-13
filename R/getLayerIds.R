#' Gets a list of layer IDs
#'
#' Layer IDs are annotation 'types'.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of layer IDs
#' 
#' @family Annotation layer functions
#' @examples
#' \dontrun{
#' ## Get names of all layers
#' layer.ids <- getLayerIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords layer
#' 
getLayerIds <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getLayerIds")
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
