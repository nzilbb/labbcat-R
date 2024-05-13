#' Gets the store's ID
#' 
#' The store's ID - i.e. the ID of the 'LaBB-CAT' instance.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return The annotation store's ID
#' @examples
#' \dontrun{
#' ## Get ID of LaBB-CAT instance
#' instance.id <- getId("https://labbcat.canterbury.ac.nz/demo/")
#' }
#'
getId <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getId")
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

