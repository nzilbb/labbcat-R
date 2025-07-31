#' List the dictionaries available
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A named list of layer manager IDs, each of which containing a list of
#' dictionaries that the layer manager makes available.
#' 
#' @family dictionary functions
#' @examples 
#' \dontrun{
#' ## List the dictionaries available
#' dictionaries <- getDictionaries("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords dictionary
#' 
getDictionaries <- function(labbcat.url) {
    resp <- http.get(labbcat.url, "api/dictionaries")
    if (httr::status_code(resp) == 404) { # endpoint not there, fall back to old endpoint
        resp <- http.get(labbcat.url, "dictionaries")
    }
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
