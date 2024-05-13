#' Gets the value of the given system attribute
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param attribute Name of the attribute.
#' @return The value of the given attribute.
#' 
#' @seealso [getLayers]
#' @examples
#' \dontrun{
#' ## Get the name of the LaBB-CAT instance
#' title <- getSystemAttribute("https://labbcat.canterbury.ac.nz/demo/", "title")
#' }
#' 
getSystemAttribute <- function(labbcat.url, attribute) {
    resp <- http.get(labbcat.url, paste("api/systemattributes", attribute, sep="/"))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 400) { # 400 = Bad request
            # the content should be valid JSON with informative errors
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
    return(resp.json$model$value)
}
