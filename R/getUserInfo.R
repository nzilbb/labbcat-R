#' Gets information about the current user.
#' 
#' Returns information about the current user, including the roles or groups they are in.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A named list containing information about current the LaBB-CAT user.
#' 
#' @seealso \link{labbcatCredentials}
#' @examples
#' \dontrun{
#' ## List file export formats supported
#' me <- getUserInfo("https://labbcat.canterbury.ac.nz/demo/")
#' 
#' ## am I an administrator?
#' admin <- "admin" %in% me$roles
#' }
#' 
#' @keywords connect username
#' 
getUserInfo <- function(labbcat.url) {
    resp <- http.get(labbcat.url, "api/user")
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
