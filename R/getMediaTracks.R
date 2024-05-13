#' List the predefined media tracks available for transcripts
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of media track definitions.
#' @examples 
#' \dontrun{
#' ## Get the media tracks configured in LaBB-CAT
#' tracks <- getMediaTracks("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords media sound
#' 
getMediaTracks <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getMediaTracks")
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
