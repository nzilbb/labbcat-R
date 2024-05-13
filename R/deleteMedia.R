#' Delete a transcript's media file
#'
#' This function deletes the given media file associated with the given transcript.
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The ID transcript whose media will be deleted.
#' @param file.name The media file name, e.g. media.file$name
#'
#' @seealso
#' - [getAvailableMedia]
#' - [saveMedia]
#' 
#' @examples
#' \dontrun{
#' ## delete the mp3 file of a transcript from the server
#' deleteMedia(labbcat.url, "my-transcript.eaf", "my-transcript.mp3")
#' }
#' @keywords transcript management
#' 
deleteMedia <- function(labbcat.url, id, file.name) {
    
    ## delete transcript
    resp <- http.post(labbcat.url, "edit/store/deleteMedia", list(id=id, fileName=file.name))
    
    ## check response
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return()
}
