#' Delete a transcript from the corpus.
#'
#' This function deletes the given transcript, and all associated files.
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The ID transcript to delete.
#' @return The ID of the deleted transcript
#' 
#' @examples
#' \dontrun{
#' ## delete a transcript from the server
#' deleteTranscript(labbcat.url, "my-transcript.eaf")
#' }
#' @keywords transcript management
#' 
deleteTranscript <- function(labbcat.url, id) {
    
    ## delete transcript
    resp <- http.post(labbcat.url, "edit/store/deleteTranscript", list(id=id))
    
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
    if (length(resp.json$errors)) return()

    return(id)
}
