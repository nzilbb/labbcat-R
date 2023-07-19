#' Deletes a participant record.
#'
#' This function deletes the identified participant from the corpus, but only if they do
#' not appear in any transcripts.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The participant ID - either the unique internal database ID, or their name.
#' @return TRUE if the participant's record was delete, FALSE otherwise.
#' 
#' @seealso 
#'   \code{\link{getParticipant}}
#'   \code{\link{saveParticipant}}
#' @examples 
#' \dontrun{
#' ## Create a new participant record
#' saveParticipant(labbcat.url, "Juan Perez")
#' 
#' ### Delete the participant we just created
#' deleteParticipant(labbcat.url, "Juan Perez")
#' }
#'
#' @keywords participant
#' 
deleteParticipant <- function(labbcat.url, id) {
    resp <- http.post(labbcat.url, "api/edit/store/deleteParticipant", list(id=id))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return(FALSE)
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    no.errors <- TRUE
    for (error in resp.json$errors) {
        print(error)
        no.errors <- FALSE
    }
    return(no.errors)
}
