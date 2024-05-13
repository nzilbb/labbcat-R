#' Gets a list of IDs of transcripts that include the given participant
#'
#' Returns a list of IDs of transcripts (i.e. transcript names) that include
#' the given participant.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A participant ID
#' @return A list of transcript IDs
#' 
#' @seealso \code{\link{getParticipantIds}}
#' @examples 
#' \dontrun{
#' ## List transcripts in which UC427_ViktoriaPapp_A_ENG speaks
#' transcripts <- getTranscriptIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")
#' }
#' 
#' @keywords graph transcript
#' 
getTranscriptIdsWithParticipant <- function(labbcat.url, id) {
    resp <- store.get(labbcat.url, "getTranscriptIdsWithParticipant", list(id=id))
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    if (length(resp.json$model) > 0) {
        return(resp.json$model)
    } else { # ensure return type is the same as it would have been with elements
        return(character(0L))
    }
}
