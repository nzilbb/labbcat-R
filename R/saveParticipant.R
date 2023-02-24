#' Saves information about a single participant.
#'
#' This function allows the participant attributes and the ID of a given participant to be
#' updated.
#'
#' To change the ID of an existing participant, pass the old/current ID as the \code{id},
#' and pass the new ID as the \code{label}.
#'
#' If the participant ID does not already exist in the database, a new participant record
#' is created. 
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The participant ID - either the unique internal database ID, or their name.
#' @param label The new ID (name) for the participant
#' @param attributes A named list of participant attribute values - the names are the
#'   participant attribute layer IDs, and the values are the corresponding new attribute
#'   values. 
#' @return TRUE if the participant's record was updated, FALSE if there were no changes detected.
#' 
#' @seealso 
#'   \code{\link{getParticipant}}
#'   \code{\link{deleteParticipant}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Create a new participant record
#' saveParticipant(labbcat.url, "Juan Perez", attributes=list(participant_gender="M"))
#' 
#' ## Change the name and the gender of the participant record
#' saveParticipant(labbcat.url, "Juan Perez", "María Perez", list(participant_gender="F"))
#' 
#' ### Delete the participant we just created
#' deleteParticipant(labbcat.url, "María Perez")
#' }
#'
#' @keywords participant
#' 
saveParticipant <- function(labbcat.url, id, label = id, attributes = NULL) {
    if (is.null(attributes)) attributes = list()
    parameters <- attributes
    parameters$id <- id
    parameters$label <- label
    resp <- http.post(labbcat.url, "api/edit/store/saveParticipant", parameters)
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
