#' Gets information about a single participant.
#'
#' Returns a nested named list with the participant information, including the given
#' participant attributes.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A participant ID
#' @param layer.ids A vector of layer IDs corresponding to participant attributes,
#'   eg. c('participant_gender', 'participant_year_of_birth')
#' @return A named list of representing the participant and its attributes, with members:
#' \itemize{
#'  \item{\emph{id} The participant's unique internal database ID}
#'  \item{\emph{label} The ID (name) of the participant}
#'  \item{\emph{annotations} A named list of participant attributes
#'  e.g. the label of the participant's 'gender' attribute would be:
#'  participant$annotations$participant_gender$label}
#' }
#' 
#' @seealso 
#'   \code{\link{getParticipantAttributes}}
#'   \code{\link{saveParticipant}}
#'   \code{\link{deleteParticipant}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get the gender and year of birth of AP511_MikeThorpe
#' participant <- getParticipant(labbcat.url, "AP511_MikeThorpe",
#'                   c("participant_gender", "participant_year_of_birth"))
#' 
#' print(paste("ID:", participant$label,
#'             "Gender:", participant$annotations$participant_gender$label,
#'             "YOB:", participant$annotations$participant_year_of_birth$label))
#' }
#'
#' @keywords transcript
#' 
getParticipant <- function(labbcat.url, id, layer.ids) {
    ## flatten lists into single newline-delimited string
    ## (because httr can't handle multiple parameters with the same name)
    layer.ids <- paste(layer.ids,collapse="\n")
    parameters <- list(id=id, layerIds=layer.ids)
    resp <- store.get(labbcat.url, "getParticipant", parameters)
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
