#' Gets participant attribute values for given participant IDs
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param participant.ids A vector of participant IDs
#' @param layer.ids A vector of layer IDs corresponding to participant attributes. In
#'   general, these are layers whose ID is prefixed 'participant_', however formally it's
#'   any layer where layer$parentId == 'participant' && layer$alignment == 0.
#' @return A data frame of attribute value labels.
#' 
#' @examples
#' \dontrun{
#' ## Get gender and age for all participants
#' attributes <- getParticipantAttributes(labbcat.url,
#'             getParticipantIds(labbcat.url),
#'             c('participant_gender', 'participant_age'))
#' 
#' }
#' 
#' @keywords layer annotation label
#' 
getParticipantAttributes <- function(labbcat.url, participant.ids, layer.ids) {    
    ## validate layer Ids
    for (layerId in layer.ids) {
        layer <- getLayer(labbcat.url, layerId)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
        ## check it's an attribute
        if (layer$parentId != 'participant' || layer$alignment != 0) {
            print(paste("ERROR:", layerId, ' is not a participant attribute'))
            return()
        }
    } # next layer
    
    ## save labels to a CSV file
    download.file = tempfile(pattern="participant-attributes.", fileext=".csv")

    ## flatten lists into single newine-delimited string
    ## (because httr can't handle multiple parameters with the same name)
    layer.ids <- paste(layer.ids,collapse="\n")
    participant.ids <- paste(participant.ids,collapse="\n")

    ## make request
    parameters <- list(
        type="participant", "content-type"="text/csv", csvFieldDelimiter=",",
        layers=layer.ids,
        participantIds=participant.ids)
    resp <- http.post(labbcat.url, "participantsExport", parameters, download.file)
    
    ## check the reponse
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }

    ## load the returned entries
    attributes <- read.csv(download.file, header=T, blank.lines.skip=F)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(attributes)
}
