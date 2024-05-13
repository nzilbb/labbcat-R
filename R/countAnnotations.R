#' Gets the number of annotations on the given layer of the given transcript
#'
#' Returns the number of annotations on the given layer of the given
#' transcript.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A transcript ID (i.e. transcript name)
#' @param layer.id A layer ID
#' @param max.ordinal The maximum ordinal for the counted annotations. e.g. a max.ordinal
#'   of 1 will ensure that only the first annotation for each parent is returned. If
#'   max.ordinal is null, then all annotations are counted, regardless of their ordinal.
#' @return The number of annotations on that layer
#' 
#' @seealso
#' - [getTranscriptIds]
#' - [getTranscriptIdsInCorpus]
#' - [getTranscriptIdsWithParticipant]
#' @examples 
#' \dontrun{
#' ## Count the number of words in UC427_ViktoriaPapp_A_ENG.eaf
#' token.count <- countAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' }
#' 
#' @keywords transcript
#' 
countAnnotations <- function(labbcat.url, id, layer.id, max.ordinal = NULL) {
    parameters <- list(id=id, layerId=layer.id)
    if (!is.null(max.ordinal)) parameters <- append(parameters, list(maxOrdinal=max.ordinal))
    resp <- store.get(labbcat.url, "countAnnotations", parameters)
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
