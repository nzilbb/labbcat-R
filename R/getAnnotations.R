#' Gets the annotations on the given layer of the given transcript
#'
#' Returns the annotations on the given layer of the given transcript.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A transcript ID (i.e. transcript name)
#' @param layer.id A layer ID
#' @param max.ordinal The maximum ordinal for the returned annotations. e.g. a max.ordinal
#' of 1 will ensure that only the first annotation for each parent is returned. If
#' max.ordinal is null, then all annotations are returned, regardless of their ordinal.
#' @param page.length The maximum number of annotations to return, or null to return all
#' @param page.number The zero-based page number to return, or null to return the first page
#' @return A named list of annotations, with members:
#'  - *id* The annotation's unique ID
#'  - *layerId* The name of the layer it comes from
#'  - *label* The value of the annotation
#'  - *startId* The ID of the start anchor,
#'  - *endId* The ID of the end anchor,
#'  - *parentId* The ID of the parent annotation,
#'  - *ordinal* The ordinal of the annotation among its peers,
#'  - *confidence* A rating from 0-100 of the confidence of the label
#'  e.g. 10: default value, 50: automatically generated, 100: manually annotated
#' 
#' @seealso 
#'   \code{\link{getTranscriptIds}}
#'   \code{\link{getTranscriptIdsInCorpus}}
#'   \code{\link{getTranscriptIdsWithParticipant}}
#'   \code{\link{countAnnotations}}
#' @examples 
#' \dontrun{
#' ## Get all the orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' }
#'
#' @keywords transcript
#' 
getAnnotations <- function(labbcat.url, id, layer.id, max.ordinal = NULL, page.length = NULL, page.number = NULL) {
    parameters <- list(id=id, layerId=layer.id)
    if (!is.null(max.ordinal)) parameters <- append(parameters, list(maxOrdinal=max.ordinal))
    if (!is.null(page.length)) parameters <- append(parameters, list(pageLength=page.length))
    if (!is.null(page.number)) parameters <- append(parameters, list(pageNumber=page.number))
    resp <- store.get(labbcat.url, "getAnnotations", parameters)
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
