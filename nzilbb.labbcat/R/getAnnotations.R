#' Gets the annotations on the given layer of the given graph.
#'
#' Returns the annotations on the given layer of the given graph
#' (transcript).
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @param layerId A layer name
#' @param pageLength The maximum number of annotations to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @return A named list of annotations, with members:
#' \itemize{
#'  \item{\emph{id} The annotation's unique ID}
#'  \item{\emph{layerId} The name of the layer it comes from}
#'  \item{\emph{label} The value of the annotation}
#'  \item{\emph{startId} The ID of the start anchor},
#'  \item{\emph{endId} The ID of the end anchor},
#'  \item{\emph{parentId} The ID of the parent annotation},
#'  \item{\emph{ordinal} The ordinal of the annotation among its peers},
#'  \item{\emph{confidence} A rating from 0-100 of the confidence of the label
#'  e.g. 10: default value, 50: automatically generated, 100: manually annotated}
#' }
#' 
#' @seealso 
#'   \code{\link{getGraphIds}}
#'   \code{\link{getGraphIdsInCorpus}}
#'   \code{\link{getGraphIdsWithParticipant}}
#'   \code{\link{countAnnotations}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all the orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' }
#'
#' @keywords graph transcript
#' 
getAnnotations <- function(labbcat.url, id, layerId, pageLength = NULL, pageNumber = NULL) {
    parameters <- list(id=id, layerId=layerId)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
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
    return(resp.json$model$result)
}
