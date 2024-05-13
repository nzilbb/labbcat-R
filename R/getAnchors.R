#' Gets the given anchors in the given transcript
#'
#' Lists the given anchors in the given transcript.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A transcript ID (i.e. transcript name)
#' @param anchor.id A vector of anchor IDs (or a string representing one anchor ID)
#' @param page.length In order to prevent timeouts when there are a large number of
#'     matches or the network connection is slow, rather than retrieving anchors in one
#'     big request, they are retrieved using many smaller requests. This parameter
#'     controls the number of anchors retrieved per request.
#' @return  A named list of anchors, with members:
#'  - *id* The annotation's unique ID,
#'  - *offset* The offset from the beginning (in seconds if it's a
#'  transcript of a recording, or in characters if it's a text document)
#'  - *confidence* A rating from 0-100 of the confidence of the offset,
#'   e.g. 10: default value, 50: force-aligned, 100: manually aligned
#' 
#' @seealso \link{getAnnotations}
#' @examples 
#' \dontrun{
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' 
#' ## Get the start anchors for the above tokens
#' word.starts <- getAnchors(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
#' }
#' 
#' @keywords anchor
#' 
getAnchors <- function(labbcat.url, id, anchor.id, page.length=1000) {
    anchorIdChunks <- split(anchor.id, ceiling(seq_along(anchor.id)/page.length))
    result <- NULL
    for (anchorId in anchorIdChunks) {
        parameters <- list(id=id)
        for (id in anchorId) parameters <- append(parameters, list(anchorIds=id))
        resp <- store.get(labbcat.url, "getAnchors", parameters)
        if (is.null(resp)) return()
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
            print(resp.content)
            return()
        }
        resp.json <- jsonlite::fromJSON(resp.content)
        for (error in resp.json$errors) print(error)
        if (is.null(result)) {
            result <- resp.json$model
        } else {
            result <- rbind(result, resp.json$model)
        }
    }
    return(result)
}
