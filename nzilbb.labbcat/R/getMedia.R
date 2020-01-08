#' Gets a given media track for a given graph.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A graph ID (i.e. transcript name)
#' @param trackSuffix The track suffix of the media
#' @param mimeType The MIME type of the media
#' @return A URL to the given media for the given graph
#' @seealso \link{getGraphIds}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get URL for the WAV file for BR2044_OllyOhlson.eaf
#' media <- getMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
#' 
#' ## Get URL for the 'QuakeFace' video file for BR2044_OllyOhlson.eaf
#' media <- getMedia(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4")
#' }
#' 
#' @keywords media audio
#' 
getMedia <- function(labbcat.url, id, trackSuffix = "", mimeType = "audio/wav") {
    parameters <- list(id=id, trackSuffix=trackSuffix, mimeType=mimeType)
    resp <- store.get(labbcat.url, "getMedia", parameters)
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
