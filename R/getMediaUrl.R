#' Gets the URL of the given media track for a given transcript.
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param id A transcript ID (i.e. transcript name).
#' @param track.suffix The track suffix of the media.
#' @param mime.type The MIME type of the media, e.g. "audio/wav" or "application/f0".
#' @return A URL to the given media for the given transcript.
#' @seealso \link{getTranscriptIds}
#' @seealso \link{getMedia}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get URL for the WAV file for BR2044_OllyOhlson.eaf
#' wavUrl <- getMediaUrl(labbcat.url, "BR2044_OllyOhlson.eaf")
#' 
#' ## Get URL for the 'QuakeFace' video file for BR2044_OllyOhlson.eaf
#' quakeFaceMp4Url <- getMediaUrl(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4")
#' }
#' 
#' @keywords media audio
#' 
getMediaUrl <- function(labbcat.url, id, track.suffix = "", mime.type = "audio/wav") {
    parameters <- list(id=id, trackSuffix=track.suffix, mimeType=mime.type)
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
    return(resp.json$model)
}
