#' List the media available for the given transcript.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A transcript ID (i.e. transcript name)
#' @return A named list of media files available for the given transcript, with members:
#' \itemize{
#'  \item{\emph{trackSuffix} The track suffix of the media}
#'  \item{\emph{mimeType} The MIME type of the file}
#'  \item{\emph{url} URL to the content of the file}
#'  \item{\emph{name} Name of the file}
#' }
#' 
#' @seealso \link{getTranscriptIds}
#' @seealso \link{saveMedia}
#' @seealso \link{deleteMedia}
#' @examples 
#' \dontrun{
#' ## List the media files available for BR2044_OllyOhlson.eaf
#' media <- getAvailableMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
#' }
#' 
#' @keywords media audio
#' 
getAvailableMedia <- function(labbcat.url, id) {
    parameters <- list(id=id)
    resp <- store.get(labbcat.url, "getAvailableMedia", parameters)
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
