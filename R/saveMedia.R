#' Uploads the given media for the given transcript.
#'
#' This function upload a media file to LaBB-CAT, associating it with a given transcript.
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The transcript ID.
#' @param media The path to the media to upload.
#' @param track.suffix The track suffix for the media, if any.
#' @return A named list describing the attributes of the uploaded media:
#'  - *trackSuffix* The track suffix of the media
#'  - *mimeType* The MIME type of the file
#'  - *url* URL to the content of the file
#'  - *name* Name of the file in LaBB-CAT
#' @seealso \link{getAvailableMedia}
#' @seealso \link{deleteMedia}
#' 
#' @examples
#' \dontrun{
#' 
#' ## upload transcript
#' saveMedia(
#'     labbcat.url, "my-transcript.eaf", "my-transcript/audio/room-mic.wav", "-room")
#' }
#' @keywords media, upload
#' 
saveMedia <- function(labbcat.url, id, media, track.suffix=NULL) {
  
  ## upload file
  parameters <- list(
    id=id,
    trackSuffix=track.suffix,
    media=httr::upload_file(media))
  resp <- http.post.multipart(labbcat.url, "api/edit/store/saveMedia", parameters)
    
  ## check response
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
