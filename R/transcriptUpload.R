#' Upload a transcript file and associated media files.
#'
#' Uploading files is the first stage in adding or modifying a transcript to LaBB-CAT.
#' The second stage is transcriptUploadParameters()
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcript The path to the transcript to upload.
#' @param media The path to the media to upload, if any.
#' @param merge Whether the upload corresponds to updates to an existing transcript
#'              (TRUE) or a new transcript (FALSE).
#' @return A named list with members:
#'   - *id* The unique identifier to use for this upload when subsequently calling
#'          transcriptUploadParameters()
#'   - *parameters* A list of named lists representing the parameters that require values
#'          to be passed into transcriptUploadParameters().
#'          The parameters returned may include both information required by the format
#'          deserializer (e.g. mappings from tiers to LaBB-CAT layers) and also general
#'          information required by LaBB-CAT (e.g. the corpus, episode, and type of the
#'          transcript).
#'          Each parameter returned is a dict that may contain the following attributes:
#'            - "name" - The name that should be used when specifying the value for the
#'               parameter when calling transcriptUploadParameters()
#'            - "label" - A label for the parameter intended for display to the user.
#'            - "hint" - A description of the purpose of the parameter, for display to the user.
#'            - "type" - The type of the parameter, e.g. "String", "Double", "Integer", "Boolean".
#'            - "required" - True if the value must be specified, False if it is optional.
#'            - "value" - A default value for the parameter.
#'            - "possibleValues" - A list of possible values, if the possibilities are limited
#'               to a finite set.
#'          The required parameters may include both information required by the format
#'          deserializer  (e.g. mappings from tiers to LaBB-CAT layers) and also general
#'          information required by LaBB-CAT, such as: 
#'            - "labbcat_corpus" - The corpus the new transcript(s) belong(s) to.
#'            - "labbcat_episode" - The episode the new transcript(s) belong(s) to.
#'            - "labbcat_transcript_type" - The transcript type for the new transcript(s).
#'            - "labbcat_generate" - Whether to re-regenerate automated annotation layers or not.
#' @seealso
#' - [transcriptUploadParameters]
#' - [transcriptUploadDelete]
#' - [newTranscript]
#' - [updateTranscript]
#' @examples
#' \dontrun{
#' ## Get attributes for new transcript
#' corpus <- getCorpusIds(labbcat.url)[1]
#' transcript.type.layer <- getLayer(labbcat.url, "transcript_type")
#' transcript.type <- transcript.type.layer$validLabels[[1]]
#' 
#' ## upload transcript and its media
#' result <- transcriptUpload(labbcat.url, "my-transcript.eaf", "my-transcript.wav", FALSE)
#' 
#' ## use the default parameter values
#' parameterValues <- list()
#' for(p in 1:length(parameters$name)) parameterValues[parameters$name[p]] <- parameters$value[p]
#'
#' ## set the upload parameters to finalise the upload
#' transcript.id <- transcriptUploadParameters(labbcat.url, result$id, parameterValues)
#' }
#' @keywords transcript management
#' 
transcriptUpload <- function(labbcat.url, transcript, media=NULL, merge=FALSE) {
    
    ## upload file(s)
    parameters <- list(
        transcript=httr::upload_file(transcript))
    if (merge) {
        parameters$merge = TRUE
        }
    if (!is.null(media)) {
        parameters$media <- httr::upload_file(media) # TODO media.suffix
    }
    resp <- http.post.multipart(labbcat.url, "api/edit/transcript/upload", parameters)
    
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
    if (length(resp.json$errors)) return()
    
    return(resp.json$model)
}
