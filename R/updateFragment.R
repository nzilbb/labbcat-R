#' Update a transcript fragment.
#'
#' This function uploads a file (e.g. Praat TextGrid) representing a fragment of a
#' transcript, with annotations or alignments to update in LaBB-CAT's version of the
#' transcript. 
#'
#' For this function to work, the credentials used to connect to the server must have at
#' least 'edit' access.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param fragment.path The path to the transcript to upload.
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'     shown when interactive().
#' @return A named list with information about the fragment that was updated.
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## upload new verison of transcript transcript
#' updateFragment(labbcat.url, "my-transcript__1.234-5.678.TextGrid")
#' }
#' @keywords transcript management
#' 
updateFragment <- function(labbcat.url, fragment.path) {
    
    ## upload file(s)
    parameters <- list(
        todo="upload",
        automaticMapping="true",
        uploadfile=httr::upload_file(fragment.path))
    resp <- http.post.multipart(labbcat.url, "edit/uploadFragment", parameters)
    
    ## check response
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", fragment.path, httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    
    return(resp.json$model)
}
