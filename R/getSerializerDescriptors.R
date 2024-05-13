#' Lists the descriptors of all registered serializers
#' 
#' Returns a list of serializers, which are modules that export annotation structures as
#' a specific file format, e.g. Praat TextGrid, plain text, etc., so the
#' \emph{mimeType} of descriptors reflects what \emph{mimeType}s can be specified for
#' [getFragments]. 
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of serializers, each including the following information:
#'   - *name* The name of the format.
#'   - *version* The installed version of the serializer module.
#'   - *fileSuffixes* The normal file name suffixes (extensions) of the files.
#'   - *mimeType* The MIME type of the format, i.e. the value to use as the
#'     *mimeType* parameter of [getFragments]
#' 
#' @seealso [getFragments]
#' @examples
#' \dontrun{
#' ## List file export formats supported
#' formats <- getSerializerDescriptors("https://labbcat.canterbury.ac.nz/demo/")
#' 
#' ## can we export as plain text?
#' plainTextSupported <- "text/plain" %in% formats$mimeType
#' }
#' 
#' @keywords format
#' 
getSerializerDescriptors <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getSerializerDescriptors")
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
