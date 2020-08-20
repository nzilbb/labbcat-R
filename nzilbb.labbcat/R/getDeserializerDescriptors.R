#' Lists the descriptors of all registered deserializers.
#' 
#' Returns a list of deserializers, which are modules that import transcriptions and
#' annotation structures from a specific file format, e.g. Praat TextGrid, plain text, etc.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of serializers, each including the following information:
#' \itemize{
#'  \item{\emph{name} The name of the format.}
#'  \item{\emph{version} The installed version of the serializer module.}
#'  \item{\emph{fileSuffixes} The normal file name suffixes (extensions) of the files.},
#'  \item{\emph{mimeType} The MIME type of the format, i.e. the value to use as the
#'     \emph{mimeType} parameter of \link{getFragments}},
#' }
#' 
#' @examples
#' \dontrun{
#' ## List file upload formats supported
#' formats <- getDeserializerDescriptors("https://labbcat.canterbury.ac.nz/demo/")
#' 
#' ## can we upload as plain text?
#' plainTextSupported <- "text/plain" %in% formats$mimeType
#' }
#' 
#' @keywords corpora
#' 
getDeserializerDescriptors <- function(labbcat.url) {
    resp <- store.get(labbcat.url, "getDeserializerDescriptors")
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
