#' Gets binary annotation data in fragments.
#'
#' This is a version of [getFragmentAnnotationData] that can have a
#' dataframe of matches piped into it.
#'
#' In some annotation layers, the annotations have not only a textual label, but also
#' binary data associated with it; e.g. an image or a data file. In these cases, the 'type'
#' of the layer is a MIME type, e.g. 'image/png'.
#' This function gets annotations between given start/end times on the given MIME-typed
#' layer, and retrieves the binary data as files, whose names are returned by the function.
#' 
#' @param matches A dataframe returned by [getMatches] or [getAllUtterances], identifying
#' the results to which acoustic measurments should be appended. 
#' @param layer.id The ID of the MIME-typed layer.
#' @param path Optional path to directory where the files should be saved.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @param start.column The column of `matches` containing the start time in seconds.
#' @param end.column The column of `matches` containing the end time in seconds.
#' @param labbcat.url URL to the LaBB-CAT instance (instead of inferring it from `matches`).
#' @return `matches` with the acoustic measurements appended as new columns.
#' 
#' @seealso
#'   - [processWithPraat]
#'   - [getMatches]
#' @examples
#' \dontrun{
#' ## Get all tokens of "vivid"
#' vivid.tokens <- getMatches(labbcat.url, "vivid")
#' ## Get mediapipe image annotations for during the tokens
#' vivid.faces <- vivid.tokens |>
#'     fragmentData(
#'         "mediapipe", path = "png",
#'         start.column=Target.word.start, end.column=Target.word.end)
#' }
#' @keywords praat
#' 
fragmentData <- function(matches, 
                         layer.id, path="", no.progress=FALSE,
                         start.column=Line, end.column=LineEnd,
                         labbcat.url=NULL) {
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    return(getFragmentAnnotationData(
        labbcat.url, matches$Transcript,
        matches[[deparse(substitute(start.column))]],
        matches[[deparse(substitute(end.column))]],
        layer.id, path, no.progress))
}
