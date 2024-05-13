#' Deprecated synonym for getTranscriptIds.
#'
#' Returns a list of graph IDs (i.e. transcript names).
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @return A list of graph IDs
#' 
#' @seealso \code{\link{getTranscriptIds}}
#' @examples 
#' \dontrun{
#' ## List all transcripts
#' transcripts <- getGraphIds("https://labbcat.canterbury.ac.nz/demo/")
#' }
#' 
#' @keywords transcript
#' 
getGraphIds <- function(labbcat.url) {
    .Deprecated("getTranscriptIds")
    return(getTranscriptIds(labbcat.url))
}
