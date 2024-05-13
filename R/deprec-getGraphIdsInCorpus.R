#' Deprecated synonym for getTranscriptIdsInCorpus.
#'
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id The ID (name) of the corpus
#' @return A list of corpus IDs
#' 
#' @seealso \code{\link{getGraphIdsInCorpus}}
#' @examples 
#' \dontrun{
#' ## List transcripts in the QB corpus
#' transcripts <- getGraphIdsInCorpus("https://labbcat.canterbury.ac.nz/demo/", "QB")
#' }
#' 
#' @keywords corpora corpus
#' 
getGraphIdsInCorpus <- function(labbcat.url, id) {
    .Deprecated("getTranscriptIdsInCorpus")
    return(getTranscriptIdsInCorpus(labbcat.url, id))
}
