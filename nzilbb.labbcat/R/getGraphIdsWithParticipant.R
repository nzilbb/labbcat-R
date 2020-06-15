#' Deprecated synonym for getTranscriptIdsWithParticipant.
#'
#' Returns a list of IDs of graphs (i.e. transcript names) that include
#' the given participant.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A participant ID
#' @return A list of graph IDs
#' 
#' @seealso \code{\link{getTranscriptIdsWithParticipant}}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## List transcripts in which UC427_ViktoriaPapp_A_ENG speaks
#' transcripts <- getGraphIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")
#' }
#' 
#' @keywords graph transcript
#' 
getGraphIdsWithParticipant <- function(labbcat.url, id) {
    .Deprecated("getTranscriptIdsWithParticipant")
    return(getTranscriptIdsWithParticipant(labbcat.url, id))
}
