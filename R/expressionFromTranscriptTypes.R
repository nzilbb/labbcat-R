#' Generates a transcript query expression for matching transcripts by type, for use with
#' \link{getMatches} or \link{getMatchingTranscriptIds}.
#'
#' This function generates a transcript query expression fragment which can be passed as
#' the transcript.expression parameter of \link{getMatches}, (or the expression parameter
#' of \link{getMatchingTranscriptIds}) in order to identify transcripts using a list of
#' transcript types.
#'
#' @param transcript.types A list of transcript types. 
#' @param not Whether to match the given IDs (FALSE), or everything *except* the
#' given IDs.
#' @return A transcript query expression which can be passed as the
#' transcript.expression parameter of \link{getMatches} or the expression parameter
#' of \link{getMatchingTranscriptIds}
#' 
#' @seealso \link{expressionFromAttributeValue}
#' @seealso \link{expressionFromAttributeValues}
#' @seealso \link{expressionFromIds}
#' @seealso \link{getMatches}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search of interviews or monologues
#' transcript.types <- c("interview","monologue")
#' results <- getMatches(labbcat.url, list(segment="I"),
#'   transcript.expression = expressionFromTranscriptTypes(transcript.types))
#' 
#' ## Perform a search of all transcripts that aren't word-lists.
#' results <- getMatches(labbcat.url, list(segment="I"),
#'   transcript.expression = expressionFromTranscriptTypes("wordlist", NOT=true))
#' }
#' @keywords search
#' 
expressionFromTranscriptTypes <- function(transcript.types, not=FALSE) {
    return(expressionFromAttributeValue("transcript_type", transcript.types, not))
}
