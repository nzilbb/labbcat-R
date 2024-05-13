#' Generates a query expression for matching a transcript/participant attribute, for use with
#' [getMatches]
#'
#' This function generates a query expression fragment which can be passed as
#' the transcript.expression or participant.expression parameter of [getMatches], (or
#' the expression parameter of [getMatchingTranscriptIds] or
#' [getMatchingParticipantIds]) matching by the number of values for a given
#' attribute. 
#'
#' The attribute defined by transcript.attribute is expected to have possibly more than
#' one value, although single-value attributes may have 0 or 1 values, and this function
#' can be used to distinguish these two possibilities as well.
#'
#' @param transcript.attribute The transcript attribute to filter by. 
#' @param comparison A string representing the operator to use for comparison, one of
#' "<", "<=", "==", "!=", ">=", ">". 
#' @param count The number to compare the count of values to.
#' @return A transcript query expression which can be passed as the
#' transcript.expression parameter of [getMatches] or the expression parameter
#' of [getMatchingTranscriptIds]
#' 
#' @seealso [expressionFromAttributeValues]
#' @seealso [expressionFromTranscriptTypes]
#' @seealso [expressionFromIds]
#' @seealso [getMatches]
#' @examples
#' \dontrun{
#' ## Search only transcripts including multilingual participants
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       participant.expression = expressionFromAttributeValuesCount(
#'                             "participant_languages", ">=", 2))
#'
#' ## Search only transcripts with no restrictions specified
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       transcript.expression = expressionFromAttributeValuesCount(
#'                             "transcript_restrictions", "==", 0))
#' }
#' @keywords search
#' 
expressionFromAttributeValuesCount <- function(transcript.attribute, comparison = "==", count) {
    escapedAttribute <- gsub("'","\\\\'", transcript.attribute)
    return(paste("labels('",escapedAttribute,"').length", " ", comparison, " ", count,
                     sep=""))
}
