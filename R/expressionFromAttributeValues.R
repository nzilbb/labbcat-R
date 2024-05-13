#' Generates a query expression for matching a multi-value transcript/participant
#' attribute, for use with \link{getMatches}
#'
#' This function generates a query expression fragment which can be passed as
#' the transcript.expression or participant.expression parameter of \link{getMatches}, (or
#' the expression parameter of \link{getMatchingTranscriptIds} or
#' \link{getMatchingParticipantIds}) using a list of possible values for a given
#' transcript attribute. 
#'
#' The attribute defined by transcript.attribute is expected to have possibly more than
#' one value. If it can have only one value, use \link{expressionFromAttributeValue} instead.
#'
#' @param transcript.attribute The transcript attribute to filter by. 
#' @param values A list of possible values for transcript.attribute. 
#' @param not Whether to match the given IDs (FALSE), or everything *except* the
#' given IDs.
#' @return A transcript query expression which can be passed as the
#' transcript.expression parameter of \link{getMatches} or the expression parameter
#' of \link{getMatchingTranscriptIds}
#' 
#' @seealso \link{expressionFromAttributeValue}
#' @seealso \link{expressionFromTranscriptTypes}
#' @seealso \link{expressionFromIds}
#' @seealso \link{getMatches}
#' @examples
#' \dontrun{
#' ## Perform a search
#' languages <- c("en","es")
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       participant.expression = expressionFromAttributeValues(
#'                             "participant_languagesSpoken", languages))
#' }
#' @keywords search
#' 
expressionFromAttributeValues <- function(transcript.attribute, values, not=FALSE) {
    escapedValues <- gsub("'","\\\\'", values)
    quotedValues  <- sapply(escapedValues, function(id)
        paste("'",id,"'", sep=""))
    valuesList     <- paste(quotedValues, collapse=",")
    escapedAttribute <- gsub("'","\\\\'", transcript.attribute)
    if (not) {
        prefix <- "!"
    } else {
        prefix <- ""
    }
    if (length(values) == 1) {
        return(paste(prefix, "labels('",escapedAttribute,"').includes(",valuesList,")",
                     sep=""))
    } else {
        return(paste(prefix, "[",valuesList,"].includesAny(labels('",escapedAttribute,"'))",
                     sep=""))
    }
}
