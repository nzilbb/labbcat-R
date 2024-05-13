#' Generates a query expression for matching a single-value transcript/participant
#' attribute, for use with \link{getMatches}
#'
#' This function generates a query expression fragment which can be passed as
#' the transcript.expression or participant.expression parameter of \link{getMatches}, (or
#' the expression parameter of \link{getMatchingTranscriptIds} or
#' \link{getMatchingParticipantIds}) using a list of possible values for a given
#' attribute. 
#'
#' The attribute defined by transcript.attribute is expected to have exactly one value. If
#' it may have multiple values, use \link{expressionFromAttributeValues} instead.
#'
#' @param transcript.attribute The transcript attribute to filter by. 
#' @param values A list of possible values for transcript.attribute. 
#' @param not Whether to match the given IDs (FALSE), or everything *except* the
#' given IDs.
#' @return A transcript query expression which can be passed as the
#' transcript.expression parameter of \link{getMatches} or the expression parameter
#' of \link{getMatchingTranscriptIds}
#' 
#' @seealso \link{expressionFromAttributeValues}
#' @seealso \link{expressionFromTranscriptTypes}
#' @seealso \link{expressionFromIds}
#' @seealso \link{getMatches}
#' @examples
#' \dontrun{
#' ## Perform a search
#' languages <- c("en","en-NZ")
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       transcript.expression = expressionFromAttributeValue(
#'                             "transcript_language", languages))
#' }
#' @keywords search
#' 
expressionFromAttributeValue <- function(transcript.attribute, values, not=FALSE) {
    escapedValues <- gsub("'","\\\\'", values)
    quotedValues  <- sapply(escapedValues, function(id) paste("'",id,"'", sep=""))
    valuesList     <- paste(quotedValues, collapse=",")
    escapedAttribute <- gsub("'","\\\\'", transcript.attribute)
    if (not) {
        prefix <- "!"
        operator <- " <> "
    } else {
        prefix <- ""
        operator <- " == "
    }
    if (length(values) == 1) {
        return(paste("first('",escapedAttribute,"').label", operator, valuesList,
                     sep=""))
    } else {
        return(paste(prefix, "[",valuesList,"].includes(first('",escapedAttribute,"').label)",
                     sep=""))
    }
}
