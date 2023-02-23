#' Generates a query expression for matching a transcript/participant attribute, for use with
#' \link{getMatches}.
#'
#' This function generates a query expression fragment which can be passed as
#' the transcript.expression or participant.expression parameter of \link{getMatches}, (or
#' the expression parameter of \link{getMatchingTranscriptIds} or
#' \link{getMatchingParticipantIds}) using a list of possible values for a given
#' transcript attribute. 
#'
#' The attribute defined by transcript.attribute is expected to have exactly one value. If
#' it may have multiple values, use \link{expressionFromAttributeValues} instead.
#'
#' @param transcript.attribute The transcript attribute to filter by. 
#' @param values A list of possible values for transcript.attribute. 
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
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' languages <- c("en","en-NZ")
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       transcript.expression = expressionFromAttributeValue(
#'                             "transcript_language", languages))
#' }
#' @keywords search
#' 
expressionFromAttributeValue <- function(transcript.attribute, values) {
    escapedValues <- gsub("'","\\\\'", values)
    quotedValues  <- sapply(escapedValues, function(id) paste("'",id,"'", sep=""))
    valuesList     <- paste(quotedValues, collapse=",")
    escapedAttribute <- gsub("'","\\\\'", transcript.attribute)
    if (length(values) == 1) {
        return(paste("first('",escapedAttribute,"').label == ", valuesList, sep=""))
    } else {
        return(paste("[",valuesList,"].includes(first('",escapedAttribute,"').label)", sep=""))
    }
}
