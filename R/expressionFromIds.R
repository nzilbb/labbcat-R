#' Generates a query expression for matching transcripts or participants by ID, for use with
#' \link{getMatches}.
#'
#' This function generates a query expression fragment which can be passed as
#' the transcript.expression or participant.expression parameter of \link{getMatches},
#' using a list of corresponding IDs. 
#'
#' @param ids A list of IDs. 
#' @param not Whether to match the given IDs (FALSE), or everything *except* the
#' given IDs.  
#' @return A query expression which can be passed as the
#' transcript.expression or participant.expression parameter of \link{getMatches} 
#' or the expression parameter of \link{getMatchingTranscriptIds} or
#' \link{getMatchingParticipantIds} 
#' 
#' @seealso \link{expressionFromAttributeValue}
#' @seealso \link{expressionFromAttributeValues}
#' @seealso \link{expressionFromTranscriptTypes}
#' @seealso \link{getMatches}
#' @examples
#' \dontrun{
#' ## Perform a search
#' transcript.ids <- c("AP511_MikeThorpe.eaf", "BR2044_OllyOhlson.eaf")
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       transcript.expression = expressionFromIds(transcript.ids))
#' }
#' @keywords search
#' 
expressionFromIds <- function(ids, not=FALSE) {
    escapedIds <- gsub("'","\\\\'", ids)
    quotedIds  <- sapply(escapedIds, function(id) paste("'",id,"'", sep=""))
    idList     <- paste(quotedIds, collapse=",")
    if (not) {
        prefix <- "!"
    } else {
        prefix <- ""
    }
    expression <- paste(prefix, "[",idList,"].includes(id)", sep="")
    return(expression)
}
