#' Generates a query expression for matching transcripts or participants by ID, for use with
#' \link{getMatches}.
#'
#' This function generates a query expression fragment which can be passed as
#' the transcript.expression or participant.expression parameter of \link{getMatches},
#' using a list of corresponding IDs. 
#'
#' @param transcript.ids A list of transcript IDs. 
#' @return A transcript query expression which can be passed as the
#' transcript.expression parameter of \link{getMatches} or the expression parameter
#' of \link{getMatchingTranscriptIds}
#' 
#' @seealso \link{expressionFromAttributeValue}
#' @seealso \link{expressionFromAttributeValues}
#' @seealso \link{expressionFromTranscriptTypes}
#' @seealso \link{getMatches}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' transcript.ids <- c("AP511_MikeThorpe.eaf", "BR2044_OllyOhlson.eaf")
#' results <- getMatches(labbcat.url, list(segment="I"),
#'                       transcript.expression = expressionFromIds(transcript.ids))
#' }
#' @keywords search
#' 
expressionFromIds <- function(transcript.ids) {
    escapedIds <- gsub("'","\\\\'", transcript.ids)
    quotedIds  <- sapply(escapedIds, function(id) paste("'",id,"'", sep=""))
    idList     <- paste(quotedIds, collapse=",")
    expression <- paste("[",idList,"].includes(id)", sep="")
    return(expression)
}
