#' Appends temporal alignments on given layers to a dataframe of matches.
#'
#' Appends labels and start/end offsets of annotations on a given layer, to a given
#' dataframe of matches returned from [getMatches] or [getAllUtterances].
#'
#' This is a version of [getMatchAnnotations] that can have a dataframe of matches piped into
#' it, and returns the dataframe with columns appended.
#' 
#' You can specify a threshold for confidence in the alignment, which is a value from 0
#' (not aligned) to 100 (manually aligned). The default is 50 (automatically aligned), so
#' only alignments that have been at least automatically aligned are specified. For cases
#' where there's a token but its alignment confidence falls below the threshold, a label
#' is returned, but the start/end times are NA.
#'
#' @param matches A dataframe returned by [getMatches] or [getAllUtterances], identifying
#' the results to which annotation labels should be appended. 
#' @param layer.ids A vector of layer IDs.
#' @param target.offset The distance from the original target of the match, e.g.
#'   - *0* -- find annotations of the match target itself
#'   - *1* -- find annotations of the token immediately *after* match target
#'   - *-1* -- find annotations of the token immediately *before* match target
#' @param annotations.per.layer The number of annotations on the given layer to
#'   retrieve. In most cases, there's only one annotation available. However, tokens
#'   may, for example, be annotated with `all possible phonemic transcriptions', in which
#'   case using a value of greater than 1 for this parameter provides other phonemic
#'   transcriptions, for tokens that have more than one.
#' @param anchor.confidence.min The minimum confidence for alignments, e.g.
#'   - *0* -- return all alignments, regardless of confidence;
#'   - *50* -- return only alignments that have been at least automatically aligned;
#'   - *100* -- return only manually-set alignments.
#' @param page.length In order to prevent timeouts when there are a large number of
#'   matches or the network connection is slow, rather than retrieving matches in one
#'   big request, they are retrieved using many smaller requests. This parameter
#'   controls the number of results retrieved per request.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @param labbcat.url URL to the LaBB-CAT instance (instead of inferring it from `matches`).
#' @param column.prefix A string to prefix each new column name with.
#' @return `matches` with the labels, start times, and end times appended as new columns.
#' 
#' @seealso
#'   - [getMatches]
#'   - [getMatchAlignments]
#' @examples
#' \dontrun{
#' ## Get all tokens of /I/
#' results <- getMatches(labbcat.url, list(segment="I")) |>
#'     appendOffsets("segment", target.offset=1, ## Get the segment following the token
#'         anchor.confidence.min=100) ## with alignment if it's been manually aligned
#' }
#' 
#' @keywords layer annotation label
#' 
appendOffsets <- function(matches, layer.ids, target.offset=0,
                          annotations.per.layer=1, anchor.confidence.min=50,
                          page.length=1000, no.progress=FALSE,
                          labbcat.url=NULL, column.prefix=NULL) {    
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    newColumns <- getMatchAlignments(
        labbcat.url, matches$MatchId, layer.ids, target.offset,
        annotations.per.layer, anchor.confidence.min,
        include.match.ids=FALSE, page.length, no.progress)
    if (!is.null(column.prefix)) {
        names(newColumns) = paste0(column.prefix, names(newColumns))
    }
    matches <- cbind(matches, newColumns)
    attr(matches, "labbcat.url") <- labbcat.url
    return(matches)
}
