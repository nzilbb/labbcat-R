#' Gets annotations in fragments.
#'
#' This is a version of [getFragmentAnnotations] that can have a
#' dataframe of matches piped into it.
#'
#' It gets annotations between given start/end times on given layers. If more
#' than one annotation matches, labels are concatentated together.
#'
#' @param matches A dataframe returned by [getMatches] or [getAllUtterances], identifying
#' the results to which acoustic measurments should be appended. 
#' @param layer.ids A vector of layer IDs.
#' @param sep The separator to use when concatenating labels when multiple annotations are
#'   in the given interval.
#' @param partial.containment Whether to include annotations that are only partially
#'   contained in the given interval.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @param start.column The column of `matches` containing the start time in seconds.
#' @param end.column The column of `matches` containing the end time in seconds.
#' @param labbcat.url URL to the LaBB-CAT instance (instead of inferring it from `matches`).
#' @param column.prefix A string to prefix each new column name with.
#' @return `matches` with the acoustic measurements appended as new columns.
#' 
#' @family Praat-related functions
#' @seealso
#'   - [processWithPraat]
#'   - [getMatches]
#' @examples
#' \dontrun{
#' ## Get all tokens of /I/
#' results <- getMatches(labbcat.url, list(topic = ".*quake.*")) |>
#'     fragmentLabels( ## concatenate labels of words between topic.start and topic.end
#'        c("word"), start.column=topic.start, end.column=topic.end)
#' }
#' @keywords praat
#' 
fragmentLabels <- function(matches, 
                           layer.ids, sep=" ", partial.containment=FALSE,
                           no.progress=FALSE, 
                           start.column=Line, end.column=LineEnd,
                           labbcat.url=NULL, column.prefix=NULL) {
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    newColumns <- getFragmentAnnotations(
        labbcat.url, matches$Transcript, matches$Participant,
        matches[[deparse(substitute(start.column))]],
        matches[[deparse(substitute(end.column))]],
        layer.ids, sep, partial.containment, no.progress)
    if (!is.null(column.prefix)) {
        names(newColumns) = paste0(column.prefix, names(newColumns))
    }
    matches <- cbind(matches, newColumns)
    attr(matches, "labbcat.url") <- labbcat.url
    return(matches)
}
