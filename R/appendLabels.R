#' Appends labels of annotations on given layers to a dataframe of matches.
#'
#' This is a version of [getMatchLabels] that can have a dataframe of matches piped into
#' it, and returns the dataframe with columns appended.
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
#' @param page.length In order to prevent timeouts when there are a large number of
#'   matches or the network connection is slow, rather than retrieving matches in one
#'   big request, they are retrieved using many smaller requests. This parameter
#'   controls the number of results retrieved per request.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @param labbcat.url URL to the LaBB-CAT instance (instead of inferring it from `matches`).
#' @param column.prefix A string to prefix each new column name with.
#' @return `matches` with the labels appended as new columns.
#' 
#' @seealso
#'   - [getMatchLabels]
#'   - [getMatches]
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(orthography="quake")) |>    
#'    appendLabels("topic") ## Get the topic annotations for the matches
#' }
#' 
#' @keywords layer annotation label
#' 
appendLabels <- function(matches, layer.ids,
                         target.offset=0, annotations.per.layer=1,
                         page.length=1000, no.progress=FALSE,
                         labbcat.url = NULL,  column.prefix=NULL) {
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    newColumns <- getMatchLabels(
        labbcat.url, matches$MatchId, layer.ids, target.offset, annotations.per.layer,
        include.match.ids = FALSE, page.length, no.progress)
    if (!is.null(column.prefix)) {
        names(newColumns) = paste0(column.prefix, names(newColumns))
    }

    matches <- cbind(matches, newColumns)
    attr(matches, "labbcat.url") <- labbcat.url
    return(matches)
}
