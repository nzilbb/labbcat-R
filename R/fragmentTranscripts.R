#' Gets transcript fragments in a given format.
#'
#' This is a version of [getFragments] that can have a
#' dataframe of matches piped into it.
#'
#' It gets fragments of transcripts from LaBB-CAT, 
#' converted to a given file format (by default, Praat TextGrid).
#'
#' **NB** Although many formats will generate exactly one file for each interval
#'   (e.g. mime.type=text/praat-textgrid), this is not guaranteed; some formats generate
#'   a single file or a fixed collection of files regardless of how many fragments there are.
#' 
#' @param matches A dataframe returned by [getMatches] or [getAllUtterances], identifying
#' the results to which acoustic measurments should be appended. 
#' @param layer.ids A vector of layer IDs.
#' @param mime.type Optional content-type - "text/praat-textgrid" is the default, but your
#'   LaBB-CAT installation may support other formats, which can be discovered using
#'   [getSerializerDescriptors].
#' @param path Optional path to directory where the files should be saved.
#' @param start.column The column of `matches` containing the start time in seconds.
#' @param end.column The column of `matches` containing the end time in seconds.
#' @param labbcat.url URL to the LaBB-CAT instance (instead of inferring it from `matches`).
#' @return `matches` with the acoustic measurements appended as new columns.
#' 
#' @family Praat-related functions
#' @seealso
#'   - [processWithPraat]
#'   - [getMatches]
#' @examples
#' \dontrun{
#' ## Get all tokens of "the"
#' the.tokens <- getMatches(labbcat.url, "the")
#' ## Get a TextGrid for each matched utterance, including word and segment intervals
#' the.textgrids <- the.tokens |> fragmentTranscripts(c("utterance", "word", "segment"))
#' ## Get a CSV for the same utterances
#' the.textgrids <- the.tokens |> fragmentTranscripts(
#'     c("utterance", "word", "segment"), mime.type = "text/csv", path="csv")
#' }
#' @keywords praat
#' 
fragmentTranscripts <- function(matches, layer.ids,
                           mime.type = "text/praat-textgrid", path="",
                           start.column=Line, end.column=LineEnd,
                           labbcat.url=NULL) {
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    return(getFragments(
        labbcat.url, matches$Transcript,
        matches[[deparse(substitute(start.column))]],
        matches[[deparse(substitute(end.column))]],
        layer.ids, mime.type, path))
}
