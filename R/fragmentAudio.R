#' Gets sound fragments from 'LaBB-CAT'.
#'
#' This is a version of [getSoundFragment] that can have a
#' dataframe of matches piped into it.
#'
#' It gets fragments of audio from LaBB-CAT, as wav files.
#' 
#' @param matches A dataframe returned by [getMatches] or [getAllUtterances], identifying
#' the results to which acoustic measurments should be appended. 
#' @param sample.rate Optional sample rate in Hz - if a positive
#'   integer, then the result is a mono file with the given sample rate.
#' @param path Optional path to directory where the files should be saved.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @param start.column The column of `matches` containing the start time in seconds.
#' @param end.column The column of `matches` containing the end time in seconds.
#' @param labbcat.url URL to the LaBB-CAT instance (instead of inferring it from `matches`).
#' @return `matches` with the acoustic measurements appended as new columns.
#' 
#' @seealso
#'   - [processWithPraat]
#'   - [getMatches]
#' @examples
#' \dontrun{
#' ## Get all tokens of "the"
#' the.tokens <- getMatches(labbcat.url, "the")
#' ## Get a 22kHz sample rate wav file for each matched utterance
#' the.wavs <- the.tokens |> fragmentTranscripts(sample.rate = 22050)
#' }
#' @keywords praat
#' 
fragmentAudio <- function(matches, 
                          sample.rate = NULL, path="", no.progress=FALSE,
                          start.column=Line, end.column=LineEnd,
                          labbcat.url=NULL) {
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    return(getSoundFragments(
        labbcat.url, matches$Transcript,
        matches[[deparse(substitute(start.column))]],
        matches[[deparse(substitute(end.column))]],
        sample.rate, path, no.progress))
}
