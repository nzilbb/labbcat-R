#' Appends measurements from Praat to a dataframe of matches.
#'
#' This is a version of [processWithPraat] that can have a dataframe of matches piped into
#' it, and returns the dataframe with columns appended.
#'
#' It instructs the LaBB-CAT server to invoke Praat for a set of sound
#' intervals, in order to extract acoustic measures.
#'
#' The exact measurements to return depend on the praat.script that is invoked. This is a
#' Praat script fragment that will run once for each sound interval specified.
#'
#' There are functions to allow the generation of a number of pre-defined praat scripts
#' for common tasks such as formant, pitch, intensity, and centre of gravity -- see
#' [praatScriptFormants], [praatScriptCentreOfGravity], [praatScriptIntensity]
#' and [praatScriptPitch].
#'
#' You can provide your own script, either by building a string with your code, or loading
#' one from a file.
#'
#' LaBB-CAT prefixes praat.script with code to open a sound file and extract a defined part
#' of it into a Sound object which is then selected.
#' 
#' LaBB-CAT `Remove's this Sound object after the script finishes executing. Any other objects
#' created by the script must be `Remove'd before the end of the script (otherwise Praat runs out
#' of memory during very large batches)
#'
#' LaBB-CAT assumes that all calls to the function 'print' correspond to fields for export
#' and each field must be printed on its own line. Specifically it scans for lines of the
#' form:
#'
#' print 'myOutputVariable' 'newline$'
#' 
#' Variables that can be assumed to be already set in the context of the script are:
#'  - *windowOffset*
#'    -- the value used for the Window Offset; how much context to include.
#'  - *windowAbsoluteStart*
#'    -- the start time of the window extracted relative to the start of the original audio file.
#'  - *windowAbsoluteEnd*
#'    -- the end time of the window extracted relative to the start of the original audio file.
#'  - *windowDuration*
#'    -- the duration of the window extracted (including window offset).
#'  - *targetAbsoluteStart*
#'    -- the start time of the target interval relative to the start of the original audio file.
#'  - *targetAbsoluteEnd*
#'    -- the end time of the target interval relative to the start of the original audio file.
#'  - *targetStart*
#'    -- the start time of the target interval relative to the start of the window extracted.
#'  - *targetEnd*
#'    -- the end time of the target interval relative to the start of the window extracted.
#'  - *targetDuration*
#'    -- the duration of the target interval.
#'  - *sampleNumber*
#'    -- the number of the sample within the set of samples being processed.
#'  - *sampleName$*
#'    -- the name of the extracted/selected Sound object.
#'
#' @param matches A dataframe returned by [getMatches] or [getAllUtterances], identifying
#' the results to which acoustic measurments should be appended. 
#' @param start.column The column of `matches` containing the start time in seconds.
#' @param end.column The column of `matches` containing the end time in seconds.
#' @param praat.script Script to run on each match. This may be a single string or a
#'   character vector.
#' @param window.offset In many circumstances, you will want some context before and after
#'   the sample start/end time.  For this reason, you can specify a "window offset" -
#'   this is a number of seconds to subtract from the sample start and add to the sample
#'   end time, before extracting that part of the audio for processing. For example, if
#'   the sample starts at 2.0s and ends at 3.0s, and you set the window offset to 0.5s,
#'   then Praat will extract a sample of audio from  1.5s to 3.5s, and do the selected
#'   processing on that sample. The best value for this depends on what the praat.script
#'   is doing; if you are getting formants from  vowels, including some context ensures
#'   that the formants at the edges are more accurate (in LaBB-CAT's web interface, the
#'   default value for this 0.025), but if you're getting max pitch or COG during a
#'   segment, most likely you want a window.offset of 0 to ensure neighbouring segments
#'   don't influence the measurement. 
#' @param gender.attribute Which participant attribute represents the participant's gender.
#' @param attributes Vector of participant attributes to make available to the script. For
#'   example, if you want to use different acoustic parameters depending on what the
#'   gender of the speaker is, including the "participant_gender" attribute will make a
#'   variable called participant_gender$ available to the praat script, whose value will
#'   be the gender of the speaker of that segment.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
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
#' results <- getMatches(labbcat.url, list(segment="I")) |>
#'     appendFromPraat( ## get F1 and F2 for the mid point of the vowel
#'        Target.segment.start, Target.segment.end, # for the vowel
#'        praatScriptFormants(),
#'        window.offset=0.5) ## get F1 and F2
#' 
#' ## Get all tokens of /i:/
#' results <- getMatches(labbcat.url, list(segment="i")) |>
#'    appendFromPraat(
#'        Target.segment.start, Target.segment.end, ## for the target vowel...
#'        paste(
#'            ## ... get first 3 formants at three points during the sample ...
#'            praatScriptFormants(c(1,2,3), c(0.25,0.5,0.75)),
#'            ## ... the mean, min, and max pitch ...
#'            praatScriptPitch(get.mean=TRUE, get.minimum=TRUE, get.maximum=TRUE),
#'            ## ... the max intensity ...
#'            praatScriptIntensity(),
#'            ## ... and the CoG using powers 1 and 2 
#'            praatScriptCentreOfGravity(powers=c(1.0,2.0))),
#'        window.offset=0.5)
#' 
#' ## Get all tokens of /s/
#' results <- getMatches(labbcat.url, list(segment="s")) |>
#'    appendFromPraat(
#'        Target.segment.start, Target.segment.end,
#'        readLines("acousticMeasurements.praat")) ## execute a custom script loaded form a file
#' }
#' @keywords praat
#' 
appendFromPraat <- function(matches, start.column, end.column,
                            praat.script, window.offset,
                            gender.attribute="participant_gender", attributes=NULL,
                            no.progress=FALSE,
                            labbcat.url=NULL, column.prefix=NULL) {
    labbcat.url <- determineLabbcatUrl(labbcat.url, matches)
    if (is.null(labbcat.url)) {
        stop("labbcat.url is NULL and could not be inferred", call.=T)
    }
    newColumns <- processWithPraat(
        labbcat.url, matches$MatchId,
        matches[[deparse(substitute(start.column))]],
        matches[[deparse(substitute(end.column))]],
        praat.script, window.offset, gender.attribute, attributes, no.progress)
    if (!is.null(column.prefix)) {
        names(newColumns) = paste0(column.prefix, names(newColumns))
    }
    matches <- cbind(matches, newColumns)
    attr(matches, "labbcat.url") <- labbcat.url
    return(matches)
}
