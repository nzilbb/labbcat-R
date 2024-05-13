#' Generates a script for extracting formants, for use with [processWithPraat]
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of [processWithPraat], in order to extract selected formants.
#'
#' The [praatScriptFastTrack] function provides an alternative to this function which
#' uses the FastTrack Praat plugin for formant analysis.
#'
#' @param formants A vector of integers specifying which formants to extract, e.g c(1,2)
#'   for the first and second formant.
#' @param sample.points A vector of numbers (0 <= sample.points <= 1) specifying multiple
#'   points at which to take the measurement.  The default is a single point at 0.5 -
#'   this means one measurement will be taken halfway through the target interval.  If,
#'   for example, you wanted eleven measurements evenly spaced throughout the interval,
#'   you would specify sample.points as being
#'   c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0).  
#' @param time.step Time step in seconds, or 0.0 for 'auto'.
#' @param max.number.formants Maximum number of formants.
#' @param max.formant Maximum formant value (Hz) for all speakers, or for female speakers,
#'   if max.formant.male is also specified.
#' @param max.formant.male Maximum formant value (Hz) for male speakers, or NULL to use
#'   the same value as max.formant.
#' @param gender.attribute Name of the LaBB-CAT participant attribute that contains the
#'   participant's gender - normally this is "participant_gender".
#' @param value.for.male The value that the gender.attribute has when the participant is male.
#' @param window.length Window length in seconds.
#' @param preemphasis.from Pre-emphasis from (Hz)
#' @return A script fragment which can be passed as the praat.script parameter of
#'   [processWithPraat] 
#' 
#' @family Praat-related functions
#' @examples
#' \dontrun{
#' ## Get all tokens of the KIT vowel
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get the first 3 formants at three points during the vowel
#' formants <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               window.offset=0.025,
#'               praatScriptFormants(formants=c(1,2,3),
#'               sample.points=c(0.25,0.5,0.75)))
#' }
#' @keywords praat
#' 
praatScriptFormants <- function(formants = c(1,2), sample.points = c(0.5), time.step = 0.0, max.number.formants = 5, max.formant = 5500, max.formant.male = 5000, gender.attribute = 'participant_gender', value.for.male = "M", window.length = 0.025, preemphasis.from = 50) {
    script <- paste("\nmaxformant =", max.formant) 
    if (!is.null(max.formant.male) && !is.null(gender.attribute)
        && max.formant != max.formant.male) {
        ## differentiate between males and others
        script <- paste(
            script,
            "\nif ", gender.attribute, "$ = \"", value.for.male, "\"",
            "\n  maxformant = ", max.formant.male,
            "\nendif",
            sep="")
    }
    script <- paste( # ensure the sound sample is selected
        script, "\nselect Sound 'sampleName$'", sep="")
    script <- paste(
        script, "\nTo Formant (burg): ", time.step, ", ", max.number.formants, ", ",
        "maxformant, ", window.length, ", ", preemphasis.from, sep="")
    for (point in sample.points) {
        varname = paste("time_", stringr::str_replace(point, "\\.","_"), sep="")
        ## first output absolute point offset
        script <- paste(script, "\npointoffset =",
                       " targetAbsoluteStart + ", point, " * targetDuration", sep="")
        script <- paste(script, "\n", varname, " = pointoffset", sep="")
        script <- paste(script, "\nprint '", varname, "' 'newline$'", sep="")
        ## now use the relative point offset
        script <- paste(script, "\npointoffset =",
                       " targetStart + ", point, " * targetDuration", sep="")
        for (f in formants) {
            varname = paste("f", f, "_time_", stringr::str_replace(point, "\\.","_"), sep="")
            script <- paste(script, "\n", varname,
                           " = Get value at time: ", f, ", pointoffset, \"hertz\", \"Linear\"",
                           sep="")
            script <- paste(script, "\nprint '", varname, ":0' 'newline$'", sep="")
        } ## next formant
    } ## next sample point
    ## remove formant object
    script <- paste(script, "\nRemove\n", sep="")
    return(script)
}
