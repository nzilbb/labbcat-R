#' Generates a script for extracting pitch, for use with \link{processWithPraat}.
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of \link{processWithPraat}, in order to extract pitch information.
#'
#' @param get.mean Extract the mean pitch for the sample.
#' @param get.minimum Extract the minimum pitch for the sample.
#' @param get.maximum Extract the maximum pitch for the sample.
#' @param time.step Step setting for praat command
#' @param pitch.floor Minimum pitch (Hz) for all speakers, or for female speakers,
#'     if pitch.floor.male is also specified.
#' @param max.number.of.candidates Maximum number of candidates setting for praat command
#' @param very.accurate Accuracy setting for praat command
#' @param silence.threshold Silence threshold setting for praat command
#' @param voicing.threshold Voicing threshold (Hz) for all speakers, or for female speakers,
#'     if voicing.threshold.male is also specified.
#' @param octave.cost Octave cost setting for praat command
#' @param octave.jump.cost Octave jump cost setting for praat command
#' @param voiced.unvoiced.cost Voiced/unvoiced cost setting for praat command
#' @param pitch.ceiling Maximum pitch (Hz) for all speakers, or for female speakers,
#'     if pitch.floor.male is also specified.
#' @param pitch.floor.male Minimum pitch (Hz) for male speakers.
#' @param voicing.threshold.male Voicing threshold (Hz) for male speakers.
#' @param pitch.ceiling.male Maximum pitch (Hz) for male speakers.
#' @param gender.attribute Name of the LaBB-CAT participant attribute that contains the
#'     participant's gender - normally this is "participant_gender".
#' @param value.for.male The value that the gender.attribute has when the participant is male.
#' @param window.length Window length in seconds.
#' @param preemphasis.from Pre-emphasis from (Hz)
#' @return A script fragment which can be passed as the praat.script parameter of
#'     \link{processWithPraat} 
#' 
#' @seealso \link{processWithPraat}
#' @seealso \link{praatScriptFormants}
#' @seealso \link{praatScriptCentreOfGravity}
#' @seealso \link{praatScriptIntensity}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get pitch mean, max, and min for all matches
#' pitch <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               praatScriptPitch(get.mean=TRUE, get.minimum=TRUE, get.maximum=TRUE),
#'               no.progress=TRUE)
#' }
#' @keywords praat
#' 
praatScriptPitch <- function(get.mean = TRUE, get.minimum = FALSE, get.maximum = FALSE, time.step = 0.0, pitch.floor = 60, max.number.of.candidates = 15, very.accurate = FALSE, silence.threshold = 0.03, voicing.threshold = 0.5, octave.cost = 0.01, octave.jump.cost = 0.35, voiced.unvoiced.cost = 0.35, pitch.ceiling = 500, pitch.floor.male = 30, voicing.threshold.male = 0.4, pitch.ceiling.male = 250, gender.attribute = 'participant_gender', value.for.male = "M", window.length = 0.025, preemphasis.from = 50) {
    script <- paste(
        "\npitchfloor = ", pitch.floor,
        "\nvoicingthreshold = ", voicing.threshold,
        "\npitchceiling = ", pitch.ceiling, sep="")
    if (very.accurate) {
        script <- paste(script, "\nveryaccurate$ = \"yes\"", sep="") 
    } else {
        script <- paste(script, "\nveryaccurate$ = \"no\"", sep="") 
    }
    if(!is.null(gender.attribute)) {
        ## differentiate between males and others
        if (!is.null(pitch.floor.male) && pitch.floor != pitch.floor.male) {
            script <- paste(
                script,
                "\nif ", gender.attribute, "$ = \"", value.for.male, "\"",
                "\n  pitchfloor = ", pitch.floor.male,
                "\nendif",
                sep="")
        }
        if (!is.null(voicing.threshold.male) && voicing.threshold != voicing.threshold.male) {
            script <- paste(
                script,
                "\nif ", gender.attribute, "$ = \"", value.for.male, "\"",
                "\n  voicingthreshold = ", voicing.threshold.male,
                "\nendif",
                sep="")
        }
        if (!is.null(pitch.ceiling.male) && pitch.ceiling != pitch.ceiling.male) {
            script <- paste(
                script,
                "\nif ", gender.attribute, "$ = \"", value.for.male, "\"",
                "\n  pitchceiling = ", pitch.ceiling.male,
                "\nendif",
                sep="")
        }
    }
    script <- paste( # ensure the sound sample is selected
        script, "\nselect Sound 'sampleName$'", sep="")
    script <- paste(
        script, "\nTo Pitch (ac): ", time.step, ", pitchfloor, ", max.number.of.candidates, ", ",
        "veryaccurate$, ", silence.threshold, ", voicingthreshold, ", octave.cost, ", ",
        octave.jump.cost, ", ", voiced.unvoiced.cost, ", pitchceiling", sep="")
    if (get.mean) {
        script <- paste(script,
                       "\nmeanPitch = Get mean: targetStart, targetEnd, \"Hertz\"",
                       "\nprint 'meanPitch' 'newline$'", sep="")
    }
    if (get.minimum) {
        script <- paste(script,
                       "\nminPitch = Get minimum: targetStart, targetEnd, \"Hertz\", \"Parabolic\"",
                       "\nprint 'minPitch' 'newline$'", sep="")
    }
    if (get.maximum) {
        script <- paste(script,
                       "\nmaxPitch = Get maximum: targetStart, targetEnd, \"Hertz\", \"Parabolic\"",
                       "\nprint 'maxPitch' 'newline$'", sep="")
    }
    script <- paste( # remove pitch object
        script, "\nRemove\n", sep="")
    return(script)
}
