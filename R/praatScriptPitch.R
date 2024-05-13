#' Generates a script for extracting pitch, for use with \link{processWithPraat}
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of \link{processWithPraat}, in order to extract pitch information.
#'
#' @param get.mean Extract the mean pitch for the sample.
#' @param get.minimum Extract the minimum pitch for the sample.
#' @param get.maximum Extract the maximum pitch for the sample.
#' @param time.step Step setting for praat command
#' @param pitch.floor Minimum pitch (Hz) for all speakers, or for female speakers,
#'   if pitch.floor.male is also specified.
#' @param max.number.of.candidates Maximum number of candidates setting for praat command
#' @param very.accurate Accuracy setting for praat command
#' @param silence.threshold Silence threshold setting for praat command
#' @param voicing.threshold Voicing threshold (Hz) for all speakers, or for female speakers,
#'   if voicing.threshold.male is also specified.
#' @param octave.cost Octave cost setting for praat command
#' @param octave.jump.cost Octave jump cost setting for praat command
#' @param voiced.unvoiced.cost Voiced/unvoiced cost setting for praat command
#' @param pitch.ceiling Maximum pitch (Hz) for all speakers, or for female speakers,
#'   if pitch.floor.male is also specified.
#' @param pitch.floor.male Minimum pitch (Hz) for male speakers.
#' @param voicing.threshold.male Voicing threshold (Hz) for male speakers.
#' @param pitch.ceiling.male Maximum pitch (Hz) for male speakers.
#' @param gender.attribute Name of the LaBB-CAT participant attribute that contains the
#'   participant's gender - normally this is "participant_gender".
#' @param value.for.male The value that the gender.attribute has when the participant is male.
#' @param sample.points A vector of numbers (0 <= sample.points <= 1) specifying multiple
#'   points at which to take the measurement.  The default is NULL, meaning no
#'   individual measurements will be taken (only the aggregate values identified by
#'   get.mean, get.minimum, and get.maximum).  A single point at 0.5 means one
#'   measurement will be taken halfway through the target interval.  If, for example, 
#'   you wanted eleven measurements evenly spaced throughout the interval, you would
#'   specify sample.points as being 
#'   c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0).  
#' @param interpolation If sample.points are specified, this is the interpolation to use
#'   when getting individual values. Possible values are 'nearest' or 'linear'.
#' @param skip.errors Sometimes, for some segments, Praat fails to create a Pitch
#'   object. If skip.errors = TRUE, analysis those segments will be skipped, and corresponding
#'   pitch values will be returned as "--undefined--". If skip.errors = FALSE, the error
#'   message from Praat will be returned in the Error field, but no pitch measures will
#'   be returned for any segments in the same recording.
#' @return A script fragment which can be passed as the praat.script parameter of
#'   \link{processWithPraat} 
#' 
#' @seealso \link{processWithPraat}
#' @seealso \link{praatScriptFormants}
#' @seealso \link{praatScriptCentreOfGravity}
#' @seealso \link{praatScriptIntensity}
#' @seealso \link{praatScriptFastTrack}
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get pitch mean, max, and min, and the midpoint of the segment, for each match
#' pitch <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               praatScriptPitch(get.mean=TRUE, get.minimum=TRUE, get.maximum=TRUE,
#'                                sample.points = c(.5)))
#' }
#' @keywords praat
#' 
praatScriptPitch <- function(get.mean = TRUE, get.minimum = FALSE, get.maximum = FALSE, time.step = 0.0, pitch.floor = 60, max.number.of.candidates = 15, very.accurate = FALSE, silence.threshold = 0.03, voicing.threshold = 0.5, octave.cost = 0.01, octave.jump.cost = 0.35, voiced.unvoiced.cost = 0.35, pitch.ceiling = 500, pitch.floor.male = 30, voicing.threshold.male = 0.4, pitch.ceiling.male = 250, gender.attribute = 'participant_gender', value.for.male = "M", sample.points = NULL, interpolation = 'linear', skip.errors = TRUE) {
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
    if (skip.errors) { ## use nocheck
        script <- paste(
            script,
            "\n# nocheck to prevent the whole script from failing, and then check for object after",
            "\nnocheck ", sep="")
    } else {
        script <- paste(script, "\n", sep="")
    }
    
    script <- paste(
        script,
        "To Pitch (ac): ", time.step, ", pitchfloor, ", max.number.of.candidates, ", ",
        "veryaccurate$, ", silence.threshold, ", voicingthreshold, ", octave.cost, ", ",
        octave.jump.cost, ", ", voiced.unvoiced.cost, ", pitchceiling", sep="")
    
    script <- paste(
        script,
        "\n# check that a Pitch object was created",
        "\nobjectCreated = extractWord$(selected$(), \"\") = \"Pitch\"", sep="")
    if (get.mean) {
        script <- paste(script,
                       "\nif objectCreated",
                       "\n  meanPitch = Get mean: targetStart, targetEnd, \"Hertz\"",
                       "\nelse",
                       "\n  meanPitch = 1/0", # --undefined--
                       "\nendif",
                       "\nprint 'meanPitch' 'newline$'", sep="")
    }
    if (get.minimum) {
        script <- paste(script,
                       "\nif objectCreated",
                       "\n  minPitch = Get minimum: targetStart, targetEnd, \"Hertz\", \"Parabolic\"",
                       "\nelse",
                       "\n  minPitch = 1/0", # --undefined--
                       "\nendif",
                       "\nprint 'minPitch' 'newline$'", sep="")
    }
    if (get.maximum) {
        script <- paste(script,
                       "\nif objectCreated",
                       "\n  maxPitch = Get maximum: targetStart, targetEnd, \"Hertz\", \"Parabolic\"",
                       "\nelse",
                       "\n  maxPitch = 1/0", # --undefined--
                       "\nendif",
                       "\nprint 'maxPitch' 'newline$'", sep="")
    }
    if (!is.null(sample.points)) {
        for (point in sample.points) {
            varname = paste("time_", stringr::str_replace(point, "\\.","_"), "_for_pitch", sep="")
            ## first output absolute point offset
            script <- paste(script, "\npointoffset =",
                            " targetAbsoluteStart + ", point, " * targetDuration", sep="")
            script <- paste(script, "\n", varname, " = pointoffset", sep="")
            script <- paste(script, "\nprint '", varname, "' 'newline$'", sep="")
            ## now use the relative point offset
            script <- paste(script, "\npointoffset =",
                            " targetStart + ", point, " * targetDuration", sep="")
            varname = paste("pitch_time_", stringr::str_replace(point, "\\.","_"), sep="")
            script <- paste(script,
                            "\nif objectCreated",
                            "\n  ", varname,
                            " = Get value at time: pointoffset, \"Hertz\", \"",interpolation,"\"",
                            "\nelse",
                            "\n  ", varname, " = 1/0", # --undefined--
                            "\nendif",
                           sep="")
            script <- paste(script, "\nprint '", varname, ":0' 'newline$'", sep="")
        } ## next sample point
    }
    script <- paste( # remove pitch object
        script,
        "\nif objectCreated",
        "\n  Remove",
        "\nendif\n", sep="")
        
    return(script)
}
