#' Generates a script for extracting maximum intensity, for use with \link{processWithPraat}
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of \link{processWithPraat}, in order to extract maximum intensity value.
#'
#' @param minimum.pitch Minimum pitch (Hz).
#' @param time.step Time step in seconds, or 0.0 for 'auto'.
#' @param subtract.mean Whether to subtract the mean or not.
#' @param get.maximum Extract the maximum intensity for the sample.
#' @param sample.points A vector of numbers (0 <= sample.points <= 1) specifying multiple
#'     points at which to take the measurement.  The default is NULL, meaning no
#'     individual measurements will be taken (only the aggregate values identified by
#'     get.mean, get.minimum, and get.maximum).  A single point at 0.5 means one
#'     measurement will be taken halfway through the target interval.  If, for example, 
#'     you wanted eleven measurements evenly spaced throughout the interval, you would
#'     specify sample.points as being 
#'     c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0).
#' @param interpolation If sample.points are specified, this is the interpolation to use
#'     when getting individual values. Possible values are 'nearest', 'linear', 'cubic',
#'     'sinc70', or 'sinc700'.
#' @param skip.errors Sometimes, for some segments, Praat fails to create an Intensity
#'     object. If skip.errors = TRUE, analysis those segments will be skipped, and corresponding
#'     pitch values will be returned as "--undefined--". If skip.errors = FALSE, the error
#'     message from Praat will be returned in the Error field, but no pitch measures will
#'     be returned for any segments in the same recording.
#' @return A script fragment which can be passed as the praat.script parameter of
#'     \link{processWithPraat} 
#' 
#' @seealso \link{processWithPraat}
#' @seealso \link{praatScriptFormants}
#' @seealso \link{praatScriptCentreOfGravity}
#' @seealso \link{praatScriptPitch}
#' @seealso \link{praatScriptFastTrack}
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="s"))
#' 
#' ## Get max intensity, and intensity at three points during the segment, for all matches
#' intensity <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               praatScriptIntensity(sample.points = c(.25, .5, .75)))
#' }
#' @keywords praat
#' 
praatScriptIntensity <- function(minimum.pitch = 100.0, time.step = 0.0, subtract.mean = TRUE, get.maximum = TRUE, sample.points = NULL, interpolation = 'cubic', skip.errors = TRUE) {
    if (subtract.mean) {
        script <- "\nsubtractmean$ = \"yes\""
    } else {
        script <- "\nsubtractmean$ = \"no\""
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
        script, "To Intensity: ", minimum.pitch, ", ", time.step, ", subtractmean$", sep="")

    script <- paste(
        script,
        "\n# check that an Intensity object was created",
        "\nobjectCreated = extractWord$(selected$(), \"\") = \"Intensity\"", sep="")
    if (get.maximum) {
        script <- paste(script,
                        "\nif objectCreated",
                        "\n  maxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
                        "\nelse",
                        "\n  maxIntensity = 1/0", # --undefined--
                        "\nendif",
                        "\nprint 'maxIntensity' 'newline$'", sep="")
    }
    
    if (!is.null(sample.points)) {
        for (point in sample.points) {
            varname = paste(
                "time_", stringr::str_replace(point, "\\.","_"), "_for_intensity", sep="")
            ## first output absolute point offset
            script <- paste(script, "\npointoffset =",
                            " targetAbsoluteStart + ", point, " * targetDuration", sep="")
            script <- paste(script, "\n", varname, " = pointoffset", sep="")
            script <- paste(script, "\nprint '", varname, "' 'newline$'", sep="")
            ## now use the relative point offset
            script <- paste(script, "\npointoffset =",
                            " targetStart + ", point, " * targetDuration", sep="")
            varname = paste("intensity_time_", stringr::str_replace(point, "\\.","_"), sep="")
            script <- paste(script,
                            "\nif objectCreated",
                            "\n  ", varname,
                            " = Get value at time: pointoffset, \"",interpolation,"\"",
                            "\nelse",
                            "\n  ", varname, " = 1/0", # --undefined--
                            "\nendif",
                            sep="")
            script <- paste(script, "\nprint '", varname, ":0' 'newline$'", sep="")
        } ## next sample point
    }
    
    script <- paste( # remove intensity object
        script,
        "\nif objectCreated",
        "\n  Remove",
        "\nendif\n", sep="")
    return(script)
}
