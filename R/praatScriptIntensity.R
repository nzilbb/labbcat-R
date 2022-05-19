#' Generates a script for extracting maximum intensity, for use with \link{processWithPraat}.
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
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="s"))
#' 
#' ## Get intensity for all matches
#' intensity <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               praatScriptIntensity())
#' }
#' @keywords praat
#' 
praatScriptIntensity <- function(minimum.pitch = 100.0, time.step = 0.0, subtract.mean = TRUE, get.maximum = TRUE, sample.points = NULL, interpolation = 'cubic') {
    if (subtract.mean) {
        script <- "\nsubtractmean$ = \"yes\""
    } else {
        script <- "\nsubtractmean$ = \"no\""
    }
    script <- paste( # ensure the sound sample is selected
        script, "\nselect Sound 'sampleName$'", sep="")
    
    script <- paste(
        script, "\nTo Intensity: ", minimum.pitch, ", ", time.step, ", subtractmean$", sep="")

    if (get.maximum) {
        script <- paste(script,
                        "\nmaxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
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
            script <- paste(script, "\n", varname,
                           " = Get value at time: pointoffset, \"",interpolation,"\"",
                           sep="")
            script <- paste(script, "\nprint '", varname, ":0' 'newline$'", sep="")
        } ## next sample point
    }
    
    script <- paste( # remove intensity object
        script, "\nRemove\n", sep="")
    return(script)
}
