#' Generates a script for extracting maximum intensity, for use with \link{processWithPraat}.
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of \link{processWithPraat}, in order to extract maximum intensity value.
#'
#' @param minimum.pitch Minimum pitch (Hz).
#' @param time.step Time step in seconds, or 0.0 for 'auto'.
#' @param subtract.mean Whether to subtract the mean or not.
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
praatScriptIntensity <- function(minimum.pitch = 100.0, time.step = 0.0, subtract.mean = TRUE) {
    if (subtract.mean) {
        script <- "\nsubtractmean$ = \"yes\""
    } else {
        script <- "\nsubtractmean$ = \"no\""
    }
    script <- paste( # ensure the sound sample is selected
        script, "\nselect Sound 'sampleName$'", sep="")
    
    script <- paste(
        script, "\nTo Intensity: ", minimum.pitch, ", ", time.step, ", subtractmean$", sep="")

    script <- paste(script,
                   "\nmaxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
                   "\nprint 'maxIntensity' 'newline$'", sep="")
    
    script <- paste( # remove intensity object
        script, "\nRemove\n", sep="")
    return(script)
}
