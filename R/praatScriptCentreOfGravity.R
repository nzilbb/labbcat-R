#' Generates a script for extracting the CoG, for use with \link{processWithPraat}
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of \link{processWithPraat}, in order to extract one or more spectral centre
#' of gravity (CoG) measurements. 
#'
#' @param powers A vector of numbers specifying which powers to query for to extract, e.g.
#'   c(1.0,2.0). 
#' @param spectrum.fast Whether to use the 'fast' option when creating the spectrum object
#'   to query .
#' @return A script fragment which can be passed as the praat.script parameter of
#'   \link{processWithPraat} 
#' 
#' @seealso \link{processWithPraat}
#' @seealso \link{praatScriptFormants}
#' @seealso \link{praatScriptIntensity}
#' @seealso \link{praatScriptPitch}
#' @seealso \link{praatScriptFastTrack}
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get centres of gravity for all matches
#' cog <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               praatScriptCentreOfGravity(powers=c(1.0,2.0)))
#' }
#' @keywords praat
#' 
praatScriptCentreOfGravity <- function(powers = c(2), spectrum.fast = TRUE) {
    # ensure the sound sample is selected
    script <- "\nselect Sound 'sampleName$'"
    if (spectrum.fast) {
        script <- paste(script, "\nfast$ = \"yes\"", sep="")
    } else {
        script <- paste(script, "\nfast$ = \"no\"", sep="")
    }
    script <- paste(
        script, "\nTo Spectrum: fast$", sep="")
    for (power in powers) {
        varname = paste("cog_", stringr::str_replace(power, "\\.","_"), sep="")
        script <- paste(script, "\n", varname, " = Get centre of gravity: ", power, sep="")
        script <- paste(script, "\nprint '", varname, ":0' 'newline$'", sep="")
    } ## next power
    ## remove spectrum object
    script <- paste(script, "\nRemove\n", sep="")
    return(script)
}
