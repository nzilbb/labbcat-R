#' Generates a script for extracting formants using FastTrack, for use with
#' \link{processWithPraat}. 
#'
#' This function generates a Praat script fragment which can be passed as the praat.script
#' parameter of \link{processWithPraat}, in order to extract selected formants using the
#' FastTrack Praat plugin.
#'
#' The FastTrack Praat plugin, developed by Santiago Barreda, automatically runs multiple
#' formant analyses on each segment, selects the best (the smoothest, with optional
#' heuristics), and makes the winning formant object available for measurement. For more
#' information, see \link{https://github.com/santiagobarreda/FastTrack}
#'
#' @param formants A vector of integers specifying which formants to extract, e.g c(1,2)
#'     for the first and second formant.
#' @param sample.points A vector of numbers (0 <= sample.points <= 1) specifying multiple
#'     points at which to take the measurement.  The default is a single point at 0.5 -
#'     this means one measurement will be taken halfway through the target interval.  If,
#'     for example, you wanted eleven measurements evenly spaced throughout the interval,
#'     you would specify sample.points as being
#'     c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0).
#' @param lowest.analysis.frequency Lowest analysis frequency (Hz) by default.
#' @param lowest.analysis.frequency.male Lowest analysis frequency (Hz) for male
#'     speakers, or NULL to use the same value as lowest.analysis.frequency.
#' @param highest.analysis.frequency Highest analysis frequency (Hz) by default.
#' @param highest.analysis.frequency.male Highest analysis frequency (Hz) for male
#'     speakers, or NULL to use the same value as highest.analysis.frequency.
#' @param gender.attribute Name of the LaBB-CAT participant attribute that contains the
#'     participant's gender - normally this is "participant_gender".
#' @param value.for.male The value that the gender.attribute has when the participant is male.
#' @param time.step Time step in seconds.
#' @param tracking.method tracking_method parameter for trackAutoselectProcedure; "burg"
#'     or "robust". 
#' @param number.of.formants Number of formants to track - 3 or 4. 
#' @param maximum.f1.frequency Specifying a non-NULL value enables the F1 frequency
#'     heuristic: Median F1 frequency should not be higher than this value.
#' @param maximum.f1.bandwidth Specifying a non-NULL value (e.g. 500)
#'     enables the F1 bandwidth heuristic: Median F1 bandwidth should not be higher than
#'     this value. 
#' @param maximum.f2.bandwidth Specifying a non-NULL value (e.g. 600)
#'     enables the F2 bandwidth heuristic: Median F2 bandwidth should not be higher than
#'     this value. 
#' @param maximum.f3.bandwidth Specifying a non-NULL value (e.g. 900)
#'     enables the F3 bandwidth heuristic: Median F3 bandwidth should not be higher than
#'     this value. 
#' @param minimum.f4.frequency Specifying a non-NULL value enables the F4 frequency
#'     heuristic: Median F4 frequency should not be lower than this value.
#' @param enable.rhotic.heuristic Whether to enable the rhotic heuristic: If F3 < 2000 Hz,
#'     F1 and F2 should be at least 500 Hz apart.
#' @param enable.f3.f4.proximity.heuristic Whether to enable the F3/F4 proximity
#'     heuristic: If (F4 - F3) < 500 Hz, F1 and F2 should be at least 1500 Hz apart.
#' @param number.of.steps Number of analyses between low and high analysis limits. More
#'     analysis steps may improve results, but will increase analysis time (50% more steps
#'     = around 50% longer to analyze).
#' @param number.of.coefficients Number of coefficients for formant prediction. More
#'     coefficients allow for more sudden, and 'wiggly' formant motion.
#' @return A script fragment which can be passed as the praat.script parameter of
#'     \link{processWithPraat} 
#' 
#' @seealso \link{processWithPraat}
#' @seealso \link{praatScriptCentreOfGravity}
#' @seealso \link{praatScriptIntensity}
#' @seealso \link{praatScriptPitch}
#' @seealso \link{praatScriptFormants}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get all tokens of the KIT vowel
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get the first 3 formants at three points during the vowel
#' formants <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'               window.offset=0.025,
#'               praatScriptFastTrack(formants=c(1,2,3),
#'               sample.points=c(0.25,0.5,0.75)))
#' }
#' @keywords praat
#' 
praatScriptFastTrack <- function(formants = c(1,2), sample.points = c(0.5), lowest.analysis.frequency = 5000, lowest.analysis.frequency.male = 4500, highest.analysis.frequency = 7000, highest.analysis.frequency.male = 6500, gender.attribute = 'participant_gender', value.for.male = "M", time.step = 0.002, tracking.method = "burg", number.of.formants = 3, maximum.f1.frequency = 1200, maximum.f1.bandwidth = NULL, maximum.f2.bandwidth = NULL, maximum.f3.bandwidth = NULL, minimum.f4.frequency = 2900, enable.rhotic.heuristic = TRUE, enable.f3.f4.proximity.heuristic = TRUE, number.of.steps = 20, number.of.coefficients = 5) {
    
    ## include FastTrack script and load default global settings
    script <- "\ninclude utils/trackAutoselectProcedure.praat\n@getSettings"
    ## override global settings that we know about
    script <- paste(script,"\ntime_step = ", time.step, sep='')
    script <- paste(script,"\nmethod$ = \"", tracking.method, "\"", sep='')
    if (is.null(maximum.f1.frequency)) {
        script <- paste(script, "\nenable_F1_frequency_heuristic = 0", sep='')
    } else {
        script <- paste(script,"\nenable_F1_frequency_heuristic = 1", sep='')
        script <- paste(script,"\nmaximum_F1_frequency_value = ", maximum.f1.frequency, sep='')
    }
    if (is.null(maximum.f1.bandwidth)) {
        script <- paste(script,"\nenable_F1_bandwidth_heuristic = 0", sep='')
    } else {
        script <- paste(script,"\nenable_F1_bandwidth_heuristic = 1", sep='')
        script <- paste(script,"\nmaximum_F1_bandwidth_value = ", maximum.f1.bandwidth, sep='')
    }
    if (is.null(maximum.f2.bandwidth)) {
        script <- paste(script,"\nenable_F2_bandwidth_heuristic = 0", sep='')
    } else {
        script <- paste(script,"\nenable_F2_bandwidth_heuristic = 1", sep='')
        script <- paste(script,"\nmaximum_F2_bandwidth_value = ", maximum.f2.bandwidth, sep='')
    }
    if (is.null(maximum.f3.bandwidth)) {
        script <- paste(script,"\nenable_F3_bandwidth_heuristic = 0", sep='')
    } else {
        script <- paste(script,"\nenable_F3_bandwidth_heuristic = 1", sep='')
        script <- paste(script,"\nmaximum_F3_bandwidth_value = ", maximum.f3.bandwidth, sep='')
    }
    if (is.null(minimum.f4.frequency)) {
        script <- paste(script,"\nenable_F4_frequency_heuristic = 0", sep='')
    } else {
        script <- paste(script,"\nenable_F4_frequency_heuristic = 1", sep='')
        script <- paste(script,"\nminimum_F4_frequency_value = ", minimum.f4.frequency, sep='')
    }
    if (enable.rhotic.heuristic) {
        script <- paste(script,"\nenable_rhotic_heuristic = 1", sep='')
    } else {
        script <- paste(script,"\nenable_rhotic_heuristic = 0", sep='')
    }
    if (enable.f3.f4.proximity.heuristic) {
        script <- paste(script,"\nenable_F3F4_proximity_heuristic = 1", sep='')
    } else {
        script <- paste(script,"\nenable_F3F4_proximity_heuristic = 0", sep='')
    }
    script <- paste(script,"\noutput_bandwidth = 1", sep='')
    script <- paste(script,"\noutput_predictions = 1", sep='')
    script <- paste(script,"\noutput_pitch = 1", sep='')
    script <- paste(script,"\noutput_intensity = 1", sep='')
    script <- paste(script,"\noutput_harmonicity = 1", sep='')
    script <- paste(script,"\noutput_normalized_time = 1", sep='')
    script <- paste(script,"\ndir$ = \".\"", sep='')
    script <- paste(script,"\nsteps = ", number.of.steps, sep='')
    script <- paste(script,"\ncoefficients = ", number.of.coefficients, sep='')
    script <- paste(script,"\nformants = ", number.of.formants, sep='')
    script <- paste(script,"\nout_formant = 2", sep='')
    script <- paste(script,"\nimage = 0", sep='')
    script <- paste(script,"\nmax_plot = 4000", sep='')
    script <- paste(script,"\nout_table = 0", sep='')
    script <- paste(script,"\nout_all = 0", sep='')
    script <- paste(script,"\ncurrent_view = 0", sep='')
    ## Segments shorter than fastTrackMinimumDuration will be ignored, and empty values returned.
    ## The FastTrack limit is 0.03s (30ms), but because of possible rounding errors
    ## in Praat arithemitic, the default is set slightly higher than this.
    script <- paste(script,"\nfastTrackMinimumDuration = 0.030000000000001", sep='')
    
    script <- paste(script,"\nlowestAnalysisFrequency = ", lowest.analysis.frequency, sep='') 
    script <- paste(script,"\nhighestAnalysisFrequency = ", highest.analysis.frequency, sep='') 
    if (!is.null(gender.attribute)) {
        ## differentiate between males and others
        script <- paste(script, "\nif ", gender.attribute, "$ = \"", value.for.male, "\"", sep="")
        if (!is.null(lowest.analysis.frequency.male)
            && lowest.analysis.frequency != lowest.analysis.frequency.male) {
            script <- paste(
                script, "\n  lowestAnalysisFrequency = ", lowest.analysis.frequency.male, sep="")
        }
        if (!is.null(highest.analysis.frequency.male)
            && highest.analysis.frequency != highest.analysis.frequency.male) {
            script <- paste(
                script, "\n  highestAnalysisFrequency = ", highest.analysis.frequency.male, sep="")
        }
        script <- paste(script, "\nendif", sep="")
    }
    script <- paste( # ensure the sound sample is selected
        script, "\nselect Sound 'sampleName$'", sep="")
    script <- paste(
        script,
        "\nif windowDuration >= fastTrackMinimumDuration",
        "\n  @trackAutoselect: selected(), dir$, lowestAnalysisFrequency, highestAnalysisFrequency, steps, coefficients, formants, method$, image, selected(), current_view, max_plot, out_formant, out_table, out_all", sep="")
    for (point in sample.points) {
        varname = paste("time_", stringr::str_replace(point, "\\.","_"), sep="")
        ## first output absolute point offset
        script <- paste(script, "\n  pointoffset =",
                        " targetAbsoluteStart + ", point, " * targetDuration", sep="")
        script <- paste(script, "\n  ", varname, " = pointoffset", sep="")
        ## now use the relative point offset
        script <- paste(script, "\n  pointoffset =",
                        " targetStart + ", point, " * targetDuration", sep="")
        for (f in formants) {
            varname = paste("f", f, "_time_", stringr::str_replace(point, "\\.","_"), sep="")
            script <- paste(script, "\n  ", varname,
                            " = Get value at time: ", f, ", pointoffset, \"hertz\", \"Linear\"",
                            sep="")
        } ## next formant
    } ## next sample point
    ## remove formant object
    script <- paste(script, "\n  Remove", sep="")
    script <- paste(script, "\nelse", sep="") ## sample is too short, output blank values
    for (point in sample.points) {
        varname = paste("time_", stringr::str_replace(point, "\\.","_"), sep="")
        ## first output absolute point offset
        script <- paste(script, "\n  pointoffset =",
                        " targetAbsoluteStart + ", point, " * targetDuration", sep="")
        script <- paste(script, "\n  ", varname, " = pointoffset", sep="")
        for (f in formants) {
            varname = paste("f", f, "_time_", stringr::str_replace(point, "\\.","_"), sep="")
            script <- paste(script, "\n  ", varname, " = \"\"", sep="")
        } ## next formant
    } ## next sample point
    script <- paste(script, "\nendif\n", sep="")
    
    ## To ensure LaBB-cAT can interpret correctly the number of outputs,
    ## only include one set of print statements:
    for (point in sample.points) {
        varname = paste("time_", stringr::str_replace(point, "\\.","_"), sep="")
        ## first output absolute point offset
        script <- paste(script, "\nprint '", varname, "' 'newline$'", sep="")
        for (f in formants) {
            varname = paste("f", f, "_time_", stringr::str_replace(point, "\\.","_"), sep="")
            script <- paste(script, "\nprint '", varname, "' 'newline$'", sep="")
        } ## next formant
    } ## next sample point
    script <- paste(script, "\n", sep="")
    return(script)
}
