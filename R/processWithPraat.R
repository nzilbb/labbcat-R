#' Process a set of intervals with Praat
#'
#' This function instructs the LaBB-CAT server to invoke Praat for a set of sound
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
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param match.ids A vector of annotation IDs, e.g. the MatchId column, or the URL column,
#'   of a results set. 
#' @param start.offsets The start time in seconds, or a vector of start times.
#' @param end.offsets The end time in seconds, or a vector of end times.
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
#'   doesn't influence the measurement. 
#' @param gender.attribute Which participant attribute represents the participant's gender.
#' @param attributes Vector of participant attributes to make available to the script. For
#'   example, if you want to use different acoustic parameters depending on what the
#'   gender of the speaker is, including the "participant_gender" attribute will make a
#'   variable called participant_gender$ available to the praat script, whose value will
#'   be the gender of the speaker for that segment.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return A data frame of acoustic measures, one row for each matchId.
#' 
#' @family Praat-related functions
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## get F1 and F2 for the mid point of the vowel
#' formants <- processWithPraat(
#'        labbcat.url,
#'        results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'        praatScriptFormants())
#' 
#' ## get first 3 formants at three points during the sample, the mean, min, and max
#' ## pitch, the max intensity, and the CoG using powers 1 and 2 
#' acoustic.measurements <- processWithPraat(
#'        labbcat.url,
#'        results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'        paste(
#'            praatScriptFormants(c(1,2,3), c(0.25,0.5,0.75)),
#'            praatScriptPitch(get.mean=TRUE, get.minimum=TRUE, get.maximum=TRUE),
#'            praatScriptIntensity(),
#'            praatScriptCentreOfGravity(powers=c(1.0,2.0))),
#'        window.offset=0.5)
#' 
#' ## execute a custom script loaded form a file
#' acoustic.measurements <- processWithPraat(
#'        labbcat.url,
#'        results$MatchId, results$Target.segment.start, result$Target.segment.end,
#'        readLines("acousticMeasurements.praat"))
#' }
#' @keywords praat
#' 
processWithPraat <- function(labbcat.url, match.ids, start.offsets, end.offsets,
                             praat.script, window.offset,
                             gender.attribute="participant_gender", attributes=NULL,
                             no.progress=FALSE) {

    ## make the script a single string
    praat.script <- paste(praat.script, collapse="\n")

    ## split match.ids into transcript ID and participant ID
    transcriptIds <- stringr::str_replace(match.ids, ".*(g_[0-9]+);.*","\\1")
    participantIds <- stringr::str_replace(match.ids, ".*(p_[0-9]+);.*","\\1")

    ## save CSV file to upload
    upload.file <- tempfile(pattern="processWithPraat.", fileext=".csv")
    write.table(data.frame(transcriptIds, participantIds, start.offsets, end.offsets),
                upload.file, sep=",", row.names=FALSE, col.names=TRUE)
    download.file <- tempfile(pattern="praatOutput.", fileext=".csv")

    if (is.null(attributes)) {
        attributes <- c(gender.attribute)
    }
    if (!is.element(gender.attribute, attributes)) {
        attributes <- c(attributes, gender.attribute)
    }

    ## upload CSV
    parameters <- list(
        attributes=attributes,
        transcriptColumn="0", participantColumn="1", startTimeColumn="2", endTimeColumn="3",
        windowOffset=window.offset,
        script=praat.script, passThroughData="false",
        csv=httr::upload_file(upload.file))
    resp <- http.post.multipart(labbcat.url, "api/praat", parameters, download.file)

    ## tidily remove the uploaded file
    file.remove(upload.file)

    ## check response
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)

    ## wait until the task is finished
    threadId <- resp.json$model$threadId
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)        
    }
    thread <- thread.get(labbcat.url, threadId)
    if (is.null(thread)) {
        return()
    }
    while (thread$running) {
        if (!is.null(pb) && !is.null(thread$percentComplete)) {
            setTxtProgressBar(pb, thread$percentComplete)
        }
        Sys.sleep(1)
        thread <- thread.get(labbcat.url, threadId)
        if (is.null(thread)) {
            return()
        }
    } # poll until finished
    if (!is.null(pb)) {
        if (!is.null(thread$percentComplete)) {
            setTxtProgressBar(pb, thread$percentComplete)
        }
        close(pb)
        if (!is.null(thread$status)) {
            cat(paste(thread$status, "\n", sep=""))
        }
    }

    ## download resulting CSV
    resp <- httr::GET(thread$resultUrl,
                      httr::write_disk(download.file, overwrite=TRUE),
                      httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)))

    ## R complains if the last line isn't blank...
    write("\n",file=download.file,append=TRUE)
    
    ## load data into frame
    ## (if we don't skip blank lines we get a row too many)
    results <- read.csv(download.file, header=T, blank.lines.skip=T)

    ## tidily remove the downloaded file
    file.remove(download.file)
    ## and release the server resources
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))
    
    return(results)

}
