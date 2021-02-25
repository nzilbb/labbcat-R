#' Process a set of intervals with Praat.
#'
#' This function instructs the LaBB-CAT server to invoke Praat for a set of sound
#' intervals, in order to extract acoustic measures.
#'
#' The exact measurements to return depend on the praat.script that is invoked. This is a
#' Praat script fragment that will run once for each sound interval specified.
#'
#' There are functions to allow the generation of a number of pre-defined praat scripts
#' for common tasks such as formant, pitch, intensity, and centre of gravity -- see
#' \link{praatScriptFormants}, \link{praatScriptCentreOfGravity}, \link{praatScriptIntensity}
#' and \link{praatScriptPitch}.
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
#' \itemize{
#'  \item{\emph{windowOffset}
#'    -- the value used for the Window Offset; how much context to include.} 
#'  \item{\emph{windowAbsoluteStart}
#'    -- the start time of the window extracted relative to the start of the original audio file.} 
#'  \item{\emph{windowAbsoluteEnd}
#'    -- the end time of the window extracted relative to the start of the original audio file.} 
#'  \item{\emph{windowDuration}
#'    -- the duration of the window extracted (including window offset).}
#'  \item{\emph{targetAbsoluteStart}
#'    -- the start time of the target interval relative to the start of the original audio file.} 
#'  \item{\emph{targetAbsoluteEnd}
#'    -- the end time of the target interval relative to the start of the original audio file.} 
#'  \item{\emph{targetStart}
#'    -- the start time of the target interval relative to the start of the window extracted.} 
#'  \item{\emph{targetEnd}
#'    -- the end time of the target interval relative to the start of the window extracted.}
#'  \item{\emph{targetDuration}
#'    -- the duration of the target interval.}
#'  \item{\emph{sampleNumber}
#'    -- the number of the sample within the set of samples being processed.} 
#'  \item{\emph{sampleName$}
#'    -- the name of the extracted/selected Sound object.}
#' }
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param matchIds A vector of annotation IDs, e.g. the MatchId column, or the URL column,
#'     of a results set. 
#' @param startOffsets The start time in seconds, or a vector of start times.
#' @param endOffsets The end time in seconds, or a vector of end times.
#' @param praat.script Script to run on each match. This may be a single string or a
#'     character vector.
#' @param window.offset In many circumstances, you will want some context before and after
#'     the sample start/end time.  For this reason, you can specify a "window offset" -
#'     this is a number of seconds to subtract from the sample start and add to the sample
#'     end time, before extracting that part of the audio for processing. For example, if
#'     the sample starts at 2.0s and ends at 3.0s, and you set the window offset to 0.5s,
#'     then Praat will extract a sample of audio from  1.5s to 3.5s, and do the selected
#'     processing on that sample.  
#' @param gender.attribute Which participant attribute represents the participant's gender.
#' @param attributes Vector of participant attributes to make available to the script. For
#'     example, if you want to use different acoustic parameters depending on what the
#'     gender of the speaker is, including the "participant_gender" attribute will make a
#'     variable called participant_gender$ available to the praat script, whose value will
#'     be the gender of the speaker for that segment.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
#' @return A data frame of acoustic measures, one row for each matchId.
#' 
#' @seealso \link{praatScriptFormants}
#' @seealso \link{praatScriptCentreOfGravity}
#' @seealso \link{praatScriptIntensity}
#' @seealso \link{praatScriptPitch}
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## get F1 and F2 for the mid point of the vowel
#' formants <- processWithPraat(
#'        labbcat.url,
#'        results$MatchId, results$Target.segment.start, results$Target.segment.end,
#'        praatScriptFormants(),
#'        no.progress=TRUE)
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
processWithPraat <- function(labbcat.url, matchIds, startOffsets, endOffsets,
                             praat.script, window.offset=0.0,
                             gender.attribute="participant_gender", attributes=NULL,
                             no.progress=FALSE) {

    ## make the script a single string
    praat.script <- paste(praat.script, collapse="\n")

    ## split matchIds into transcript ID and participant ID
    transcriptIds <- stringr::str_replace(matchIds, ".*(g_[0-9]+);.*","\\1")
    participantIds <- stringr::str_replace(matchIds, ".*(p_[0-9]+);.*","\\1")

    ## save CSV file to upload
    upload.file <- tempfile(pattern="processWithPraat.", fileext=".csv")
    write.table(data.frame(transcriptIds, participantIds, startOffsets, endOffsets),
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
        todo="upload",
        gender_attribute=gender.attribute, attribute=attributes,
        transcript="0", speaker="1", starttime="2", endtime="3", # column indices
        column_mapping=T, windowoffset=window.offset,
        custom_script=praat.script, pass_through_data="false",
        csv=httr::upload_file(upload.file))
    resp <- http.post.multipart(labbcat.url, "praat", parameters, download.file)

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
    if (!no.progress) {
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
        if (!is.null(thread$status)) {
            cat(paste("\n", thread$status, "\n", sep=""))
        }
    }

    ## download resulting CSV
    resp <- httr::GET(thread$resultUrl,
                      httr::write_disk(download.file, overwrite=TRUE),
                      httr::timeout(getOption("nzilbb.labbcat.timeout", default=180)))

    ## R complains if the last line isn't blank...
    write("\n",file=download.file,append=TRUE)
    
    ## load data into frame
    results <- read.csv(download.file, header=T)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(results)

}
