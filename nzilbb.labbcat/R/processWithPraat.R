#' Process a set of intervals with Praat.
#'
#' This function instructs the LaBB-CAT server to invoke Praat for a set of sound
#' intervals, in order to exctract acoustic measures.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param matchIds A vector of annotation IDs, e.g. the MatchId column, or the URL column,
#'     of a results set. 
#' @param startOffsets The start time in seconds, or a vector of start times.
#' @param endOffsets The end time in seconds, or a vector of end times.
#' @param praat.script Script to run on each match.
#' @param gender.attribute Which participant attribute represents the participant's gender.
#' @param attributes Vector of participant attributes to make available to the script. For
#'     example, if you want to use different acoustic parameters depending on what the
#'     gender of the speaker is, including the "participant_gender" attribute will make a
#'     variable called participant_gender$ available to the praat script, whose value will
#'     be the gender of the speaker for that segment.
#' @param window.offset n many circumstances, you will want some context before and after
#'     the sample start/end time.  For this reason, you can specify a "window offset" -
#'     this is a number of seconds to subtract from the sample start and add to the sample
#'     end time, before extracting that part of the audio for processing. For example, if
#'     the sample starts at 2.0s and ends at 3.0s, and you set the window offset to 0.5s,
#'     then Praat will extract a sample of audio from  1.5s to 3.5s, and do the selected
#'     processing on that sample.  
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
#' @return A data frame of acoustic measures, one row for each matchId.
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segments="I"))
#' 
#' ## Get a list of fragments with no progress bar
#' wav.file <- processWithPraat(
#'               labbcat.url,
#'               results$MatchId, results$Target.segments.start, results$Target.segments.end,
#'               praat.script.formants(),
#'               no.progress=TRUE)
#' }
#' @keywords praat
#' 
processWithPraat <- function(labbcat.url, matchIds, startOffsets, endOffsets,
                             praat.script, gender.attribute="participant_gender", attributes=NULL,
                             window.offset=0.0, no.progress=FALSE) {
    
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

    ## load data into frame
    results <- read.csv(download.file, header=T)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(results)

}
