#' Get all utterances of participants.
#'
#' Identifies all utterances of a given set of participants.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param participant.ids A list of participant IDs to identify the utterances of.
#' @param transcript.types An optional list of transcript types to limit the results
#'     to. If null, all transcript types will be searched. 
#' @param main.participant TRUE to search only main-participant utterances, FALSE to
#'     search all utterances.
#' @param max.matches The maximum number of matches to return, or null to return all.
#' @return A data frame identifying matches, containing the following columns:
#' \itemize{
#'  \item{\emph{SearchName} A name based on the pattern -- the same for all rows}
#'  \item{\emph{Number} Row number}
#'  \item{\emph{Transcript} Name of the transcript in which the match was found}
#'  \item{\emph{Line} The start offset of the utterance/line}
#'  \item{\emph{LineEnd} The end offset of the utterance/line}
#'  \item{\emph{MatchId} A unique ID for the matching target token}
#'  \item{\emph{Before.Match} Transcript text immediately before the match}
#'  \item{\emph{Text} Transcript text of the match}
#'  \item{\emph{Before.Match} Transcript text immediately after the match}
#'  \item{\emph{Target.word} Text of the target word token}
#'  \item{\emph{Target.word.start} Start offset of the target word token}
#'  \item{\emph{Target.word.end} End offset of the target word token}
#'  \item{\emph{Target.segment} Label of the target segment (only present if the segment
#'     layer is included in the pattern)}
#'  \item{\emph{Target.segment.start} Start offset of the target segment (only present if the
#'     segment layer is included in the pattern)}
#'  \item{\emph{Target.segment.end} End offset of the target segment (only present if the
#'     segment layer is included in the pattern)}
#' }
#' 
#' @seealso \code{\link{getParticipantIds}}
#' 
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#'
#' ## get all utterances of the given participants
#' participant.ids <- getParticipantIds(labbcat.url)[1:3]
#' results <- getAllUtterances(labbcat.url, participant.ids)
#'
#' ## results$MatchId can be used to access results
#' }
#'
#' @keywords search
#' 
getAllUtterances <- function(labbcat.url, participant.ids, transcript.types=NULL, main.participant=TRUE, max.matches=NULL) {
    
    ## start the task
    parameters <- list(list="list", id=as.list(participant.ids))
    if (main.participant) {
        parameters$only_main_speaker <- TRUE
    }
    if (!is.null(transcript.types)) {
        parameters$transcript_type <- as.list(transcript.types)
    }
    
    resp <- http.get(labbcat.url, "allUtterances", parameters)
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)

    ## we get a task ID back
    threadId <- resp.json$model$threadId

    pb <- NULL
    if (interactive()) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)        
    }

    ## monitor the task until it finishes
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
    
    # now that the task is finished, get the results as CSV
    # (ignore thread$resultUrl - we want the results stream, which starts returning immediately
    # and saves memory on the server)
    download.file <- paste(thread$threadName, ".csv", sep="");
    # columns:
    csv_option <- c("collection_name", "result_number", "transcript_name", "speaker_name", 
                    "line_time", "line_end_time", "match", "result_text", "word_url")
    resp <- http.get(labbcat.url,
                     "resultsStream",
                     list(threadId=threadId, todo="csv", csvFieldDelimiter=",",
                          csv_option=csv_option, csv_layer_option=c(),
                          pageLength=max.matches),
                     content.type="text/csv",
                     file.name = download.file)
    if (is.null(resp)) return()
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(httr::content(resp, as="text", encoding="UTF-8"))
        return()
    }
    
    ## free the search thread so it's not using server resources
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))

    ## load the returned entries
    results <- read.csv(download.file, header=T)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(results)
}
