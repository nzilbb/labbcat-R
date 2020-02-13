#' Search for tokens.
#'
#' Searches through transcripts for tokens matching the given pattern.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param pattern An object representing the pattern to search for.
#'
#' Strictly speaking, this should be a named list that replicates the structure of the
#'     `search matrix' in the LaBB-CAT browser interface, with one element called
#'     ``columns'', containing a named list for each column.
#'
#' Each element in the ``columns'' named list contains an element named ``layers'', whose
#'     value is a named list for patterns to match on each layer, and optionally an
#'     element named ``adj'', whose value is a number representing the maximum distance, in
#'     tokens, between this column and the next column - if ``adj'' is not specified, the
#'     value defaults to 1, so tokens are contiguous.
#'
#' Each element in the ``layers'' named list is named after the layer it matches, and the
#'     value is a named list with the following possible elements:
#' \itemize{
#'  \item{\emph{pattern}  A regular expression to match against the label}
#'  \item{\emph{min}  An inclusive minimum numeric value for the label}
#'  \item{\emph{max}  An exclusive maximum numeric value for the label}
#'  \item{\emph{not}  TRUE to negate the match}
#'  \item{\emph{anchorStart}  TRUE to anchor to the start of the annotation on this layer
#'     (i.e. the matching word token will be the first at/after the start of the matching
#'     annotation on this layer)}
#'  \item{\emph{anchorEnd}  TRUE to anchor to the end of the annotation on this layer
#'     (i.e. the matching word token will be the last before/at the end of the matching
#'     annotation on this layer)}
#'  \item{\emph{target}  TRUE to make this layer the target of the search; the results will
#'     contain one row for each match on the target layer}
#' }
#'
#' Examples of valid pattern objects include:
#' \preformatted{
#' ## words starting with 'ps...'
#' pattern <- list(columns = list(
#'     list(layers = list(
#'            orthography = list(pattern = "ps.*")))))
#' 
#' ## the word 'the' followed immediately or with one intervening word by
#' ## a hapax legomenon (word with a frequency of 1) that doesn't start with a vowel
#' pattern <- list(columns = list(
#'     list(layers = list(
#'            orthography = list(pattern = "the")),
#'          adj = 2),
#'     list(layers = list(
#'            phonemes = list(not = TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\$@].*"),
#'            frequency = list(max = "2")))))
#' }
#' For ease of use, the function will also accept the following abbreviated forms:
#'
#' \preformatted{
#' ## a single list representing a 'one column' search, 
#' ## and string values, representing regular expression pattern matching
#' pattern <- list(orthography = "ps.*")
#'
#' ## a list containing the columns (adj defaults to 1, so matching tokens are contiguous)...
#' pattern <- list(
#'     list(orthography = "the"),
#'     list(phonemes = list(not = TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\$@].*"),
#'          frequency = list(max = "2")))
#' }
#' @param participantIds An optional list of participant IDs to search the utterances of. If
#'     not supplied, all utterances in the corpus will be searched.
#' @param main.participant TRUE to search only main-participant utterances, FALSE to
#'     search all utterances.
#' @param words.context Number of words context to include in the `Before.Match' and
#'     `After.Match' columns in the results.
#' @param maxMatches The maximum number of matches to return, or null to return all.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
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
#'  \item{\emph{Target.transcript} Text of the target word token}
#'  \item{\emph{Target.transcript.start} Start offset of the target word token}
#'  \item{\emph{Target.transcript.end} End offset of the target word token}
#'  \item{\emph{Target.segments} Label of the target segment (only present if the segment
#'     layer is included in the pattern)}
#'  \item{\emph{Target.segments.start} Start offset of the target segment (only present if the
#'     segment layer is included in the pattern)}
#'  \item{\emph{Target.segments.end} End offset of the target segment (only present if the
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
#' ## create a pattern object to match against
#' pattern <- list(columns = list(
#'     list(layers = list(
#'            orthography = list(pattern = "the")),
#'          adj = 2),
#'     list(layers = list(
#'            phonemes = list(not=TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\$@].*"),
#'            frequency = list(max = "2")))))
#' 
#' ## get the tokens matching the pattern
#' results <- getMatches(labbcat.url, pattern)
#'
#' ## results$MatchId can be used to access results
#' }
#'
#' @keywords search
#' 
getMatches <- function(labbcat.url, pattern, participantIds=NULL, main.participant=TRUE, words.context=0, maxMatches=NULL, no.progress=FALSE) { ## TODO transcriptTypes=NULL
    
    ## first normalize the pattern...

    ## if pattern isn't a list, convert it to one
    if (!is.list(pattern)) pattern <- as.list(pattern)

    ## if pattern isn't a list with a "columns" element, wrap a list around it
    if (is.null(pattern$columns)) pattern <- list(columns = pattern)

    ## if pattern$columns isn't a 'nameless' list (i.e. with numeric indices) wrap a
    ## nameless list around it
    if (!is.null(names(pattern$columns))) pattern$columns <- list(pattern$columns)

    ## columns contain lists with no "layers" element, wrap a list around them
    for (c in 1:length(pattern$columns)) {
        if (is.null(pattern$columns[[c]]$layers)) {
            pattern$columns[[c]] <- list(layers = pattern$columns[[c]])
        }
    } # next column

    ## convert layer=string to layer=list(pattern=string)
    segments.layer <- FALSE # (and check for searching the "segments" layer)
    for (c in 1:length(pattern$columns)) { # for each column
        for (l in names(pattern$columns[[c]]$layers)) { # for each layer in the column
            # if the layer value isn't a list
            if (!is.list(pattern$columns[[c]]$layers[[l]])) {
                # wrap a list(pattern=...) around it
                pattern$columns[[c]]$layers[[l]] <- list(pattern = pattern$columns[[c]]$layers[[l]])
            } # value isn't a list
            if (l == "segments") segments.layer <- TRUE
        } # next layer
    } # next column

    ## start the search
    searchJson <- jsonlite::toJSON(pattern, auto_unbox = TRUE)
    parameters <- list(command="search", searchJson=searchJson,
                       words_context=words.context)
    if (main.participant) parameters$only_main_speaker <- TRUE
    if (!is.null(participantIds)) parameters$participant_id <- as.list(participantIds)
    resp <- http.get(labbcat.url, "search", parameters)
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
    if (!no.progress) {
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
    
    # now that the search is finished, get the results as CSV
    # (ignore thread$resultUrl - we want the results stream, which starts returning immediately
    # and saves memory on the server)
    download.file <- paste(thread$threadName, ".csv", sep="");
    # columns:
    csv_option <- c("collection_name", "result_number", "transcript_name", "speaker_name", 
                    "line_time", "line_end_time", "match", "result_text", "word_url")
    # layers - "transcript", and "segments" if mentioned in the pattern
    csv_layer_option <- c("0")
    if (segments.layer) csv_layer_option <- c("0","1")
    resp <- http.get(labbcat.url,
                     "resultsStream",
                     list(threadId=threadId, todo="csv", csvFieldDelimiter=",",
                          csv_option=csv_option, csv_layer_option=csv_layer_option,
                          pageLength=maxMatches),
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
