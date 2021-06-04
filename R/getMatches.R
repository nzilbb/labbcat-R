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
#' @param participant.ids An optional list of participant IDs to search the utterances of. If
#'     not supplied, all utterances in the corpus will be searched.
#' @param transcript.types An optional list of transcript types to limit the results
#'     to. If null, all transcript types will be searched. 
#' @param main.participant TRUE to search only main-participant utterances, FALSE to
#'     search all utterances.
#' @param aligned true to include only words that are aligned (i.e. have anchor
#'     confidence &ge; 50, false to search include un-aligned words as well. 
#' @param matches.per.transcript Optional maximum number of matches per transcript to
#'     return. NULL means all matches.
#' @param words.context Number of words context to include in the `Before.Match' and
#'     `After.Match' columns in the results.
#' @param max.matches The maximum number of matches to return, or null to return all.
#' @param overlap.threshold The percentage overlap with other utterances before
#'     simultaneous speech is excluded, or null to include overlapping speech.
#' @param page.length In order to prevent timeouts when there are a large number of
#'     matches or the network connection is slow, rather than retrieving matches in one
#'     big request, they are retrieved using many smaller requests. This parameter
#'     controls the number of results retrieved per request.
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'     shown when interactive().
#' @return A data frame identifying matches, containing the following columns:
#' \itemize{
#'  \item{\emph{SearchName} A name based on the pattern -- the same for all rows}
#'  \item{\emph{MatchId} A unique ID for the matching target token}
#'  \item{\emph{Transcript} Name of the transcript in which the match was found}
#'  \item{\emph{Participant} Name of the speaker}
#'  \item{\emph{Corpus} The corpus of the transcript}
#'  \item{\emph{Line} The start offset of the utterance/line}
#'  \item{\emph{LineEnd} The end offset of the utterance/line}
#'  \item{\emph{Before.Match} Transcript text immediately before the match}
#'  \item{\emph{Text} Transcript text of the match}
#'  \item{\emph{After.Match} Transcript text immediately after the match}
#'  \item{\emph{Number} Row number}
#'  \item{\emph{URL} URL of the first matching word token}
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
#' ## create a pattern object to match against
#' pattern <- list(columns = list(
#'     list(layers = list(
#'            orthography = list(pattern = "the")),
#'          adj = 2),
#'     list(layers = list(
#'            phonemes = list(not=TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\$@].*"),
#'            frequency = list(max = "2")))))
#' 
#' ## get the tokens matching the pattern, excluding overlapping speech
#' results <- getMatches(labbcat.url, pattern, overlap.threshold = 5)
#'
#' ## results$MatchId can be used to access results
#' }
#'
#' @keywords search
#' 
getMatches <- function(labbcat.url, pattern, participant.ids=NULL, transcript.types=NULL, main.participant=TRUE, aligned=FALSE, matches.per.transcript=NULL, words.context=0, max.matches=NULL, overlap.threshold=NULL, page.length=1000, no.progress=FALSE) {
    
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
    segment.layer <- FALSE # (and check for searching the "segment" layer)
    for (c in 1:length(pattern$columns)) { # for each column
        for (l in names(pattern$columns[[c]]$layers)) { # for each layer in the column
            # if the layer value isn't a list
            if (!is.list(pattern$columns[[c]]$layers[[l]])) {
                # wrap a list(pattern=...) around it
                pattern$columns[[c]]$layers[[l]] <- list(pattern = pattern$columns[[c]]$layers[[l]])
            } # value isn't a list
            if (l == "segment") segment.layer <- TRUE
        } # next layer
    } # next column

    ## start the search
    searchJson <- jsonlite::toJSON(pattern, auto_unbox = TRUE)
    parameters <- list(command="search", searchJson=searchJson,
                       words_context=words.context)
    if (main.participant) {
        parameters$only_main_speaker <- TRUE
    }
    if (aligned) {
        parameters$only_aligned <- TRUE
    }
    if (!is.null(matches.per.transcript)) {
        parameters$matches_per_transcript <- as.list(matches.per.transcript)
    }
    if (!is.null(participant.ids)) {
        parameters$participant_id <- as.list(participant.ids)
    }
    if (!is.null(transcript.types)) {
        parameters$transcript_type <- as.list(transcript.types)
    }
    if (!is.null(overlap.threshold)) {
        parameters$overlap_threshold <- overlap.threshold
    }
    
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
    if (interactive() && !no.progress) {
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
            close(pb)
        }
        if (!is.null(thread$status)) {
            cat(paste(thread$status, " - fetching data...", "\n", sep=""))
        }
    }

    ## define the dataframe to return (which is, for now, empty)
    allMatches <- data.frame(matrix(ncol = 15, nrow = 0))
    if (segment.layer) {
        allMatches <- data.frame(matrix(ncol = 18, nrow = 0))
    }
    if (thread$size > 0) { ## there were actually some matches
        
        ## ensure labbcat base URL has a trailing slash (for URL reconstruction)
        if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

        ## layers - "word", and "segment" if mentioned in the pattern
        tokenLayers <- c("word")
        if (segment.layer) tokenLayers <- c("word", "segment")
        
        ## search results can be very large, and httr timeouts are short and merciless,
        ## so we break the results into chunks and retrieve them using lots of small
        ## requests instead of one big request
        
        totalMatches <- min(thread$size, max.matches) ## (works even if max.matches == NULL)
        matchesLeft <- totalMatches
        pageNumber <- 0

        pb <- NULL
        if (interactive()) {
            pb <- txtProgressBar(min = 0, max = matchesLeft, style = 3)        
        }
        
        while(matchesLeft > 0) { ## loop until we've got all the matches we want        
            resp <- http.get(labbcat.url,
                             "resultsStream",
                             list(threadId=threadId, words_context=words.context,
                                  pageLength=page.length, pageNumber=pageNumber),
                             content.type="application/json")
            if (is.null(resp)) break
            if (httr::status_code(resp) != 200) { # 200 = OK
                print(paste("ERROR: ", httr::http_status(resp)$message))
                print(httr::content(resp, as="text", encoding="UTF-8"))
                break
            }
        
            resp.content <- httr::content(resp, as="text", encoding="UTF-8")
            resp.json <- jsonlite::fromJSON(resp.content)
            matches <- resp.json$model$matches
            
            ## decrement the number of rows left to get
            matchesLeft <- matchesLeft - nrow(matches)
            ## ensure we don't have too many rows (on the last page)
            if (matchesLeft < 0) { 
                matches <- head(matches, page.length + matchesLeft)
            }

            matches <- cbind(resp.json$model$name, matches)
            ## extract number from MatchId
            matches <- cbind(
                matches, as.numeric(stringr::str_match(matches$MatchId, "prefix=0*([0-9]+)-")[,2]))
            ## reconstruct url
            matches <- cbind(
                matches, paste(
                             labbcat.url, "transcript?transcript=",
                             matches$Transcript, "#",
                             stringr::str_match(matches$MatchId, "\\[0\\]=(.*)(;.*|$)")[,2], sep=""))
            tokens <- getMatchAlignments(labbcat.url, matches$MatchId, tokenLayers, no.progress=T)
            matches <- cbind(matches, tokens)

            ## add this chunk to the collection
            allMatches <- rbind(allMatches, matches)

            if (!is.null(pb)) {
                setTxtProgressBar(pb, nrow(allMatches))
            }

            ## next page
            pageNumber <- pageNumber + 1
        } ## loop
    } ## there are matches
    ## finished with the progress bar
    if (!is.null(pb)) {
        setTxtProgressBar(pb, nrow(allMatches))
        close(pb)
    }
    
    frameNames <- c(
        "SearchName","MatchId","Transcript","Participant","Corpus","Line","LineEnd",
        "Before.Match","Text","After.Match","Number","URL",
        "Target.word","Target.word.start","Target.word.end")
    if (segment.layer) {
        frameNames <- c(frameNames,
                        c("Target.segment", "Target.segment.start", "Target.segment.end"))
    }
    names(allMatches) = frameNames
    
    ## free the search thread so it's not using server resources
    http.get(labbcat.url, "threads", list(threadId=threadId, command="release"))

    return(allMatches)
}
