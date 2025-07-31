#' Search for tokens
#'
#' Searches through transcripts for tokens matching the given pattern.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param pattern An object representing the pattern to search for.
#'
#' This can be:
#' * A string, representing a search of the orthography layer - spaces are
#'   taken to be word boundaries
#' * A single named list, representing a one-column search - names are taken to be layer IDs
#' * A list of named lists, representing a multi-column search - the outer list
#'   represents the columns of the search matrix where each column 'immediately
#'   follows' the previous, and the names of the inner lists are taken to be layer IDs
#' * A named list (or for segment layers, a list of named lists) fully replicating
#'   the structure of the search matrix in the LaBB-CAT browser interface, with one
#'   element called "columns", containing a named list for each column.  
#'
#'   Each element in the "columns" named list contains an element named "layers", whose
#'   value is a named list (or a list of named lists) for patterns to match on each
#'   layer, and optionally an element named "adj", whose value is a number representing
#'   the maximum distance, in tokens, between this column and the next column - if "adj"
#'   is not specified, the value defaults to 1, so tokens are contiguous.  
#'
#'   Each element in the "layers" named list is named after the layer it matches, and the
#'   value is a named list with the following possible elements:
#'   - *pattern*  A regular expression to match against the label
#'   - *min*  An inclusive minimum numeric value for the label
#'   - *max*  An exclusive maximum numeric value for the label
#'   - *not*  TRUE to negate the match
#'   - *anchorStart*  TRUE to anchor to the start of the annotation on this layer
#'     (i.e. the matching word token will be the first at/after the start of the matching
#'     annotation on this layer)
#'   - *anchorEnd*  TRUE to anchor to the end of the annotation on this layer
#'     (i.e. the matching word token will be the last before/at the end of the matching
#'     annotation on this layer)
#'   - *target*  TRUE to make this layer the target of the search; the
#'     results will contain one row for each match on the target layer
#'
#' Examples of valid pattern objects include:
#' ```
#' ## the word 'the' followed immediately by a word starting with an orthographic vowel
#' pattern <- "the [aeiou].*"
#' 
#' ## a word spelt with "k" but pronounced "n" word initially
#' pattern <- list(orthography = "k.*", phonemes = "n.*")
#' 
#' ## the word 'the' followed immediately by a word starting with a phonemic vowel
#' pattern <- list(
#'     list(orthography = "the"),
#'     list(phonemes = "[cCEFHiIPqQuUV0123456789~#\\$@].*"))
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
#'
#' ## words that contain the /I/ phone followed by the /l/ phone
#' ## (multiple patterns per word currently only works for segment layers)
#' pattern <- list(segment = list("I", "l"))
#'
#' ## words that contain the /I/ phone followed by the /l/ phone, targeting the /l/ segment
#' ## (multiple patterns per word currently only works for segment layers)
#' pattern <- list(segment = list("I", list(pattern="l", target=T)))
#'
#' ## words where the spelling starts with "k", but the first segment is /n/
#' pattern <- list(
#'   orthography = "k.*", 
#'   segment = list(pattern = "n", anchorStart = T)
#' 
#' ```
#' @param participant.expression An optional participant query expression for identifying
#'   participants to search the utterances of. This should be the output of
#'   [expressionFromIds], [expressionFromAttributeValue],
#'   or [expressionFromAttributeValues], or more than one concatentated together
#'   and delimited by ' && '. If not supplied, utterances of all participants will be searched.
#' @param transcript.expression An optional transript query expression for identifying
#'   transcripts to search in. This should be the output of [expressionFromIds],
#'   [expressionFromTranscriptTypes], [expressionFromAttributeValue],
#'   or [expressionFromAttributeValues], or more than one concatentated together
#'   and delimited by ' && '. If not supplied, all transcripts will be searched.
#' @param main.participant TRUE to search only main-participant utterances, FALSE to
#'   search all utterances.
#' @param aligned This parameter is deprecated and will be removed in future versions;
#'   please use `anchor.confidence.min = 50` instead.
#' @param matches.per.transcript Optional maximum number of matches per transcript to
#'   return. NULL means all matches.
#' @param words.context Number of words context to include in the 'Before.Match' and
#'   'After.Match' columns in the results.
#' @param max.matches The maximum number of matches to return, or null to return all.
#' @param overlap.threshold The percentage overlap with other utterances before
#'   simultaneous speech is excluded, or null to include overlapping speech.
#' @param anchor.confidence.min The minimum confidence for alignments, e.g.
#'   - *0* - return all alignments, regardless of confidence;
#'   - *50* - return only alignments that have been at least automatically aligned;
#'   - *100* - return only manually-set alignments.
#' @param page.length In order to prevent timeouts when there are a large number of
#'   matches or the network connection is slow, rather than retrieving matches in one
#'   big request, they are retrieved using many smaller requests. This parameter
#'   controls the number of results retrieved per request.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return A data frame identifying matches, containing the following columns:
#'   - *Title* The title of the LaBB-CAT instance
#'   - *Version* The current version of the LaBB-CAT instance
#'   - *SearchName* A name based on the pattern -- the same for all rows
#'   - *MatchId* A unique ID for the matching target token
#'   - *Transcript* Name of the transcript in which the match was found
#'   - *Participant* Name of the speaker
#'   - *Corpus* The corpus of the transcript
#'   - *Line* The start offset of the utterance/line
#'   - *LineEnd* The end offset of the utterance/line
#'   - *Before.Match* Transcript text immediately before the match
#'   - *Text* Transcript text of the match
#'   - *After.Match* Transcript text immediately after the match
#'   - *Number* Row number
#'   - *URL* URL of the first matching word token
#'   - *Target.word* Text of the target word token
#'   - *Target.word.start* Start offset of the target word token
#'   - *Target.word.end* End offset of the target word token
#'   - *Target.segment* Label of the target segment (only present if the segment
#'      layer is included in the pattern)
#'   - *Target.segment.start* Start offset of the target segment (only present if the
#'      segment layer is included in the pattern)
#'   - *Target.segment.end* End offset of the target segment (only present if the
#'      segment layer is included in the pattern)
#' 
#' @seealso
#'   - [getFragments]
#'   - [getSoundFragments]
#'   - [getMatchLabels]
#'   - [getMatchAlignments]
#'   - [processWithPraat]
#'   - [getParticipantIds]
#' 
#' @examples 
#' \dontrun{
#' ## the word 'the' followed immediately by a word starting with an orthographic vowel
#' theThenOrthVowel <- getMatches(labbcat.url, "the [aeiou]")
#'
#' ## a word spelt with "k" but pronounced "n" word initially
#' knWords <- getMatches(labbcat.url, list(orthography = "k.*", phonemes = "n.*"))
#'
#' ## the word 'the' followed immediately by a word starting with an phonemic vowel
#' theThenPhonVowel <- getMatches(
#'   labbcat.url, list(
#'     list(orthography = "the"),
#'     list(phonemes = "[cCEFHiIPqQuUV0123456789~#\\$@].*")))
#' 
#' ## the word 'the' followed immediately or with one intervening word by
#' ## a hapax legomenon (word with a frequency of 1) that doesn't start with a vowel
#' results <- getMatches(
#'   labbcat.url, list(columns = list(
#'     list(layers = list(
#'            orthography = list(pattern = "the")),
#'          adj = 2),
#'     list(layers = list(
#'            phonemes = list(not=TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\$@].*"),
#'            frequency = list(max = "2"))))),
#'   overlap.threshold = 5)
#'
#' ## all tokens of the KIT vowel, from the interview or monologue
#' ## of the participants AP511_MikeThorpe and BR2044_OllyOhlson
#' results <- getMatches(labbcat.url, list(segment="I"),
#'   participant.expression = expressionFromIds(c("AP511_MikeThorpe","BR2044_OllyOhlson")),
#'   transcript.expression = expressionFromTranscriptTypes(c("interview","monologue")))
#' 
#' ## all tokens of the KIT vowel for male speakers who speak English
#' results <- getMatches(labbcat.url, list(segment="I"),
#'   participant.expression = paste(
#'     expressionFromAttributeValue("participant_gender", "M"),
#'     expressionFromAttributeValues("participant_languages_spoken", "en"),
#'     sep=" && "))
#'
#' ## results$Text is the text that matched
#' ## results$MatchId can be used to access results using other functions
#' }
#'
#' @keywords search
#' 
getMatches <- function(labbcat.url, pattern, participant.expression=NULL, transcript.expression=NULL, main.participant=TRUE, aligned=NULL, matches.per.transcript=NULL, words.context=0, max.matches=NULL, overlap.threshold=NULL, anchor.confidence.min=NULL, page.length=1000, no.progress=FALSE) {

    ## if they've explicitly passed a value for aligned
    if (!is.null(aligned)) {
        print("WARNING: the getMatches parameter 'aligned' is deprecated; please use anchor.confidence.min=50 instead")
    } else { ## use previous default value
        aligned = FALSE
        ## if they've just followed the advice above, but their LaBB-CAT version is old
        ## we need to make sure that it still behaves as it did before
        if (!is.null(anchor.confidence.min)) {
            if (anchor.confidence.min >= 50) {
                aligned = TRUE ## (this has no effect on newer versions of LaBB-CAT)
            }
        }
    }
    
    ## first normalize the pattern...
    if (is.character(pattern) && length(pattern) == 1) { # it's a string
        ## assume it's an orthography search
        tokens <- strsplit(pattern," ")
        pattern <- list()
        for (token in tokens[[1]]) {
            pattern[[length(pattern)+1]] <- list(orthography=token)
        } ## next token
    } # it's a string

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
    for (c in 1:length(pattern$columns)) { # for each column
        for (l in names(pattern$columns[[c]]$layers)) { # for each layer in the column
            ## if the layer value isn't a list
            if (!is.list(pattern$columns[[c]]$layers[[l]])) {
                ## wrap a list(pattern=...) around it
                pattern$columns[[c]]$layers[[l]] <-
                    list(pattern = pattern$columns[[c]]$layers[[l]])
            } else if (is.null(names(pattern$columns[[c]]$layers[[l]]))) {
                ## the layer value list is 'nameless' - i.e. multiple matches on same layer
                for (m in 1:length(pattern$columns[[c]]$layers[[l]])) {
                    ## if the layer match isn't a list
                    if (!is.list(pattern$columns[[c]]$layers[[l]][[m]])) {
                        ## wrap a list(pattern=...) around it
                        pattern$columns[[c]]$layers[[l]][[m]] <-
                            list(pattern = pattern$columns[[c]]$layers[[l]][[m]])
                    }
                } # next layer match
            }
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
    if (!is.null(anchor.confidence.min)) {
        parameters$offsetThreshold <- anchor.confidence.min
    }
    if (!is.null(matches.per.transcript)) {
        parameters$matches_per_transcript <- as.list(matches.per.transcript)
    }
    if (!is.null(participant.expression)) {
        if (length(participant.expression) > 1        # it's a list, not a string
            || !grepl("'", participant.expression)) { # or it is a string, but not an expression
            ## for backwards compatibility for when the 3rd parameter was participant.ids
            ## we convert a list of IDs to the appropriate participant expression
            participant.expression <- expressionFromIds(participant.expression)
        } # it's a list, not a string
        parameters$participant_expression <- as.list(participant.expression)
    }
    if (!is.null(transcript.expression)) {
        if (length(transcript.expression) > 1        # it's a list, not a string
            || !grepl("'", transcript.expression)) { # or it is a string, but not an expression
            ## for backwards compatibility for when the 4th parameter was trascript.types
            ## we convert a list of IDs to the appropriate transcript expression
            transcript.expression <- expressionFromTranscriptTypes(transcript.expression)
        } # it's a list, not a string
        parameters$transcript_expression <- as.list(transcript.expression)
    }
    if (!is.null(overlap.threshold)) {
        parameters$overlap_threshold <- overlap.threshold
    }
    
    resp <- http.get(labbcat.url, "api/search", parameters)
    if (is.null(resp)) return()
    deprecatedApi <- FALSE
    if (httr::status_code(resp) == 404) { # server version prior to 20230511.1949
        resp <- http.get(labbcat.url, "search", parameters) # use deprecated endpoint
        if (is.null(resp)) return()
        deprecatedApi <- TRUE
    }
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
            if (thread$size > 0) {
                cat(paste(thread$status, " - fetching data...", "\n", sep=""))
            } else {
                cat(paste(thread$status, "\n", sep=""))
            }
        }
    }

    ## define the dataframe to return (which is, for now, empty)
    allMatches <- data.frame(matrix(ncol = 15, nrow = 0))
    if (thread$size > 0) { ## there were actually some matches
        
        ## ensure labbcat base URL has a trailing slash (for URL reconstruction)
        if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")

        ## layers - "word", and "segment" if mentioned in the pattern
        tokenLayers <- c("word")
        
        ## search results can be very large, and httr timeouts are short and merciless,
        ## so we break the results into chunks and retrieve them using lots of small
        ## requests instead of one big request
        
        totalMatches <- min(thread$size, max.matches) ## (works even if max.matches == NULL)
        matchesLeft <- totalMatches
        pageNumber <- 0

        pb <- NULL
        if (interactive() && !no.progress) {
            pb <- txtProgressBar(min = 0, max = matchesLeft, style = 3)        
        }

        endpoint <- "api/results"
        if (deprecatedApi) endpoint <- "resultsStream" # server version prior to 20230511.1949
        while(matchesLeft > 0) { ## loop until we've got all the matches we want        
            resp <- http.get(labbcat.url,
                             endpoint,
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
            
            if (nrow(allMatches) == 0) { ## if it's the first row
                ## figure out what the target layer is from the first result MatchId
                target.layer <- "word" # default to word
                target.layer_id = gsub(".*#=e.?_([0-9]+)_.*", "\\1", matches$MatchId[[1]])
                if (target.layer_id == "0" || target.layer_id == "2") { ## word or orthography
                    target.layer <- "word"
                } else if (target.layer_id == "1") { # segment
                    target.layer <- "segment"
                } else { ## not an a priori known layer_id, so look it up
                    if (!deprecatedApi) {
                        layer <- getLayer(labbcat.url, target.layer_id)
                        if (!is.null(layer)) {
                            target.layer = layer$id
                        }
                    } 
                }
                if (target.layer != "word") {
                    allMatches <- data.frame(matrix(ncol = 18, nrow = 0))
                    tokenLayers <- c("word", target.layer)
                }
            }
            
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
            if (deprecatedApi) {
                ## reconstruct url
                matches <- cbind(
                    matches, paste(
                                 labbcat.url, "transcript?transcript=",
                                 matches$Transcript, "#",
                                 stringr::str_match(matches$MatchId, "\\[0\\]=([^;]*)(;.*|$)")[,2], sep=""))
            }
            ## Ensure previous default anchor.confidence.min behaviour still works
            ## i.e. if they don't specify a value, then only autmatically aligned offsets
            ## are returned.
            ## If they really want all offsets regardless of confidence, they can call with
            ## anchor.confidence.min=0
            if (is.null(anchor.confidence.min)) {
                anchor.confidence.min = 50
            }
            ## get the alignments
            tokens <- getMatchAlignments(
                labbcat.url, matches$MatchId, tokenLayers,
                anchor.confidence.min=anchor.confidence.min, no.progress=T)
            matches <- cbind(matches, tokens)

            ## add this chunk to the collection
            allMatches <- rbind(allMatches, matches)

            if (!is.null(pb)) {
                setTxtProgressBar(pb, nrow(allMatches))
            }

            ## next page
            pageNumber <- pageNumber + 1
        } ## loop
        ## finished with the progress bar
        if (!is.null(pb)) {
            setTxtProgressBar(pb, nrow(allMatches))
            close(pb)
        }
    } ## there are matches

    if (deprecatedApi) {
        frameNames <- c(
            "SearchName","MatchId","Transcript","Participant","Corpus","Line","LineEnd",
            "Before.Match","Text","After.Match","Number","URL",
            "Target.word","Target.word.start","Target.word.end")
    } else {
        frameNames <- c(
            "SearchName","Transcript","Participant","Corpus","Line","LineEnd",
            "MatchId","URL","Before.Match","Text","After.Match","Number",
            "Target.word","Target.word.start","Target.word.end")
    }
    if (thread$size > 0 && target.layer != "word") {
        frameNames <- c(frameNames,
                        c(paste("Target.",target.layer,sep=""),
                          paste("Target.",target.layer,".start",sep=""),
                          paste("Target.",target.layer,".end",sep="")))
    }
    names(allMatches) = frameNames
    ## ensure pipe-friendly functions like appendLabels don't have to infer URL every time
    attr(allMatches, "labbcat.url") <- labbcat.url
    
    ## free the search thread so it's not using server resources
    thread.release(labbcat.url, threadId)

    return(allMatches)
}
