#' Gets labels of annotations on a given layer, identified by given match IDs
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param match.ids A vector of annotation IDs, e.g. the MatchId column, or the URL column,
#'    of a results set. 
#' @param layer.ids A vector of layer IDs.
#' @param target.offset The distance from the original target of the match, e.g.
#'   - *0* -- find annotations of the match target itself
#'   - *1* -- find annotations of the token immediately *after* match target
#'   - *-1* -- find annotations of the token immediately *before* match target
#' @param annotations.per.layer The number of annotations on the given layer to
#'   retrieve. In most cases, there's only one annotation available. However, tokens
#'   may, for example, be annotated with `all possible phonemic transcriptions', in which
#'   case using a value of greater than 1 for this parameter provides other phonemic
#'   transcriptions, for tokens that have more than one.
#' @param include.match.ids Whether or not the data frame returned includes the original
#'   MatchId column or not.
#' @param page.length In order to prevent timeouts when there are a large number of
#'   matches or the network connection is slow, rather than retrieving matches in one
#'   big request, they are retrieved using many smaller requests. This parameter
#'   controls the number of results retrieved per request.
#' @param no.progress TRUE to suppress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return A data frame of labels.
#' 
#' @seealso
#'   - [getMatches]
#'   - [getMatchAlignments]
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(orthography="quake"))
#' 
#' ## Get the topic annotations for the matches
#' topics <- getMatchLabels(labbcat.url, results$MatchId, "topic")
#' }
#' 
#' @keywords layer annotation label
#' 
getMatchLabels <- function(labbcat.url, match.ids, layer.ids, target.offset=0, annotations.per.layer=1, include.match.ids = FALSE, page.length=1000, no.progress=FALSE) {    
    ## validate layer Ids
    for (layerId in layer.ids) {
        layer <- getLayer(labbcat.url, layerId)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
    } # next layer
    
    ## save keys to a CSV file
    upload.file = tempfile(pattern="matcheIds.", fileext=".csv")
    download.file = tempfile(pattern="labels.", fileext=".csv")

    allLabels <- NULL
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = length(match.ids), style = 3)
    }

    ## the API is now api/results[/upload] but used to be api/getMatchAnnotations
    ## we might need to fall back to the old endpoint
    deprecatedApi <- F

    ## break match.ids into manageable chunks
    matchIdChunks <- split(match.ids, ceiling(seq_along(match.ids)/page.length))
    for (MatchId in matchIdChunks) {
        write.table(
            data.frame(MatchId), upload.file, sep=",", row.names=FALSE, col.names=TRUE)
        
        ## make request
        layer.ids <- paste(layer.ids,collapse="\n")
        if (!deprecatedApi) {
            parameters <- list(
                csvFieldDelimiter=",", targetColumn="MatchId", "content-type"="text/csv",
                results=httr::upload_file(upload.file))
            resp <- http.post.multipart(
                labbcat.url, "api/results/upload", parameters, download.file)
            if (httr::status_code(resp) != 404) { ## endpoint exists
                resp.json <- jsonlite::fromJSON(
                                           httr::content(resp, as="text", encoding="UTF-8"))
                for (error in resp.json$errors) print(error)
                ## we get a task ID back
                threadId <- resp.json$model$threadId
                ## wait for task until it finishes (should be immediate)
                thread <- thread.get(labbcat.url, threadId)
                if (is.null(thread)) return()
                while (thread$running) {
                    Sys.sleep(1)
                    thread <- thread.get(labbcat.url, threadId)
                    if (is.null(thread)) return()
                } # poll until finished
                ## export fields
                parameters <- list(
                    threadId=threadId,
                    csv_layers=layer.ids,
                    offsetThreshold="none",
                    targetOffset=target.offset,
                    annotationsPerLayer=annotations.per.layer,
                    csvFieldDelimiter=",", 
                    "content-type"="text/csv",
                    plus="Token.plus.", minus="Token.minus.")
                resp <- http.post(
                    labbcat.url, "api/results", parameters, download.file)
                
                ## free the upload thread so it's not using server resources
                thread.release(labbcat.url, threadId)
            } else { ## endpoint not found
                deprecatedApi <- T ## fall through to next block
            }
        } ## can fall through to below        
        if (deprecatedApi) {
            parameters <- list(
                layerIds=layer.ids,
                targetOffset=target.offset, annotationsPerLayer=annotations.per.layer,
                csvFieldDelimiter=",", targetColumn=0, copyColumns=include.match.ids,
                "content-type"="text/csv",
                plus="Token.plus.", minus="Token.minus.",
                uploadfile=httr::upload_file(upload.file))
            resp <- http.post.multipart(
                labbcat.url, "api/getMatchAnnotations", parameters, download.file)
        }
        
        ## tidily remove upload file
        file.remove(upload.file)

        ## check the reponse
        if (is.null(resp)) return()
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
            print(resp.content)
            return()
        }

        ## load the returned entries
        labels <- read.csv(download.file, header=T, blank.lines.skip=F, na.strings="")
        if (!include.match.ids && !deprecatedApi) {
            ## the entries include a MatchId column, so remove it
            labels <- subset(labels, select=-MatchId)
        }
        
        ## tidily remove the downloaded file
        file.remove(download.file)

        ## append results to culumative data frame
        allLabels <- rbind(allLabels, labels)
        if (!is.null(pb)) {
            setTxtProgressBar(pb, nrow(allLabels))
        }
    } ## next chunk
    if (!is.null(pb)) { ## if there was a progress bar, 
        close(pb)
    }    
    return(allLabels)
}
