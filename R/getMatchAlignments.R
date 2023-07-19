#' Gets temporal alignments of matches on a given layer.
#' 
#' Gets labels and start/end offsets of annotations on a given layer, identified by given
#' match IDs.
#'
#' You can specify a threshold for confidence in the alignment, which is a value from 0
#' (not aligned) to 100 (manually aligned). The default is 50 (automatically aligned), so
#' only alignments that have been at least automatically aligned are specified. For cases
#' where there's a token but its alignment confidence falls below the threshold, a label
#' is returned, but the start/end times are NA.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param match.ids A vector of annotation IDs, e.g. the MatchId column, or the URL column,
#'     of a results set. 
#' @param layer.ids A vector of layer IDs.
#' @param target.offset The distance from the original target of the match, e.g.
#' \itemize{
#'  \item{\emph{0} -- find annotations of the match target itself},
#'  \item{\emph{1} -- find annotations of the token immediately \emph{after} match target}
#'  \item{\emph{-1} -- find annotations of the token immediately \emph{before} match target}
#' }
#' @param annotations.per.layer The number of annotations on the given layer to
#'     retrieve. In most cases, there's only one annotation available. However, tokens
#'     may, for example, be annotated with `all possible phonemic transcriptions', in which
#'     case using a value of greater than 1 for this parameter provides other phonemic
#'     transcriptions, for tokens that have more than one.
#' @param anchor.confidence.min The minimum confidence for alignments, e.g.
#' \itemize{
#'  \item{\emph{0} -- return all alignments, regardless of confidence;}
#'  \item{\emph{50} -- return only alignments that have been at least automatically aligned;}
#'  \item{\emph{100} -- return only manually-set alignments.}
#' }
#' @param include.match.ids Whether or not the data frame returned includes the original
#'     MatchId column or not.
#' @param page.length In order to prevent timeouts when there are a large number of
#'     matches or the network connection is slow, rather than retrieving matches in one
#'     big request, they are retrieved using many smaller requests. This parameter
#'     controls the number of results retrieved per request.
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'     shown when interactive().
#' @return A data frame with label, start time, and end time, for each layer.
#' 
#' @seealso
#' \code{\link{getMatches}}
#' \code{\link{getMatchLabels}}
#' @examples
#' \dontrun{
#' ## Perform a search
#' results <- getMatches(labbcat.url, list(segment="I"))
#' 
#' ## Get the segment following the token, with alignment if it's been manually aligned
#' following.segment <- getMatchAlignments(labbcat.url, results$MatchId, "segment",
#'     target.offset=1, anchor.confidence.min=100)
#' }
#' 
#' @keywords layer annotation label
#' 
getMatchAlignments <- function(labbcat.url, match.ids, layer.ids, target.offset=0,
                               annotations.per.layer=1, anchor.confidence.min=50,
                               include.match.ids=FALSE, page.length=1000, no.progress=FALSE) {    
    ## validate layer Ids
    for (layerId in layer.ids) {
        layer <- getLayer(labbcat.url, layerId)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
    } # next layer
    
    ## save keys to a CSV file
    upload.file = tempfile(pattern="matcheIds.", fileext=".csv")
    download.file = tempfile(pattern="labels.", fileext=".csv")
    
    ## break match.ids into manageable chunks
    allLabels <- NULL
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = length(match.ids), style = 3)
    }

    matchIdChunks <- split(match.ids, ceiling(seq_along(match.ids)/page.length))
    for (match.ids in matchIdChunks) {
        write.table(match.ids, upload.file, sep=",", row.names=FALSE, col.names=TRUE)
        
        ## make request
        layer.ids <- paste(layer.ids,collapse="\n")
        parameters <- list(
            layerIds=layer.ids,
            targetOffset=target.offset, annotationsPerLayer=annotations.per.layer,
            csvFieldDelimiter=",", targetColumn=0, copyColumns=include.match.ids,
            "content-type"="text/csv",
            plus="Token.plus.", minus="Token.minus.",
            offsetThreshold=anchor.confidence.min,
            uploadfile=httr::upload_file(upload.file))
        resp <- http.post.multipart(labbcat.url, "api/getMatchAnnotations", parameters, download.file)
        
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
