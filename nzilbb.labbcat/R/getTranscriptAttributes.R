#' Gets transcript attribute values for given transcript IDs.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcriptIds A vector of transcript IDs
#' @param layerIds A vector of layer IDs corresponding to transcript attributes. In
#'     general, these are layers whose ID is prefixed 'transcript_', however formally it's
#'     any layer where layer$parentId == 'graph' && layer$alignment == 0, which includes
#'     'corpus' as well as transcript attribute layers.
#' @return A data frame of attribute value labels.
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Get language, duration, and corpus for transcripts starting with 'BR'
#' attributes <- getTranscriptAttributes(labbcat.url,
#'             getMatchingTranscriptIds(labbcat.url, "id MATCHES 'BR.+'"),
#'             c('transcript_language', 'transcript_duration', 'corpus'))
#' 
#' }
#' 
#' @keywords layer annotation label
#' 
getTranscriptAttributes <- function(labbcat.url, transcriptIds, layerIds) {    
    ## validate layer Ids
    for (layerId in layerIds) {
        layer <- getLayer(labbcat.url, layerId)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
        ## check it's an attribute
        if (layer$parentId != 'graph' || layer$alignment != 0) {
            print(paste("ERROR:", layerId, ' is not a transcript attribute'))
            return()
        }
    } # next layer
    
    ## save labels to a CSV file
    download.file = tempfile(pattern="transcript-attributes.", fileext=".csv")

    ## add 'graph' layer so that results can be matched with transcript IDs
    layerIds <- c('graph', layerIds)

    ## flatten lists into single newine-delimited strings
    ## (because httr can't handle multiple parameters with the same name)
    layerIds <- paste(layerIds,collapse="\n")
    transcriptIds <- paste(transcriptIds,collapse="\n")

    ## make request
    parameters <- list(
        todo='export', exportType='csv', 
        layers=layerIds,
        ids=transcriptIds)
    resp <- http.post(labbcat.url, "transcripts", parameters, download.file)
    
    ## check the reponse
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }

    ## load the returned entries
    attributes <- read.csv(download.file, header=T)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(attributes)
}
