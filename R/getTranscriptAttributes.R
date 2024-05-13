#' Gets transcript attribute values for given transcript IDs
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param transcript.ids A vector of transcript IDs
#' @param layer.ids A vector of layer IDs corresponding to transcript attributes. In
#'   general, these are layers whose ID is prefixed 'transcript_', however formally it's
#'   any layer where layer$parentId == 'transcript' && layer$alignment == 0, which includes
#'   'corpus' as well as transcript attribute layers.
#' @return A data frame of attribute value labels.
#' 
#' @examples
#' \dontrun{
#' ## Get language, duration, and corpus for transcripts starting with 'BR'
#' attributes <- getTranscriptAttributes(labbcat.url,
#'             getMatchingTranscriptIds(labbcat.url, "/'BR.+'/.test(id)"),
#'             c('transcript_language', 'transcript_duration', 'corpus'))
#' 
#' }
#' 
#' @keywords layer annotation label
#' 
getTranscriptAttributes <- function(labbcat.url, transcript.ids, layer.ids) {    
    ## validate layer Ids
    for (layerId in layer.ids) {
        layer <- getLayer(labbcat.url, layerId)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
        ## check it's an attribute
        if (layer$parentId != 'transcript' || layer$alignment != 0) {
            print(paste("ERROR:", layerId, ' is not a transcript attribute'))
            return()
        }
    } # next layer
    
    ## save labels to a CSV file
    download.file = tempfile(pattern="transcript-attributes.", fileext=".csv")

    ## add 'transcript' layer so that results can be matched with transcript IDs
    layer.ids <- c('transcript', layer.ids)

    ## flatten lists into single newine-delimited strings
    ## (because httr can't handle multiple parameters with the same name)
    layer.ids <- paste(layer.ids,collapse="\n")
    transcript.ids <- paste(transcript.ids,collapse="\n")

    ## make request
    parameters <- list(
        layers=layer.ids,
        ids=transcript.ids)
    resp <- http.post(labbcat.url, "api/attributes", parameters, download.file)
    
    ## check the reponse
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }

    ## load the returned entries
    attributes <- read.csv(download.file, header=T, blank.lines.skip=F)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(attributes)
}
