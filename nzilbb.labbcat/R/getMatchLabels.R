#' Gets labels of annotations on a given layer, identified by given match IDs.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A vector of annotation IDs, e.g. the MatchId column of a results set.
#' @param layerId One or more layer names.
#' @param targetOffset The distance from the original target of the match, e.g.
#' \itemize{
#'  \item{\emph{0} -- find annotations of the match target itself},
#'  \item{\emph{1} -- find annotations of the token immediately \emph{after} match target}
#'  \item{\emph{-1} -- find annotations of the token immediately \emph{before} match target}
#' }
#' @param annotationsPerLayer The number of annotations on the given layer to
#'     retrieve. In most cases, there's only one annotation available. However, tokens
#'     may, for example, be annotated with `all possible phonemic transcriptions', in which
#'     case using a value of greater than 1 for this parameter provides other phonemic
#'     transcriptions, for tokens that have more than one.
#' @return A data frame of labels.
#' 
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## Load some search results previously exported from LaBB-CAT
#' results <- read.csv("results.csv", header=T)
#'
#' ## Get the topic annotations for the matches
#' topics <- getAnnotationLabels(labbcat.url, results$MatchId, "topic")
#' }
#' 
#' @keywords layer annotation label
#' 
getMatchLabels <- function(labbcat.url, matchIds, layerIds, targetOffset=0, annotationsPerLayer=1) {    
    ## validate layer Ids
    for (layerId in layerIds) {
        layer <- getLayer(labbcat.url, layerId)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
    } # next layer
    
    ## save keys to a CSV file
    upload.file = tempfile(pattern="matcheIds.", fileext=".csv")
    download.file = tempfile(pattern="labels.", fileext=".csv")
    write.table(matchIds, upload.file, sep=",", row.names=FALSE, col.names=TRUE)

    ## make request
    layerIds <- paste(layerIds,collapse="\n")
    parameters <- list(
        layerIds=layerIds,
        targetOffset=targetOffset, annotationsPerLayer=annotationsPerLayer,
        csvFieldDelimiter=",", columnMapping=TRUE, copyColumns=FALSE,
        plus="Token.plus.", minus="Token.minus.",
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
    labels <- read.csv(download.file, header=T)

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(labels)
}
