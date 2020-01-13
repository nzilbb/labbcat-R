#' Gets labels of annotations on a given layer, identified by given annotation IDs.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param id A vector of annotation IDs, e.g. the MatchId column of a results set.
#' @param layerId One or more layer names.
#' @param count The number of annotations on the given layer to retrieve.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
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
getAnnotationLabels <- function(labbcat.url, id, layerId, count=1, no.progress=FALSE) {

    ## validate layer Ids
    for (l in layerId) {
        layer <- getLayer(labbcat.url, l)
        ## getLayer prints an error if the layerId isn't valid
        if (is.null(layer)) return()
    } # next layer
    
    pb <- NULL
    if (!no.progress && length(id) > 1) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)        
    }

    ## set column names
    cols <- c()
    for (l in layerId) {
        for (col in 1:count) {
            if (count == 1) {
                col.name <- l
            } else {
                col.name <- paste(l,".",col,sep="")
            }
            cols <- append(cols, col.name)
        } # next colum
    } # next layer

    ## create a matrix of NA, which we update in-situ
    numLayers <- length(layerId)
    labels.matrix <- matrix(nrow=length(id), ncol=numLayers*count, byrow=TRUE)
    colnames(labels.matrix) <- cols

    ## loop through each id, getting fragments individually
    r <- 1
    for (annotation.id in id) {

        ## if the ID is actually a URL or MatchId, pick out the ew_0_n+ part
        annotation.id <- stringr::str_match(annotation.id, "ew_0_[0-9]+")

        colOffset <- 0
        ## for each layerId specified...
        for (l in layerId) {
            ## TODO can't necessarily assume that annotation.id is on the transcript layer
            expression = paste("my('transcript').id = '", annotation.id, "' AND layer.id = '",l,"'", sep="")
            parameters <- list(expression=expression, pageLength=count, pageNumber=0)
            resp <- store.get(labbcat.url, "getMatchingAnnotations", parameters)
            if (is.null(resp)) return()
        
            resp.content <- httr::content(resp, as="text", encoding="UTF-8")
            if (httr::status_code(resp) != 200) { # 200 = OK
                print(paste("ERROR: ", httr::http_status(resp)$message))
            } else {
                resp.json <- jsonlite::fromJSON(resp.content)
                for (error in resp.json$errors) print(error)
                if (length(resp.json$model$result) > 0) {
                    ## populate the row
                    for (col in 1:count) {
                        labels.matrix[r, colOffset+col] <- resp.json$model$result$label[col]
                    }
                }
            }
            colOffset <- colOffset + count
        } ## next layerId
        
        if (!is.null(pb)) setTxtProgressBar(pb, r)
        r <- r+1
    } ## next row
    if (!is.null(pb)) close(pb)

    labels.df <- as.data.frame(labels.matrix, col.names=cols)
    
    return(labels.df)
}
