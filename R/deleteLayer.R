#' Deletes an existing layer.
#'
#' This function deletes an existing annotation layer, including all annotation data
#' associated with it.
#'
#' You must have administration privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param layer.id The ID of the layer to delete.
#' @return NULL, or an error message if deletion failed.
#' 
#' @seealso
#' \code{\link{newLayer}}
#' \code{\link{saveLayer}}
#' @examples
#' \dontrun{
#' ## Delete the phonemes layer
#' deleteLayer(labbcat.url, "phonemes")
#' }
#' 
#' @keywords layer annotation
#' 
deleteLayer <- function(labbcat.url, layer.id) {

    if (layer.id == "transcript" || layer.id == "word" || layer.id == "utterance"
        || layer.id == "turn" || layer.id == "participant") {
        return(paste("ERROR: cannot delete layer: ", layer.id))
    }
    
    resp <- http.post(labbcat.url, "api/admin/store/deleteLayer", list(id=layer.id))
    
    ## check the reponse
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 400) { # 400 = Bad request
            ## the content should be valid JSON with informative errors
            resp.json <- jsonlite::fromJSON(resp.content)
            return(resp.json$errors)
        } else {
            return(httr::http_status(resp)$message)
        }
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    if (length(resp.json$errors)) return(resp.json$errors)
    return(NULL)
}
