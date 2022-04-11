#' Adds an entry to a layer dictionary.
#'
#' This function adds a new entry to the dictionary that manages a given layer,
#' and updates all affected tokens in the corpus. Words can have multiple entries.
#'
#' You must have edit privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param layer.id The ID of the layer with a dictionary configured to manage it.
#' @param key The key (word) in the dictionary to add an entry for.
#' @param entry The value (definition) for the given key.
#' @return NULL if the entry was added, or a list of error messages if not.
#' 
#' @seealso
#' \code{\link{generateLayer}}
#' @examples
#' \dontrun{
#' ## Add a pronunciation for the word "robert" to the phonemes layer dictionary
#' addLayerDictionaryEntry(labbcat.url, "phonemes", "robert", "'rQ-b@t")
#' }
#' 
#' @keywords layer annotation
#' 
addLayerDictionaryEntry <- function(labbcat.url, layer.id, key, entry) {

    ## make request, sending the object as the JSON-encoded body of the request
    resp <- http.post(labbcat.url, "api/edit/dictionary/add",
                      list(layerId=layer.id, key=key, entry=entry))
    
    ## check the reponse
    if (is.null(resp)) return()
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 400) { # 400 = Bad request
            ## the content should be valid JSON with informative errors
            resp.json <- jsonlite::fromJSON(resp.content)
            for (error in resp.json$errors) print(paste("ERROR:", error))
        } else {
            print(paste("ERROR:", httr::http_status(resp)$message))
            print(resp.content)
        }
        return(NULL)
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    if (length(resp.json$errors) > 0) {
        return (resp.json$errors)
    } else {
        return(NULL)
    }
}
