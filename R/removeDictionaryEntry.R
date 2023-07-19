#' Removes an entry from a dictionary.
#'
#' This function removes an existing entry from the given dictionary.
#'
#' You must have edit privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param manager.id The layer manager ID of the dictionary, as returned by getDictionaries
#' @param dictionary.id The ID of the dictionary, as returned by getDictionaries
#' @param key The key (word) in the dictionary to remove an entry for.
#' @param entry The value (definition) for the given key, or NULL to remove all entries
#'     for the key.
#' @return NULL if the entry was removed, or a list of error messages if not.
#' 
#' @seealso \link{getDictionaries}
#' @seealso \link{getDictionaryEntries}
#' @examples
#' \dontrun{
#' ## Remove a pronuciation of the word "robert" from the CELEX wordform pronunciation dictionary
#' removeDictionaryEntry(labbcat.url, "CELEX-EN", "Phonology (wordform)", "robert", "'rQ-bErt")
#' }
#' 
#' @keywords layer annotation
#' 
removeDictionaryEntry <- function(labbcat.url, manager.id, dictionary.id, key, entry=NULL) {

    ## make request, sending the object as the JSON-encoded body of the request
    resp <- http.post(
        labbcat.url, "api/edit/dictionary/remove",
        list(layerManagerId=manager.id, dictionaryId=dictionary.id, key=key, entry=entry))
    
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
