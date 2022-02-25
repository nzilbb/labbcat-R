#' Delete a previously loaded lexicon.
#' 
#' By default LaBB-CAT includes a layer manager called the Flat Lexicon Tagger, which can
#' be configured to annotate words with data from a dictionary loaded from a plain text
#' file (e.g. a CSV file). 
#'
#' This function deletes such a lexicon, which was previously added using loadLexicon.
#'
#' You must have editing privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param lexicon The name of the lexicon to delete, e.g. 'cmudict'
#' @return An error message, or NULL if the upload was successful.
#' @keywords lexicon
#' @seealso
#' \code{\link{loadLexicon}}
#' @examples
#' \dontrun{
#' ## Delete the previously loaded CMU Pronouncing Dictionary lexicon
#' deleteLexicon(labbcat.url, "cmudict")
#' }
#'
deleteLexicon <- function(labbcat.url, lexicon) {
    
    ## make request
    path = paste("edit/annotator/ext/FlatLexiconTagger/deleteLexicon?",lexicon,sep="")
    resp <- http.get(labbcat.url, path)
    if (is.null(resp)) return()
    if (httr::status_code(resp) != 200) { # 200 = OK
        return(httr::http_status(resp)$message)
    }
    ## if the request was successful, the response content may be error
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (resp.content != "") {
        return(resp.content)
    } else {
        return(NULL)
    }
}
