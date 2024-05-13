#' Upload a flat lexicon file for lexical tagging
#' 
#' By default LaBB-CAT includes a layer manager called the Flat Lexicon Tagger, which can
#' be configured to annotate words with data from a dictionary loaded from a plain text
#' file (e.g. a CSV file). The file must have a 'flat' structure in the sense that it's a
#' simple list of dictionary entries with a fixed number of columns/fields, rather than
#' having a complex structure.
#'
#' This function uploads such a lexicon file, for use in tagging tokens.
#' 
#' You must have editing privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param file The full path name of the lexicon file.
#' @param lexicon The name for the resulting lexicon. If the named lexicon already exists,
#'   it will be completely replaced with the contents of the file (i.e. all existing
#'   entries will be deleted befor adding new entries from the file).
#'   e.g. 'cmudict'
#' @param field.delimiter The character used to delimit fields in the file.
#'   If this is " - ", rows are split on only the <em>first</em> space, in line with
#'   common dictionary formats.
#'   e.g. ',' for Comma Separated Values (CSV) files.
#' @param field.names A list of field names, delimited by field.delimiter,
#'   e.g. 'Word,Pronunciation'.
#' @param quote The character used to quote field values (if any), e.g. '"'.
#' @param comment The character used to indicate a line is a comment (not an entry) (if any)
#'   e.g. '#'.
#' @param skip.first.line Whether to ignore the first line of the file (because it
#'   contains field names). 
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'   shown when interactive().
#' @return An error message, or NULL if the upload was successful.
#' @keywords lexicon
#' @seealso
#' \code{\link{getDictionaries}}
#' \code{\link{deleteLexicon}}
#' @examples
#' \dontrun{
#' ## Upload the CMU Pronouncing Dictionary 
#' loadLexicon(labbcat.url, "cmudict", " - ", "", ";", "Word - Pron", FALSE, "cmudict.txt")
#' }
#'
loadLexicon <- function(labbcat.url, file, lexicon, field.delimiter, field.names,
                        quote="", comment="", skip.first.line=FALSE, no.progress=FALSE) {
    
    ## make request
    parameters <- list(lexicon=lexicon,
                       fieldDelimiter=field.delimiter,
                       quote=quote,
                       comment=comment,
                       field.names=field.names,
                       skipFirstLine=skip.first.line,
                       file=httr::upload_file(file))
    path = "edit/annotator/ext/FlatLexiconTagger/loadLexicon"
    resp <- http.post.multipart(labbcat.url, path, parameters)
    if (is.null(resp)) return()
    if (httr::status_code(resp) != 200) { # 200 = OK
        return(httr::http_status(resp)$message)
    }
    ## if the request was successful, the response content may be error
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (resp.content != "") {
        return(resp.content)
    }
    ## a blank string means no error.
    ## the annotator is now processing the file, so we follow its progress
    running <- TRUE
    percent.complete <- 0
    status <- "Processing..."
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = 100, style = 3)        
    }
    while (running) {
        Sys.sleep(1)
        resp <- http.get(
            labbcat.url, "edit/annotator/ext/FlatLexiconTagger/getRunning",
            content.type = "text/plain")
        running <- httr::content(resp, as="text", encoding="UTF-8") == "true"
        resp <- http.get(
            labbcat.url, "edit/annotator/ext/FlatLexiconTagger/getStatus",
            content.type = "text/plain")
        status <- httr::content(resp, as="text", encoding="UTF-8")
        resp <- http.get(
            labbcat.url, "edit/annotator/ext/FlatLexiconTagger/getPercentComplete",
            content.type = "text/plain")
        percent.complete <- httr::content(resp, as="text", encoding="UTF-8")
        if (!is.null(pb)) {
            setTxtProgressBar(pb, strtoi(percent.complete))
        }
    } # poll until finished
    if (!is.null(pb)) {
        close(pb)
    }

    if (percent.complete == "100") { # got to the end       
        return(NULL) # assume this means success
    } else { # didn't get to the end
        ## presumably this is a failure, hopefully the last status is informative
        return(paste("ERROR:",status))
    }
}
