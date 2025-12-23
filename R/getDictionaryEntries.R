#' Lookup entries in a dictionary
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param manager.id The layer manager ID of the dictionary, as returned by getDictionaries
#' @param dictionary.id The ID of the dictionary, as returned by getDictionaries
#' @param keys A list of keys (words) identifying entries to look up
#' @return A data frame with the keys and their dictionary entries, if any.
#' 
#' @family dictionary functions
#' @examples 
#' \dontrun{
#' keys <- c("the", "quick", "brown", "fox")
#' 
#' ## get the pronunciations according to CELEX
#' entries <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)", keys)
#' }
#' 
#' @keywords dictionary
#' 
getDictionaryEntries <- function(labbcat.url, manager.id, dictionary.id, keys) {
    ## save keys to a CSV file
    upload.file = "keys.csv"
    download.file = "entries.csv"
    write.table(keys, upload.file, sep=",", row.names=FALSE, col.names=TRUE)

    ## make request
    parameters <- list(managerId=manager.id, dictionaryId=dictionary.id, uploadfile=httr::upload_file(upload.file))    
    resp <- http.post.multipart(labbcat.url, "api/dictionary", parameters, download.file)
    if (httr::status_code(resp) == 404) { # endpoint not there, fall back to old endpoint
        resp <- http.post.multipart(labbcat.url, "dictionary", parameters, download.file)
    }

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
    ## the file has a header row, but the number of headers might not match all rows
    ## so first we load with the headers as if they're rows
    entries <- read.csv(download.file, header=F, blank.lines.skip=F, row.names=NULL)
    ## then we strip off the first row
    entries <- entries[-1, ]

    ## rename the columns so that the one containing the keys is called "key"
    numValueColumns <- ncol(entries) - 1
    if (numValueColumns < 1) numValueColumns <- 1
    colnames(entries) <- c("key", paste0("V", seq_len(numValueColumns)))
    
    ## tidily remove the downloaded file
    file.remove(download.file)
    return(entries)
}
