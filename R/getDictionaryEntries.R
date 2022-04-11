#' Lookup entries in a dictionary.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param manager.id The layer manager ID of the dictionary, as returned by getDictionaries
#' @param dictionary.id The ID of the dictionary, as returned by getDictionaries
#' @param keys A list of keys (words) identifying entries to look up
#' @return A data frame with the keys and their dictionary entries, if any.
#' 
#' @seealso \link{getDictionaries}
#' @examples 
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#'
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
    write.table(keys, upload.file, sep=",", row.names=FALSE, col.names=FALSE)

    ## make request
    parameters <- list(managerId=manager.id, dictionaryId=dictionary.id, uploadfile=httr::upload_file(upload.file))    
    resp <- http.post.multipart(labbcat.url, "dictionary", parameters, download.file)

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

    ## ensure we use the correct number of columns
    ncol <- max(count.fields(download.file, sep=",", quote="\""))
    
    ## load the returned entries
    entries <- read.csv(
        download.file, header=F, col.names = paste0("V", seq_len(ncol)), blank.lines.skip=F)

    ## rename the columns so that the one containing the keys is called "key"
    colnames(entries) <- c("key", head(colnames(entries), length(colnames(entries)) - 1))

    ## tidily remove the downloaded file
    file.remove(download.file)
    
    return(entries)
}
