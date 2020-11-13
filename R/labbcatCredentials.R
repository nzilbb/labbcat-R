#' Sets the username and password that the package should use for connecting
#' to a given LaBB-CAT server in future function calls.
#'
#' This step is optional, as all functions will prompt the user for the username
#' and password if required.  If the script is running in RStudio, then the
#' RStudio password input dialog is used, hiding the credentials from view.
#' Otherwise, the console is used, and credentials are visible.
#'
#' The recommended approach is to *not* use labbcatCredentials, to avoid saving
#' user credentials in script files that may eventually become visible to other.
#' Use labbcatCredentials *only* in cases where the script execution is unsupervised.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param username The LaBB-CAT username, if it is password-protected
#' @param password The LaBB-CAT password, if it is password-protected
#' @return NULL if the username/password are correct, and a string describing the problem
#'     if a problem occurred, e.g. "Credentials rejected" if the username/password are
#'     incorrect, or a string starting "Version mismatch" if the server's version of
#'     LaBB-CAT is lower than the minimum required.
#' @examples
#' \dontrun{
#' ## define the LaBB-CAT URL
#' labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"
#' 
#' ## specify the username/password in the script
#' ## (only use labbcatCredentials for scripts that must execute unsupervised!)
#' labbcatCredentials(labbcat.url, "demo", "demo")
#' }
#'
#' @keywords connect username password timeout
#' 
labbcatCredentials <- function(labbcat.url, username, password) {
    ## ensure labbcat base URL has a trailing slash
    if (!grepl("/$", labbcat.url)) labbcat.url <- paste(labbcat.url, "/", sep="")
    
    version.check.url <- paste(labbcat.url, "store?call=", sep="")
    authorization <- httr::authenticate(username, password)
    tryCatch(expr={
        resp <- httr::GET(version.check.url,
                          authorization,
                          httr::add_headers("User-Agent" = .user.agent),
                          httr::timeout(getOption("nzilbb.labbcat.timeout", default=10)))
        
        if (httr::status_code(resp) != 200) { # 200 = OK
            if (httr::status_code(resp) == 401) {
                return("Credentials rejected")
            } else {
                return(httr::http_status(resp)$message)
            }
        } ## not 200 OK
        
        ## do a second request
        ## - this seems to be required for credentials to 'take' in non-interactive mode
        resp <- httr::GET(version.check.url,
                          authorization,
                          httr::add_headers("User-Agent" = .user.agent),
                          httr::timeout(getOption("nzilbb.labbcat.timeout", default=10)))
        
        ## check the LaBB-CAT version
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        resp.json <- jsonlite::fromJSON(resp.content)
        version <- resp.json$model$version
        if (is.null(version) || version < .min.labbcat.version) {
            return(paste("Version mismatch: ", labbcat.url, "is version", version, "but the minimum version is", .min.labbcat.version))
        }
        return(NULL)
    },
    error=function(e){
        return(e$message)
    })
}
