#' Sets the username and password for a given LaBB-CAT server.
#' 
#' Sets the username and password that the package should use for connecting
#' to a given LaBB-CAT server in future function calls.
#'
#' If you are using R interactively, this step is optional, as all functions will prompt
#' the user for the username and password if required. If the script is running in
#' RStudio, then the RStudio password input dialog is used, hiding the credentials from view.
#' Otherwise, the console is used, and credentials are visible.
#'
#' The recommended approach is to \strong{not} use labbcatCredentials, to avoid saving
#' user credentials in script files that may eventually become visible to other.
#' Use labbcatCredentials \strong{only} in cases where the script execution is unsupervised,
#' e.g. if you are executing an R script from a shell script, or using Knit to render an
#' Rmarkdown document.
#'
#' If you must use labbcatCredentials, avoid including the actual username and password in
#' your script. The recommended approach is to store the username and password (and
#' perhaps the URL too) in your \file{.Renviron} file (in your home directory, or the
#' porject directory), like this:
#'
#' ```
#' LABBCAT_URL=https://labbcat.canterbury.ac.nz/demo/
#' LABBCAT_USERNAME=demo
#' LABBCAT_PASSWORD=demo
#' ```
#'
#' And then call Sys.getenv to retrieve the
#' username/password, as illustrated in the example.
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
#' ## load the LaBB-CAT URL from .Renviron
#' labbcat.url <- Sys.getenv('LABBCAT_URL')
#' 
#' ## load the username/password from .Renviron so secrets are not included in the script:
#' labbcatCredentials(
#'     labbcat.url, Sys.getenv('LABBCAT_USERNAME'), Sys.getenv('LABBCAT_PASSWORD'))
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
