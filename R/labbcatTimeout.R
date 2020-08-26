#' Sets the timeout for request to the LaBB-CAT server in future function calls. The
#' default timeout is 10 seconds.
#'
#' @param seconds The number of seconds before requests return with a timeout error.
#' @return The request timeout in seconds
#' @examples
#' \dontrun{
#' ## the request timeout
#' labbcatTimeout(30)
#' }
#'
#' @keywords connect username password timeout
#' 
labbcatTimeout <- function(seconds=NULL) {
    if (!is.null(seconds)) { 
        options(nzilbb.labbcat.timeout=seconds)
    }
    return(getOption("nzilbb.labbcat.timeout", default=10))
}

