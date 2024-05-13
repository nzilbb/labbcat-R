#' Retrieve annotator's "ext" resource
#' 
#' Retrieve a given resource from an annotator's "ext" web app. Annotators are modules
#' that perform different annotation tasks, and can optionally implement functionality for
#' providing extra data or extending functionality in an annotator-specific way. If the
#' annotator implements an "ext" web app, it can provide resources and implement a
#' mechanism for iterrogating the annotator. This function provides a mechanism for
#' accessing these resources via R.
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param annotator.id ID of the annotator to interrogate.
#' @param resource The name of the file to retrieve or instance method (function) to
#'   invoke. Possible values for this depend on the specific annotator being interrogated.
#' @param parameters Optional list of ordered parameters for the instance method (function).
#' @return The resource requested.
#' @examples
#' \dontrun{
#' ## Get the version of the currently installed LabelMapper annotator:
#' annotatorExt(labbcat.url, "LabelMapper", "getVersion")
#' 
#' ## Get the summary of the segment to speakerDependentPhone mapping
#' ## implemented by the LabelMapper annotator:
#' summaryJson <- annotatorExt(labbcat.url,
#'               "LabelMapper", "summarizeMapping", list("segment","speakerDependentPhone"))
#' summary <- jsonlite::fromJSON(summaryJson)
#' }
#'
annotatorExt <- function(labbcat.url, annotator.id, resource, parameters=NULL) {
    queryString = ""
    if (!is.null(parameters)) {
        queryString=paste("?", enc(paste(parameters,collapse=",")), sep="")
    }
    path = paste("edit/annotator/ext/",annotator.id,"/",resource,queryString, sep="")
    resp <- http.get(labbcat.url, path, content.type = "text/plain")
    if (is.null(resp)) return()
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(httr::content(resp, as="text", encoding="UTF-8"))
        return()
    }
    return(httr::content(resp, as="text", encoding="UTF-8"))
}

