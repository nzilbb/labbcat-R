#' Gets annotator information
#' 
#' Retrieve information about an annotator. Annotators are modules that perform different
#' annotation tasks. This function provides information about a given annotator, for
#' example the currently installed version of the module, what configuration parameters it
#' requires, etc.
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param annotator.id ID of the annotator module.
#' @return The annotator info:
#'  - *annotatorId* The annotators's unique ID
#'  - *version* The currently install version of the annotator.
#'  - *info* HTML-encoded description of the function of the annotator.
#'  - *infoText* A plain text version of $info (converted automatically).
#'  - *hasConfigWebapp* Determines whether the annotator includes a web-app for
#'     installation or general configuration.
#'  - *configParameterInfo* An HTML-encoded definition of the installation config
#'     parameters, including a list of all parameters, and the encoding of the parameter
#'     string.
#'  - *configParameterInfoText* A plain text version of $configParameterInfo
#'     (converted automatically).
#'  - *hasTaskWebapp* Determines whether the annotator includes a web-app for
#'     task parameter configuration.
#'  - *taskParameterInfo} An HTML-encoded definition of the task parameters,
#'     including a list of all parameters, and the encoding of the parameter string.*
#'  - *taskParameterInfoText* A plain text version of $taskParameterInfo
#'     (converted automatically). 
#'  - *hasExtWebapp* Determines whether the annotator includes an extras web-app
#'     which implements functionality for providing extra data or extending functionality
#'     in an annotator-specific way.
#'  - *extApiInfo* An HTML-encoded document containing information about what
#'     endpoints are published by the ext web-app.
#'  - *extApiInfoText* A plain text version of $extApiInfo
#'     (converted automatically).
#' 
#' @seealso \code{\link{annotatorExt}}
#' \code{\link{newLayer}}
#' @examples
#' \dontrun{
#' ## Get information about the BAS Annotator
#' basAnnotator <- getAnnotatorDescriptor("https://labbcat.canterbury.ac.nz/demo/", "BASAnnotator")
#' cat(basAnnotator$infoText)
#' }
#'
#' @keywords annotator layer
#' 
getAnnotatorDescriptor <- function(labbcat.url, annotator.id) {
    resp <- store.get(labbcat.url, "getAnnotatorDescriptor", list(annotatorId=annotator.id))
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
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    
    descriptor <- resp.json$model
    
    ## format HTML elements as plain text for convenience
    pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
    if (!is.null(descriptor[["info"]])) {
        descriptor$infoText = html.to.text(descriptor)
    }
    if (!is.null(descriptor[["configParameterInfo"]])) {
        descriptor$configParameterInfoText = html.to.text(descriptor$configParameterInfo)
    }
    if (!is.null(descriptor[["taskParameterInfo"]])) {
        descriptor$taskParameterInfoText = html.to.text(descriptor$taskParameterInfo)
    }
    if (!is.null(descriptor[["extApiInfo"]])) {
        descriptor$extApiInfoText = html.to.text(descriptor$extApiInfo)
    }
    return(descriptor)
}
