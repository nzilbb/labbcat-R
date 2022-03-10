#' Gets annotator information.
#' 
#' Retrieve information about an annotator. Annotators are modules that perform different
#' annotation tasks. This function provides information about a given annotator, for
#' example the currently installed version of the module, what configuration parameters it
#' requires, etc.
#'
#' @param labbcat.url URL to the LaBB-CAT instance.
#' @param annotator.id ID of the annotator module.
#' @return The annotator info:
#' \itemize{
#'  \item{\emph{annotatorId} The annotators's unique ID}
#'  \item{\emph{version} The currently install version of the annotator.}
#'  \item{\emph{info} HTML-encoded description of the function of the annotator.}
#'  \item{\emph{infoText} A plain text version of $info (converted automatically).}
#'  \item{\emph{hasConfigWebapp} Determines whether the annotator includes a web-app for
#'     installation or general configuration.}
#'  \item{\emph{configParameterInfo} An HTML-encoded definition of the installation config
#'     parameters, including a list of all parameters, and the encoding of the parameter
#'     string.}
#'  \item{\emph{configParameterInfoText} A plain text version of $configParameterInfo
#'     (converted automatically).} 
#'  \item{\emph{hasTaskWebapp} Determines whether the annotator includes a web-app for
#'     task parameter configuration.}
#'  \item{\emph{taskParameterInfo} An HTML-encoded definition of the task parameters,
#'     including a list of all parameters, and the encoding of the parameter string.}
#'  \item{\emph{taskParameterInfoText} A plain text version of $taskParameterInfo
#'     (converted automatically).} 
#'  \item{\emph{hasExtWebapp} Determines whether the annotator includes an extras web-app
#'     which implements functionality for providing extra data or extending functionality
#'     in an annotator-specific way.}
#'  \item{\emph{extApiInfo} An HTML-encoded document containing information about what
#'     endpoints are published by the ext web-app.}
#'  \item{\emph{extApiInfoText} A plain text version of $extApiInfo
#'     (converted automatically).} 
#' }
#' 
#' @seealso \code{\link{annotatorExt}}
#' \code{\link{newLayer}}
#' @examples
#' \dontrun{
#' ## Get information about the BAS Annotator
#' basAnnotator <- getAnnotatorDescriptor("https://labbcat.canterbury.ac.nz/demo/", "BASAnnotator")
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
