#' Saves the details of an existing layer.
#'
#' This function saves the definition of an existing annotation layer. 
#'
#' You must have administration privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param layer A named list object representing the layer attributes, as would be
#'     returned by \link{getLayer} or \link{newLayer}, with members:
#' \itemize{
#'  \item{\emph{id} The layer's unique ID}
#'  \item{\emph{parentId} The layer's parent layer ID}
#'  \item{\emph{description} The description of the layer}
#'  \item{\emph{alignment} The layer's alignment - 0 for none, 1 for point alignment, 2
#'     for interval alignment} 
#'  \item{\emph{peers} Whether children have peers or not}
#'  \item{\emph{peersOverlap} Whether child peers can overlap or not}
#'  \item{\emph{parentIncludes} Whether the parent t-includes the child}
#'  \item{\emph{saturated} Whether children must temporally fill the entire parent
#'     duration (true) or not (false)} 
#'  \item{\emph{parentIncludes} Whether the parent t-includes the child}
#'  \item{\emph{type} The type for labels on this layer}
#'  \item{\emph{validLabels} List of valid label values for this layer}
#' }
#' @return The resulting layer definition, with members:
#' \itemize{
#'  \item{\emph{id} The layer's unique ID}
#'  \item{\emph{parentId} The layer's parent layer ID}
#'  \item{\emph{description} The description of the layer}
#'  \item{\emph{alignment} The layer's alignment - 0 for none, 1 for point alignment, 2
#'     for interval alignment} 
#'  \item{\emph{peers} Whether children have peers or not}
#'  \item{\emph{peersOverlap} Whether child peers can overlap or not}
#'  \item{\emph{parentIncludes} Whether the parent t-includes the child}
#'  \item{\emph{saturated} Whether children must temporally fill the entire parent
#'     duration (true) or not (false)} 
#'  \item{\emph{parentIncludes} Whether the parent t-includes the child}
#'  \item{\emph{type} The type for labels on this layer}
#'  \item{\emph{validLabels} List of valid label values for this layer}
#' }
#' 
#' @seealso
#' \code{\link{newLayer}}
#' \code{\link{getLayer}}
#' \code{\link{deleteLayer}}
#' @examples
#' \dontrun{
#' ## Get the pronunciation layer definition
#' pronunciation <- getLayer(labbcat.url, "pronunciation")
#' 
#' ## Change some details of the definition
#' pronunciation$description <- "CMU Dict pronunciations encoded in DISC"
#' pronunciation$type <- "ipa"
#'
#' ## Save the changes to the layer definition
#' saveLayer(labbcat.url, pronunciation)
#' }
#' 
#' @keywords layer annotation
#' 
saveLayer <- function(labbcat.url, layer) {

    ## make request, sending the object as the JSON-encoded body of the request
    resp <- http.post(labbcat.url, "api/admin/store/saveLayer",
                      jsonlite::toJSON(layer, auto_unbox=TRUE, null='null', na='null'))
    
    ## check the reponse
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
        return(NULL)
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    for (error in resp.json$errors) print(error)
    return(resp.json$model)
}
