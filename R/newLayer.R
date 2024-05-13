#' Creates a new layer
#'
#' This function creates a new annotation layer. The layer may be configured with a layer
#' manager ID and task parameters, for automatic annotation. If so, this function will
#' create the layer and the automation task, but automatic annotation will not be run by
#' this function. To generate the automatic annotations, use \link{generateLayer}.
#'
#' You must have administration privileges in LaBB-CAT in order to be able to use this function.
#'
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param layer.id The ID of the layer to create, which must be unique to the LaBB-CAT instance.
#' @param description A description of the annotations the layer will contain.
#' @param type The type of data the labels will represent. Valid values are "string",
#'     "number", "ipa" (for phoneme representations), or "boolean" (labels "0" or "1").
#' @param alignment How annotations on the layer will relate to time alignment; valid
#'     values are 0 (no alignment; annototations are just tags on the parent annotation),
#'     1 (instants; annotations mark a single point in time), or 2 (intervals; annotations
#'     have a start and end time).
#' @param category The project/category the layer belongs to.
#' @param parent.id The parent layer; valid values are "word" (for word layers), "segment"
#'     (for segment layers) "turn" (for phrase layers), or "transcript" (for span layers).
#' @param annotator.id The ID of the layer manager that automatically fills in
#'     annotations on the layer, if any
#' @param annotator.task.parameters The configuration the layer manager should use when
#'     filling the layer with annotations. This is a string whose format is specific to
#'     each layer manager.
#' @return The resulting layer definition, with members:
#'  - *id* The layer's unique ID
#'  - *parentId* The layer's parent layer ID
#'  - *description* The description of the layer
#'  - *alignment* The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment
#'  - *peers* Whether children have peers or not
#'  - *peersOverlap* Whether child peers can overlap or not
#'  - *parentIncludes* Whether the parent t-includes the child
#'  - *saturated* Whether children must temporally fill the entire parent duration (true) or not (false)
#'  - *parentIncludes* Whether the parent t-includes the child
#'  - *type* The type for labels on this layer
#'  - *validLabels* List of valid label values for this layer
#' 
#' @seealso
#' \code{\link{generateLayer}}
#' \code{\link{saveLayer}}
#' \code{\link{deleteLayer}}
#' @examples
#' \dontrun{
#' ## Upload the CMU Pronouncing Dictionary 
#' loadLexicon(labbcat.url, "cmudict", " - ", "", ";", "Word - Pron", FALSE, "cmudict.txt")
#' 
#' ## Create a layer that tags each token with its CMU Pronouncing Dictionary pronunciation
#' newLayer(labbcat.url, "pronunciation", "CMU Dict pronunciations encoded in ARPAbet",
#'          annotator.id="FlatFileDictionary",
#'          annotator.task.parameters=
#'              "tokenLayerId=orthography&tagLayerId=phonemes&dictionary=cmudict:Word->Pron")
#'
#' ## Generate the pronunciation tags
#' generateLayer(labbcat.url, "pronunciation")
#' }
#' 
#' @keywords layer annotation
#' 
newLayer <- function(labbcat.url, layer.id, description, type="string", alignment=0,
                     category="General", parent.id="word", annotator.id=NULL, 
                     annotator.task.parameters=NULL) {
    
    ## create layer object
    scope = "W"
    if (parent.id == "turn") scope = "M"
    else if (parent.id == "transcript") scope = "F"
    else if (parent.id == "segment") scope = "S"
    layer = list(
        id=layer.id,
        description=description,
        type=type,
        alignment=alignment,
        category=category,
        scope=scope,
        parentId=parent.id,
        parentIncludes=TRUE,
        peers=TRUE,
        peersOverlap=FALSE,
        saturated=(alignment==0),
        layer_manager_id=annotator.id,
        extra=annotator.task.parameters,
        enabled="WTL")
    
    ## make request, sending the object as the JSON-encoded body of the request
    resp <- http.post(labbcat.url, "api/admin/store/newLayer",
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
