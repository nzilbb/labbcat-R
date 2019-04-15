#' \packageTitle{nzilbb.labbcat}
#'
#' \packageDescription{nzilbb.labbcat}
#'
#' \packageDESCRIPTION{nzilbb.labbcat}
#' \packageIndices{nzilbb.labbcat}
#' 
#' 'LaBB-CAT' is a web-based language corpus management system and this
#' package provides access to data stored in a 'LaBB-CAT' instance.
#' You must have at least version 20190412.1154 of 'LaBB-CAT' to use
#' this package.
#' 
#' As 'LaBB-CAT' instances are usually password-protected, the function
#' \code{labbcat.instance} must be used first of all to create an object
#' that provides access to a specific instance of 'LaBB-CAT' with specific
#' credentials.  This object is then used as the first argument for all
#' other function calls.
#' 
#' @docType package
#' @keywords package
#' @name nzilbb.labbcat
#' @author \packageAuthor{nzilbb.labbcat}
#' @references
#' \cite{Robert Fromont and Jennifer Hay, "{ONZE Miner}: the development of a browser-based research tool", 2008}
#' \cite{Robert Fromont, "Toward a format-neutral annotation store", 2017}
#' @examples
#' ## connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the 5 seconds starting from 10s as a mono 22kHz file
#' wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
#' 
NULL

### Internal variables:

## minimum version of LaBB-CAT required:
.min.labbcat.version <- "20190412.1154"

## encode a parameter value for inclusion in the URL
enc <- function(value) {
    return(stringr::str_replace_all(URLencode(value),"\\+","%2B"))
}

## build a store call URL 
buildUrl <- function(labbcat, call, parameters = NULL) {
    url <- paste(labbcat$storeUrl, call, sep="")
    if (!is.null(parameters)) {
        for (name in names(parameters)) {
            url <- paste(url, "&", name, "=", parameters[name], sep="")
        } # next parameter
    } # there are parameters
    url <- enc(url)
    return(url)
}

## Export functions:

#' Connects to the given 'LaBB-CAT' instance, and returns and object that
#' must be used for all other functions.
#'
#' If a username and password are not passed, and the 'LaBB-CAT' instance
#' is password-protected (and the function is called in interactive
#' mode), then the user will be prompted for the username and
#' password. This is the recommended method for accessing
#' password-protected 'LaBB-CAT' instances, in order to avoid saving
#' passwords in script files. The username and password parameters are
#' provided for cases where the script is not run in interactive mode.
#' 
#' @param url URL to the LaBB-CAT instance
#' @param username The LaBB-CAT username, if it is password-protected
#' @param password The LaBB-CAT password, if it is password-protected
#' @param timeout Maximum time for any LaBB-CAT request
#' @return An object that can be passed as the labbcat parameter for
#'     other functions in this package 
#' @examples
#' ## connect to an open or password-protected instance of LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/")
#'
#' ## connect to a password-protected instance of LaBB-CAT with explicit credentials
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/",
#'      username="demo", password="demo")
#'
#' @keywords connect username password timeout
#' 
labbcat.instance <- function(url, username = NULL, password = NULL, timeout = 10) {
    baseUrl <- url
    ## ensure baseUrl has a trailing slash
    if (!grepl("/$", url)) baseUrl <- paste(url, "/", sep="")
    
    storeUrl <- paste(baseUrl, "store?call=", sep="")
    
    if (is.null(username)) {
        authorization <- NULL
        resp <- httr::GET(storeUrl, httr::timeout(timeout))
    } else {  
        authorization <- httr::authenticate(username, password)
        resp <- httr::GET(storeUrl, authorization, httr::timeout(timeout))
    }
    if (httr::status_code(resp) != 200) { # 200 = OK
        if (httr::status_code(resp) == 401 && is.null(username) && is.null(password)) {
            ## it's password-protected, but they haven't provided credentials
            ## so ask them for the username and password
            return(labbcat.instance(
                url, readline("LaBB-CAT Username: "), readline("LaBB-CAT Password: ")))
        } else {
            print(paste("ERROR: ", httr::http_status(resp)$message))
            return(NULL)
        }
    } else { ## respons was OK
        ## check the LaBB-CAT version
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        resp.json <- jsonlite::fromJSON(resp.content)
        version <- resp.json$model$version
        if (is.null(version) || version < .min.labbcat.version) {
            print(paste("ERROR:", baseUrl, "is version", version, "but the minimum version is", .min.labbcat.version))
            return(NULL)
        } else { ## everything OK
            return (list(
                baseUrl = baseUrl,
                version = version,
                storeUrl = storeUrl,
                timeout = timeout,
                authorization = authorization
            ))
        }
    }
}

#' Gets the store's ID.
#' 
#' The store's ID - i.e. the ID of the 'LaBB-CAT' instance.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return The annotation store's ID
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get ID of LaBB-CAT instance
#' instance.id <- labbcat.getId(labbcat)
#'
labbcat.getId <- function(labbcat) {
    url <- buildUrl(labbcat, "getId")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of layer IDs.
#'
#' Layer IDs are annotation 'types'.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of layer IDs
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get names of all layers
#' layer.ids <- labbcat.getLayerIds(labbcat)
#' 
#' @keywords layer
#' 
labbcat.getLayerIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getLayerIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of layer definitions.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of layer definitions, with members:
#' \enumerate{
#'  \item{id The layer's unique ID}
#'  \item{parentId The layer's parent layer ID}
#'  \item{description The description of the layer}
#'  \item{alignment The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment}
#'  \item{peers Whether children have peers or not}
#'  \item{peersOverlap Whether child peers can overlap or not}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{saturated Whether children must temporally fill the entire parent duration (true) or not (false)}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{type The type for labels on this layer}
#'  \item{validLabels List of valid label values for this layer}
#' }
#' 
#' @seealso \code{\link{labbcat.getLayerIds}}
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get definitions of all layers
#' layers <- labbcat.getLayerIds(labbcat)
#' 
#' @keywords layer
#' 
labbcat.getLayers <- function(labbcat) {
    url <- buildUrl(labbcat, "getLayers")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a layer definition.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id ID of the layer to get the definition for
#' @return The definition of the given layer, with members:
#' \enumerate{
#'  \item{id The layer's unique ID}
#'  \item{parentId The layer's parent layer ID}
#'  \item{description The description of the layer}
#'  \item{alignment The layer's alignment - 0 for none, 1 for point alignment, 2 for interval alignment}
#'  \item{peers Whether children have peers or not}
#'  \item{peersOverlap Whether child peers can overlap or not}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{saturated Whether children must temporally fill the entire parent duration (true) or not (false)}
#'  \item{parentIncludes Whether the parent t-includes the child}
#'  \item{type The type for labels on this layer}
#'  \item{validLabels List of valid label values for this layer}
#' }
#' 
#' @seealso \code{\link{labbcat.getLayerIds}}
#' \code{\link{labbcat.getLayers}}
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the definition of the orthography layer
#' orthography.layer <- labbcat.getLayer(labbcat, "orthography")
#'
#' @keywords layer
#' 
labbcat.getLayer <- function(labbcat, id) {
    url <- buildUrl(labbcat, "getLayer", list(id=id))
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of corpus IDs.
#' 
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of corpus IDs
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List corpora
#' corpora <- labbcat.getCorpusIds(labbcat)
#' 
#' @keywords corpora
#' 
labbcat.getCorpusIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getCorpusIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' List the predefined media tracks available for transcripts.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of media track definitions.
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the media tracks configured in LaBB-CAT
#' tracks <- labbcat.getMediaTracks(labbcat)
#' 
#' @keywords media sound
#' 
labbcat.getMediaTracks <- function(labbcat) {
    url <- buildUrl(labbcat, "getMediaTracks")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of participant IDs.
#'
#' Returns a list of participant IDs.
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of participant IDs
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List all speakers
#' speakers <- labbcat.getParticipantIds(labbcat)
#' 
#' @keywords speaker participant
#' 
labbcat.getParticipantIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getParticipantIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of graph IDs.
#'
#' Returns a list of graph IDs (i.e. transcript names).
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @return A list of graph IDs
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List all transcripts
#' transcripts <- labbcat.getGraphIds(labbcat)
#' 
#' @keywords graph transcript
#' 
labbcat.getGraphIds <- function(labbcat) {
    url <- buildUrl(labbcat, "getGraphIds")
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of corpus IDs.
#'
#' Returns a list of corpora in the given 'LaBB-CAT' instance.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id The ID (name) of the corpus
#' @return A list of corpus IDs
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List corpora
#' corpora <- labbcat.getCorpusIds(labbcat)
#' 
#' @keywords corpora corpus
#' 
labbcat.getGraphIdsInCorpus <- function(labbcat, id) {
    url <- buildUrl(labbcat, "getGraphIdsInCorpus", list(id=id))
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of IDs of graphs that include the given participant.
#'
#' Returns a list of IDs of graphs (i.e. transcript names) that include
#' the given participant.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A participant ID
#' @return A list of graph IDs
#' 
#' @seealso \code{\link{labbcat.getParticipantIds}}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List transcripts in which UC427_ViktoriaPapp_A_ENG speaks
#' transcripts <- labbcat.getGraphIdsWithParticipant(labbcat, "UC427_ViktoriaPapp_A_ENG")
#' 
#' @keywords graph transcript
#' 
labbcat.getGraphIdsWithParticipant <- function(labbcat, id) {
    url <- buildUrl(labbcat, "getGraphIdsWithParticipant", list(id=id))
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a list of IDs of graphs that match a particular pattern.
#'
#' Gets a list of IDs of graphs (i.e. transcript names) that match a
#' particular pattern.
#'
#' The results can be exhaustive, by omitting pageLength and
#' pageNumber, or they  can be a subset (a 'page') of results, by
#' given pageLength and pageNumber values.
#'
#' The order of the list can be specified.  If ommitted, the graphs
#' are listed in ID order.
#'
#' The expression language is currently not well defined, but
#' expressions such as those in the examples can be used.
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param expression An expression that determines which graphs match
#' @param pageLength The maximum number of IDs to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @param order An expression that determines the order the graphs are
#' listed in
#' @return A list of graph IDs (i.e. transcript names)
#' 
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get all transcripts whose names start with "BR"
#' transcripts <- labbcat.getMatchingGraphIdsPage(labbcat, "id MATCHES 'BR.+'")
#' 
#' ## Get the first twenty transcripts in the "QB" corpus
#' transcripts <- labbcat.getMatchingGraphIdsPage(
#'         labbcat, "my('corpus').label = 'QB'", 20, 0)
#' 
#' ## Get the second transcript that has "QB247_Jacqui" as a speaker
#' transcripts <- labbcat.getMatchingGraphIdsPage(
#'         labbcat, "'QB247_Jacqui' IN labels('who')", 1, 1)
#' 
#' ## Get all transcripts whose names start with "BR" and have "QB247_Jacqui" as a speaker,
#' ## in word-count order 
#' transcripts <- labbcat.getMatchingGraphIdsPage(
#'         labbcat, "my('corpus').label = 'QB' AND 'QB247_Jacqui' IN labels('who')", 1, 1,
#'         "my('transcript_word_count').label")
#' 
#' @keywords graph transcript expression
#' 
labbcat.getMatchingGraphIdsPage <- function(labbcat, expression, pageLength = NULL, pageNumber = NULL, order = NULL) {
    parameters <- list(expression=expression)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    if (!is.null(order)) parameters <- append(parameters, list(order=order))
    url <- buildUrl(labbcat, "getMatchingGraphIdsPage", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets the number of annotations on the given layer of the given graph.
#'
#' Returns the number of annotations on the given layer of the given
#' graph (transcript).
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A graph ID (i.e. transcript name)
#' @param layerId A layer name
#' @return The number of annotations on that layer
#' 
#' @seealso
#' \code{\link{labbcat.getGraphIds}}
#' \code{\link{labbcat.getGraphIdsInCorpus}}
#' \code{\link{labbcat.getGraphIdsWithParticipant}}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Count the number of words in UC427_ViktoriaPapp_A_ENG.eaf
#' token.count <- labbcat.countAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' @keywords graph transcript
#' 
labbcat.countAnnotations <- function(labbcat, id, layerId) {
    parameters <- list(id=id, layerId=layerId)
    url <- buildUrl(labbcat, "countAnnotations", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets the annotations on the given layer of the given graph.
#'
#' Returns the annotations on the given layer of the given graph
#' (transcript).
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A graph ID (i.e. transcript name)
#' @param layerId A layer name
#' @param pageLength The maximum number of annotations to return, or null to return all
#' @param pageNumber The zero-based page number to return, or null to return the first page
#' @return A named list of annotations, with members:
#' \enumerate{
#'  \item{id The annotation's unique ID}
#'  \item{layerId The name of the layer it comes from}
#'  \item{label The value of the annotation}
#'  \item{startId The ID of the start anchor},
#'  \item{endId The ID of the end anchor},
#'  \item{parentId The ID of the parent annotation},
#'  \item{ordinal The ordinal of the annotation among its peers},
#'  \item{confidence A rating from 0-100 of the confidence of the label
#'  e.g. 10: default value, 50: automatically generated, 100: manually annotated}
#' }
#' 
#' @seealso 
#'   \code{\link{labbcat.getGraphIds}}
#'   \code{\link{labbcat.getGraphIdsInCorpus}}
#'   \code{\link{labbcat.getGraphIdsWithParticipant}}
#'   \code{\link{labbcat.countAnnotations}}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get all the orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- labbcat.getAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- labbcat.getAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#'
#' @keywords graph transcript
#' 
labbcat.getAnnotations <- function(labbcat, id, layerId, pageLength = NULL, pageNumber = NULL) {
    parameters <- list(id=id, layerId=layerId)
    if (!is.null(pageLength)) parameters <- append(parameters, list(pageLength=pageLength))
    if (!is.null(pageNumber)) parameters <- append(parameters, list(pageNumber=pageNumber))
    url <- buildUrl(labbcat, "getAnnotations", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets the given anchors in the given graph.
#'
#' Lists the given anchors in the given graph (transcript).
#' 
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A graph ID (i.e. transcript name)
#' @param anchorId A vector of anchor IDs (or a string representing one anchor ID)
#' @return  A named list of anchors, with members:
#' \enumerate{
#'  \item{id The annotation's unique ID},
#'  \item{offset The offset from the beginning (in seconds if it's a
#'  transcript of a recording, or in characters if it's a text document)}
#'  \item{confidence A rating from 0-100 of the confidence of the offset,
#'   e.g. 10: default value, 50: force-aligned, 100: manually aligned}
#' }
#' 
#' @seealso \link{labbcat.getAnnotations}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
#' orthography <- labbcat.getAnnotations(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
#' 
#' ## Get the start anchors the above tokens
#' word.starts <- labbcat.getAnchors(labbcat, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
#' 
#' @keywords anchor
#' 
labbcat.getAnchors <- function(labbcat, id, anchorId) {
    parameters <- list(id=id)
    for (id in anchorId) parameters <- append(parameters, list(anchorId=id))
    url <- buildUrl(labbcat, "getAnchors", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' List the media available for the given graph.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A graph ID (i.e. transcript name)
#' @return A named list of media files available for the given graph, with members:
#' \enumerate{
#'  \item{trackSuffix The track suffix of the media}
#'  \item{mimeType The MIME type of the file}
#'  \item{url URL to the content of the file}
#'  \item{name Name of the file}
#' }
#' 
#' @seealso \link{labbcat.getGraphIds}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## List the media files available for BR2044_OllyOhlson.eaf
#' media <- labbcat.getAvailableMedia(labbcat, "BR2044_OllyOhlson.eaf")
#' 
#' @keywords media audio
#' 
labbcat.getAvailableMedia <- function(labbcat, id) {
    parameters <- list(id=id)
    url <- buildUrl(labbcat, "getAvailableMedia", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a given media track for a given graph.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a call to labbcat.instance
#' @param id A graph ID (i.e. transcript name)
#' @param trackSuffix The track suffix of the media
#' @param mimeType The MIME type of the media
#' @return A URL to the given media for the given graph
#' @seealso \link{labbcat.getGraphIds}
#' @examples 
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get URL for the WAV file for BR2044_OllyOhlson.eaf
#' media <- labbcat.getMedia(labbcat, "BR2044_OllyOhlson.eaf")
#' 
#' ## Get URL for the 'QuakeFace' video file for BR2044_OllyOhlson.eaf
#' media <- labbcat.getMedia(labbcat, "BR2044_OllyOhlson.eaf", "_face", "video/mp4")
#' 
#' @keywords media audio
#' 
labbcat.getMedia <- function(labbcat, id, trackSuffix = "", mimeType = "audio/wav") {
    parameters <- list(id=id, trackSuffix=trackSuffix, mimeType=mimeType)
    url <- buildUrl(labbcat, "getMedia", parameters)
    resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
    resp.content <- httr::content(resp, as="text", encoding="UTF-8")
    if (httr::status_code(resp) != 200) { # 200 = OK
        print(paste("ERROR: ", httr::http_status(resp)$message))
        print(resp.content)
        return()
    }
    resp.json <- jsonlite::fromJSON(resp.content)
    return(resp.json$model$result)
}

#' Gets a sound fragment from 'LaBB-CAT'.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a
#'     call to labbcat.instance 
#' @param id The graph ID (transcript name) of the sound recording, or
#'     a vector of graph IDs. 
#' @param start The start time in seconds, or a vector of start times.
#' @param end The end time in seconds, or a vector of end times.
#' @param sampleRate Optional sample rate in Hz - if a positive
#'     integer, then the result is a mono file with the given sample rate.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
#' @return The name of the file, which is saved in the current
#'     directory, or a list of names of files, if multiple
#'     id's/start's/end's were specified 
#'
#' If a list of files is returned, they are in the order that they
#'     were returned by the server, which *should* be the order that
#'     they were specified in the id/start/end lists.
#' 
#' @examples
#' \dontrun{
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get the 5 seconds starting from 10s after the beginning of a recording
#' wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0)
#' 
#' ## Get the 5 seconds starting from 10s as a mono 22kHz file
#' wav.file <- labbcat.getSoundFragment(labbcat, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
#' 
#' ## Get a list of fragments
#' results <- data.frame(
#'              id=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
#'              start=c(10.0, 20.0, 30.0),
#'              end=c(15.0, 25.0, 35.0))
#' wav.files <- labbcat.getSoundFragment(labbcat, results$id, results$start, results$end)
#' 
#' ## Get a list of fragments with no prgress bar
#' wav.file <- labbcat.getSoundFragment(
#'               labbcat, results$id, results$start, results$end, no.progress=TRUE)
#' }
#' @keywords sample sound fragment wav
#' 
labbcat.getSoundFragment <- function(labbcat, id, start, end, sampleRate = NULL, no.progress=FALSE) {
    if (length(id) == 1) { ## one fragment
        dir <- ""
    } else { ## multiple fragments
        ## save fragments into their own directory
        dir <- "fragments"
        if (file.exists(dir)) {
            ## ensure it's a new directory by adding a number
            n <- 1
            new.dir = paste(dir,"(",n,")", sep="")
            while (file.exists(new.dir)) {
                n <- n + 1
                new.dir = paste(dir,"(",n,")", sep="")
            } # next try
            dir <- new.dir
        }
        dir.create(dir)
        ## add trailing slash
        dir <- paste(dir, .Platform$file.sep, sep="")
    }

    pb <- NULL
    if (!no.progress && length(id) > 1) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)        
    }

    ## loop throug each triple, getting fragments individually
    ## (we could actually pass the lot to LaBB-CAT in one go and get a ZIP file back
    ##  but then we can't be sure the results contain a row for every fragment specified
    ##  and we can't display a progress bar)
    file.names = c()
    r <- 1
    for (graph.id in id) {
        url <- paste(labbcat$baseUrl, "soundfragment", sep="")
        parameters <- list(id=graph.id, start=start[r], end=end[r])
        if (!is.null(sampleRate)) parameters <- list(id=graph.id, start=start[r], end=end[r], sampleRate=sampleRate)
        file.name <- paste(dir, stringr::str_replace(graph.id, "\\.[^.]+$",""), "__", start[r], "-", end[r], ".wav", sep="")

        tryCatch({
            resp <- httr::POST(url, labbcat$authorization,
                               httr::write_disk(file.name, overwrite=TRUE),
                               httr::timeout(labbcat$timeout),
                               body = parameters, encode = "form")
            if (httr::status_code(resp) != 200) { # 200 = OK
                print(paste("ERROR: ", httr::http_status(resp)$message))
                if (httr::status_code(resp) != 404) { # 404 means the audio wasn't on the server
                    ## some other error occurred so print what we got from the server
                    print(readLines(file.name))
                }
                file.remove(file.name)
                file.name <<- NULL
            } else {
                content.disposition <- as.character(httr::headers(resp)["content-disposition"])
                content.disposition.parts <- strsplit(content.disposition, "=")
                if (length(content.disposition.parts[[1]]) > 1
                    && file.name != content.disposition.parts[[1]][2]) {
                    ## file name is specified, so use it
                    final.file.name <- paste(dir, content.disposition.parts[[1]][2], sep="")
                    file.rename(file.name, final.file.name)
                    file.name <- final.file.name
                }
            }
        }, error = function(e) {
            print(paste("ERROR:", e))
            file.name <<- NULL
        })
        file.names <- append(file.names, file.name)
        
        if (!is.null(pb)) setTxtProgressBar(pb, r)
        r <- r+1
    } ## next row
    if (!is.null(pb)) close(pb)
    return(file.names)   
}

#' Gets labels of annotations on a given layer, identified by given annotation IDs.
#'
#' @param labbcat A LaBB-CAT instance object previously created by a
#'     call to labbcat.instance 
#' @param id A vector of annotation IDs. 
#' @param layerId A layer name.
#' @param count The number of annotations on the given layer to retrieve.
#' @param no.progress Optionally suppress the progress bar when
#'     multiple fragments are  specified - TRUE for no progress bar.
#' @return A data frame of labels.
#' 
#' @examples
#' ## Connect to LaBB-CAT
#' labbcat <- labbcat.instance("https://labbcat.canterbury.ac.nz/demo/", "demo", "demo")
#' 
#' ## Get a list of topics
#' results <- data.frame(
#'              id=c("AP513_Steve.eaf", "AP513_Steve.eaf", "AP513_Steve.eaf"),
#'              MatchId=c("ew_0_7260", "ew_0_7487", "ew_0_7704"))
#' topics <- labbcat.getLabels(labbcat, results$MatchId, "topic")
#' 
#' ## Get a list of fragments with no prgress bar
#' wav.file <- labbcat.getSoundFragment(
#'               labbcat, results$id, results$start, results$end, no.progress=TRUE)
#' @keywords sample sound fragment wav
#' 
labbcat.getLabels <- function(labbcat, id, layerId, count=1, no.progress=FALSE) {

    pb <- NULL
    if (!no.progress && length(id) > 1) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)        
    }

    ## we need a vector of size 'count' to store vectors of labels
    labels = c()
    for (col in 1:count) labels <- append(labels, c())
    
    ## loop through each id, getting fragments individually
    r <- 1
    for (annotation.id in id) {
        ## if the ID is actually a URL or MatchId, pick out the ew_0_n+ part
        annotation.id <- stringr::str_match(annotation.id, "ew_0_[0-9]+")
        ## TODO can't necessarily assume that annotation.id is on the transcript layer
        expression = paste("my('transcript').id = '", annotation.id, "' AND layer.id = '",layerId,"'", sep="")
        parameters <- list(expression=expression, pageLength=count, pageNumber=0)
        url <- buildUrl(labbcat, "getMatchingAnnotations", parameters)
        ## create default row
        row <- c()
        ## fill it with NA
        for (col in 1:count) row <- append(row, NA)

        resp <- httr::GET(url, labbcat$authorization, httr::timeout(labbcat$timeout))
        resp.content <- httr::content(resp, as="text", encoding="UTF-8")
        if (httr::status_code(resp) != 200) { # 200 = OK
            print(paste("ERROR: ", httr::http_status(resp)$message))
        } else {
            resp.json <- jsonlite::fromJSON(resp.content)
            if (length(resp.json$model$result) > 0) {
                ## populate the row
                for (col in 1:count) {
                    row[col] <- resp.json$model$result$label[col]
                }
            }
        }
        labels <- append(labels, row)
        
        if (!is.null(pb)) setTxtProgressBar(pb, r)
        r <- r+1
    } ## next row
    if (!is.null(pb)) close(pb)
    cols <- c()
    for (col in 1:count) {
        if (count == 1) {
            col.name <- layerId
        } else {
            col.name <- paste(layerId,".",col,sep="")
        }
        cols <- append(cols, col.name)
    }
    labels.matrix <- matrix(labels, ncol=count, byrow=TRUE)
    colnames(labels.matrix) <- cols
    labels.df <- as.data.frame(labels.matrix, col.names=cols)

    return(labels.df)
}
