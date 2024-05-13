#' Renames a list of participants
#'
#' This function changes the IDs of a given set of participants, where possible.
#' 
#' @param labbcat.url URL to the LaBB-CAT instance
#' @param current.ids A vector of participant IDs that as they are currently defined in
#'   the corpus.
#' @param new.ids A vector of new participant IDs, each element corresponding to an ID in
#'   current.ids. 
#' @param no.progress TRUE to supress visual progress bar. Otherwise, progress bar will be
#'     shown when interactive().
#' @return A vector of results, each element corresponding to an ID in current.ids. If the
#'   ID was successfully changed, the corresponding element is TRUE. If the ID could not
#'   be changed (e.g. because there is already an existing participant using the new ID),
#'   then the corresponding element is FALSE.
#' 
#' @seealso 
#'   \code{\link{getParticipantIds}}
#'   \code{\link{getMatchingParticipantIds}}
#'   \code{\link{getParticipant}}
#'   \code{\link{saveParticipant}}
#'   \code{\link{deleteParticipant}}
#' @examples 
#' \dontrun{
#' ## Create some new participant records
#' old.ids <- c("test-id-1","test-id-2","test-id-3")
#' for (id in old.ids) saveParticipant(labbcat.url, id)
#' 
#' ## Batch change the IDs
#' new.ids <- c("test-id-1-changed","test-id-2-changed","test-id-3-changed")
#' renameParticipants(labbcat.url, old.ids, new.ids)
#' 
#' ## Delete the participants we just created
#' for (id in new.ids) deleteParticipant(labbcat.url, id)
#' }
#'
#' @keywords participant
#' 
renameParticipants <- function(labbcat.url, current.ids, new.ids, no.progress = FALSE) {
    renamed <- c()

    if (length(current.ids) != length(new.ids)) {
        print("current.ids and new.ids must be vectors of the same length.")
        return(NULL)
    }
    
    pb <- NULL
    if (interactive() && !no.progress) {
        pb <- txtProgressBar(min = 0, max = length(current.ids), style = 3)        
    }

    p <- 0
    for (current.id in current.ids) {
        p <- p + 1
        if (current.id != new.ids[[p]]) { ## only rename if the two IDs are different
            saved <- saveParticipant(labbcat.url, current.id, new.ids[[p]])
        } else { ## both IDs are the same
            renamed <- TRUE
        }
        renamed <- append(renamed, !is.null(saved))
        if (!is.null(pb)) {
            setTxtProgressBar(pb, p)
        }
    }
    if (!is.null(pb)) close(pb)
    return(renamed)
}
