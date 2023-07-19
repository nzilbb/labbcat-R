labbcat.url <- "http://localhost:8080/labbcat"

test_that("participant CRUD functions work", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "labbcat", "labbcat"))) skip("Server not available")

    ## Create some new participant records
    old.ids <- c("test-id-1","test-id-2","test-id-3")
    for (id in old.ids) {
        result <- saveParticipant(labbcat.url, id)
        expect_true(result[[1]])
        ## ensure ID exists
        expect_false(is.null(getParticipant(labbcat.url, id, list())))
    }

    ## Batch change the IDs
    new.ids <- c("test-id-1-changed","test-id-2-changed","test-id-3-changed")
    for (id in new.ids) {
        ## ensure ID doesn't exist yet
        expect_true(is.null(getParticipant(labbcat.url, id, list())))
    }
    renameParticipants(labbcat.url, old.ids, new.ids)
    for (id in new.ids) {
        ## ensure new ID exists
        expect_false(is.null(getParticipant(labbcat.url, id, list())))
    }
    for (id in old.ids) {
        ## ensure old ID doesn't exist any more
        expect_true(is.null(getParticipant(labbcat.url, id, list())))
    }
    
    ## Delete the participants we just created
    for (id in new.ids) {
        deleteParticipant(labbcat.url, id)
        ## ensure ID doesn't exist any more
        expect_true(is.null(getParticipant(labbcat.url, id, list())))
    }
})
