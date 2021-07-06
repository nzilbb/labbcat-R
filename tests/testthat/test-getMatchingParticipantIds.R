labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatchingParticipantIds works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getMatchingParticipantIds(labbcat.url, "/^BR.+/.test(id)")
    expect_equal(length(ids), 5)
    expect_true("BR946_RodgerCurragh" %in% ids)
    expect_false("QB247_Jacqui" %in% ids)
})
