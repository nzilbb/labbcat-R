labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatchingTranscriptIds works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getMatchingTranscriptIds(labbcat.url, "id MATCHES 'BR.+'")
    expect_equal(length(ids), 6)
    expect_true("BR946_RodgerCurragh.eaf" %in% ids)
    expect_false("QB247_Jacqui.eaf" %in% ids)
})
