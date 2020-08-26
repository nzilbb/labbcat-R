labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getCorpusIds works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    corpora <- getCorpusIds(labbcat.url)
    expect_true(length(corpora) >= 2)
    expect_true("QB" %in% corpora)
    expect_true("UC" %in% corpora)
})
