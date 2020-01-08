labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getCorpusIds works", {
    corpora <- getCorpusIds(labbcat.url)
    expect_true(length(corpora) >= 2)
    expect_true("QB" %in% corpora)
    expect_true("UC" %in% corpora)
})
