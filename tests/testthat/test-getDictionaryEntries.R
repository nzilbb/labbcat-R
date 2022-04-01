labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getDictionaryEntries works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    entries <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)",
g                                    c("the", "quick", "brown", "fox"))

    expect_equal(length(entries), 5)
    expect_equal(names(entries), c("key", "V1", "V2", "V3", "V4"))
})
