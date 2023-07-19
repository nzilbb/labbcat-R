labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getDictionaryEntries works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    entries <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)",
                                    c("the", "quick", "brown", "fox"))

    expect_equal(length(entries), 5)
    expect_equal(names(entries), c("key", "V1", "V2", "V3", "V4"))
})
