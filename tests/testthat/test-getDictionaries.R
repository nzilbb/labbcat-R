labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getDictionaries works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    dictionaries <- getDictionaries(labbcat.url)

    expect_true("CELEX-EN" %in% names(dictionaries))
    expect_true("Phonology (wordform)" %in% dictionaries$`CELEX-EN`)
})
