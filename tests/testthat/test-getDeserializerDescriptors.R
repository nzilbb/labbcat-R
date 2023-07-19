labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getDeserializerDescriptors works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    formats <- getDeserializerDescriptors(labbcat.url)
    expect_true(length(formats$name) > 0)
    expect_true("text/praat-textgrid" %in% formats$mimeType)
})
