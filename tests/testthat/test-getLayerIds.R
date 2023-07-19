labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getLayerIds works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    layerIds <- getLayerIds(labbcat.url)
    expect_true(length(layerIds) >= 41)
    expect_true("word" %in% layerIds)
    expect_true("orthography" %in% layerIds)
    expect_true("utterance" %in% layerIds)
    expect_true("turn" %in% layerIds)
    expect_true("participant" %in% layerIds)
})
