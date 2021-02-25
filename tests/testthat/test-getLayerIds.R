labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getLayerIds works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    layerIds <- getLayerIds(labbcat.url)
    expect_true(length(layerIds) >= 41)
    expect_true("word" %in% layerIds)
    expect_true("orthography" %in% layerIds)
    expect_true("utterance" %in% layerIds)
    expect_true("turn" %in% layerIds)
    expect_true("participant" %in% layerIds)
})
