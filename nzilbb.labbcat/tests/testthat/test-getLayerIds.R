labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getLayerIds works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    layerIds <- getLayerIds(labbcat.url)
    expect_true(length(layerIds) >= 41)
    expect_true("transcript" %in% layerIds)
    expect_true("orthography" %in% layerIds)
    expect_true("utterances" %in% layerIds)
    expect_true("turns" %in% layerIds)
    expect_true("who" %in% layerIds)
})
