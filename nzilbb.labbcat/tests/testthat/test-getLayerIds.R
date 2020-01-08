labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getLayerIds works", {
    layerIds <- getLayerIds(labbcat.url)
    expect_true(length(layerIds) >= 41)
    expect_true("transcript" %in% layerIds)
    expect_true("orthography" %in% layerIds)
    expect_true("utterances" %in% layerIds)
    expect_true("turns" %in% layerIds)
    expect_true("who" %in% layerIds)
})
