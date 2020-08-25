labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getLayer works for orthography", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    layers <- getLayer(labbcat.url, "orthography")
    
    expect_equal(layers$id, "orthography")
    expect_equal(layers$description, "Standard Orthography")
    expect_equal(layers$parentId, "transcript")
    expect_equal(layers$type, "string")
    expect_equal(layers$alignment, 0)
    expect_false(layers$peers)
    expect_false(layers$peersOverlap)
    expect_true(layers$saturated)
    expect_true(layers$parentIncludes)
})
