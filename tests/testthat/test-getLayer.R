labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getLayer works for orthography", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    layer <- getLayer(labbcat.url, "orthography")
    
    expect_equal(layer$id, "orthography")
    expect_equal(layer$description, "Standard Orthography")
    expect_equal(layer$parentId, "word")
    expect_equal(layer$type, "string")
    expect_equal(layer$alignment, 0)
    expect_false(layer$peers)
    expect_false(layer$peersOverlap)
    expect_true(layer$saturated)
    expect_true(layer$parentIncludes)
})
