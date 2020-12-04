labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getLayers works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    layers <- getLayers(labbcat.url)
    
    expect_true(length(layers$id) >= 41)
    expect_true(length(layers$description) >= 41)
    expect_true(length(layers$parentId) >= 41)
    expect_true(length(layers$alignment) >= 41)
    expect_true(length(layers$type) >= 41)
    expect_true(length(layers$peers) >= 41)
    expect_true(length(layers$peersOverlap) >= 41)
    expect_true(length(layers$parentIncludes) >= 41)
    expect_true(length(layers$saturated) >= 41)
    
    expect_false(is.logical(layers$id))
    expect_false(is.logical(layers$description))
    expect_false(is.logical(layers$parentId))
    expect_false(is.logical(layers$alignment))
    expect_false(is.logical(layers$type))
    expect_true(is.logical(layers$peers))
    expect_true(is.logical(layers$peersOverlap))
    expect_true(is.logical(layers$parentIncludes))
    expect_true(is.logical(layers$saturated))
    
    expect_false(is.numeric(layers$id))
    expect_false(is.numeric(layers$description))
    expect_false(is.numeric(layers$parentId))
    expect_true(is.numeric(layers$alignment))
    expect_false(is.numeric(layers$type))
    expect_false(is.numeric(layers$peers))
    expect_false(is.numeric(layers$peersOverlap))
    expect_false(is.numeric(layers$parentIncludes))
    expect_false(is.numeric(layers$saturated))
    
    expect_true("transcript" %in% layers$id)
    expect_true("orthography" %in% layers$id)
    expect_true("utterances" %in% layers$id)
    expect_true("turns" %in% layers$id)
    expect_true("who" %in% layers$id)
})
