labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getAnnotations works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
    expect_equal(length(orthography$id), 20)
    expect_equal(length(orthography$layerId), 20)
    expect_equal(length(orthography$label), 20)
    expect_equal(length(orthography$startId), 20)
    expect_equal(length(orthography$endId), 20)
    expect_equal(length(orthography$parentId), 20)
    expect_equal(length(orthography$ordinal), 20)
    expect_equal(length(orthography$confidence), 20)

    expect_false(is.numeric(orthography$id))
    expect_false(is.numeric(orthography$layerId))
    expect_false(is.numeric(orthography$label))
    expect_false(is.numeric(orthography$startId))
    expect_false(is.numeric(orthography$endId))
    expect_false(is.numeric(orthography$parentId))
    expect_true(is.numeric(orthography$ordinal))
    expect_true(is.numeric(orthography$confidence))

    expect_equal(orthography$label[[1]], "i")
    expect_equal(orthography$label[[20]], "for")
})
