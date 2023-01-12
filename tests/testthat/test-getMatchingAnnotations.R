labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatchingAnnotations works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    topic <- getMatchingAnnotations(labbcat.url, "layer.id == 'topic' && /.*quake.*/.test(label)", 20, 0)
    expect_equal(length(topic$id), 20)
    expect_equal(length(topic$layerId), 20)
    expect_equal(length(topic$label), 20)
    expect_equal(length(topic$startId), 20)
    expect_equal(length(topic$endId), 20)
    expect_equal(length(topic$parentId), 20)
    expect_equal(length(topic$ordinal), 20)
    expect_equal(length(topic$confidence), 20)

    expect_false(is.numeric(topic$id))
    expect_false(is.numeric(topic$layerId))
    expect_false(is.numeric(topic$label))
    expect_false(is.numeric(topic$startId))
    expect_false(is.numeric(topic$endId))
    expect_false(is.numeric(topic$parentId))
    expect_true(is.numeric(topic$ordinal))
    expect_true(is.numeric(topic$confidence))

    expect_equal(topic$label[[1]], "{September earthquake experience}")
    expect_equal(topic$label[[20]], "{personal background to the earthquakes}")
})
