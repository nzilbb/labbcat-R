labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getAnchors works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
    anchors <- getAnchors(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
    expect_equal(length(anchors$id), 20)
    expect_equal(length(anchors$offset), 20)
    expect_equal(length(anchors$confidence), 20)
    
    expect_false(is.numeric(anchors$id))
    expect_true(is.numeric(anchors$offset))
    expect_true(is.numeric(anchors$confidence))
})
