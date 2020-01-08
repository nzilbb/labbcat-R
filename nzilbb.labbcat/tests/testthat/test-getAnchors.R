labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getAnchors works", {
    orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
    anchors <- getAnchors(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
    expect_equal(length(anchors$id), 20)
    expect_equal(length(anchors$offset), 20)
    expect_equal(length(anchors$confidence), 20)
    
    expect_false(is.numeric(anchors$id))
    expect_true(is.numeric(anchors$offset))
    expect_true(is.numeric(anchors$confidence))
})
