labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getAnchors works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    orthography <- getAnnotations(
        labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", NULL, 20, 0)
    anchors <- getAnchors(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)
    expect_equal(length(anchors$id), 20)
    expect_equal(length(anchors$offset), 20)
    expect_equal(length(anchors$confidence), 20)
    
    expect_false(is.numeric(anchors$id))
    expect_true(is.numeric(anchors$offset))
    expect_true(is.numeric(anchors$confidence))
})
