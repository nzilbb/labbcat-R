labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getMedia works", {
    expect_equal(
        getMedia(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4"),
        "https://labbcat.canterbury.ac.nz/demo/files/QB/BR2044_OllyOhlson/mp4/BR2044_OllyOhlson_face.mp4")
})
