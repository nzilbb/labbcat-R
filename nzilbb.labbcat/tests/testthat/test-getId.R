labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getId works", {    
    expect_equal(getId(labbcat.url), labbcat.url)
})
