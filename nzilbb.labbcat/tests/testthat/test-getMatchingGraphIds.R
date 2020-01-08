labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getMatchingGraphIds works", {
    ids <- getMatchingGraphIds(labbcat.url, "id MATCHES 'BR.+'")
    expect_equal(length(ids), 6)
    expect_true("BR946_RodgerCurragh.eaf" %in% ids)
    expect_false("QB247_Jacqui.eaf" %in% ids)
})
