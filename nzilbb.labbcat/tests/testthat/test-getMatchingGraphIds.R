labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatchingGraphIds works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getMatchingGraphIds(labbcat.url, "id MATCHES 'BR.+'")
    expect_equal(length(ids), 6)
    expect_true("BR946_RodgerCurragh.eaf" %in% ids)
    expect_false("QB247_Jacqui.eaf" %in% ids)
})
