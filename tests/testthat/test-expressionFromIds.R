test_that("expressionFromIds works", {
    expect_equal(
        expressionFromIds(c("transcript1.trs","transcript2.eaf")),
        "['transcript1.trs','transcript2.eaf'].includes(id)")
})
test_that("expressionFromIds works with negation", {
    expect_equal(
        expressionFromIds(c("transcript1.trs","transcript2.eaf"), not=TRUE),
        "!['transcript1.trs','transcript2.eaf'].includes(id)")
})
test_that("expressionFromIds works with IDs that contain quotes", {
    expect_equal(
        expressionFromIds(c("O'Sullivan.TextGrid","\"T\" Rex.slt")),
        "['O\\'Sullivan.TextGrid','\"T\" Rex.slt'].includes(id)")
})
