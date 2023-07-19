test_that("expressionFromTranscriptTypes works", {
    expect_equal(
        expressionFromTranscriptTypes(c("wordlist","monologue")),
        "['wordlist','monologue'].includes(first('transcript_type').label)")
})
test_that("expressionFromTranscriptTypes works with negation", {
    expect_equal(
        expressionFromTranscriptTypes(c("wordlist","monologue"), not=TRUE),
        "!['wordlist','monologue'].includes(first('transcript_type').label)")
})
test_that("expressionFromTranscriptTypes works with quotes", {
    expect_equal(
        expressionFromTranscriptTypes(c("participant's life story","\"Star Wars\" retelling")),
        "['participant\\'s life story','\"Star Wars\" retelling'].includes(first('transcript_type').label)")
})
test_that("expressionFromTranscriptTypes works with a single value", {
    expect_equal(
        expressionFromTranscriptTypes("interview"),
        "first('transcript_type').label == 'interview'")
})
