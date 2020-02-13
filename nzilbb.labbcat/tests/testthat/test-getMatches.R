labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMatches works with 1x1 orthographic search using full structure", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    
    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     orthography = list(pattern = "knox")))))

    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)

    ## check dataframe columns
    expect_equal(length(matches$MatchId), 1)
    expect_equal(length(matches$Transcript), 1)
    expect_equal(length(matches$Line), 1)
    expect_equal(length(matches$LineEnd), 1)
    expect_equal(length(matches$Text), 1)
    expect_equal(length(matches$URL), 1)
    expect_equal(length(matches$Target.transcript), 1)
    expect_equal(length(matches$Target.transcript.start), 1)
    expect_equal(length(matches$Target.transcript.end), 1)

    ## search doens't include segments - ensure there's no segment information returned
    expect_true(is.null(matches$Target.segments))
    expect_true(is.null(matches$Target.segments.start))
    expect_true(is.null(matches$Target.segments.end))

    ## check dataframe column types
    expect_false(is.numeric(matches$MatchId))
    expect_false(is.numeric(matches$Transcript))
    expect_true(is.numeric(matches$Line))
    expect_true(is.numeric(matches$LineEnd))
    expect_false(is.numeric(matches$Text))
    expect_false(is.numeric(matches$URL))
    expect_false(is.numeric(matches$Target.transcript))
    expect_true(is.numeric(matches$Target.transcript.start))
    expect_true(is.numeric(matches$Target.transcript.end))

    expect_equal(as.vector(matches$Text)[[1]], "Knox")
})

test_that("getMatches works with 2x1 orthographic search using full structure", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     orthography = list(pattern = "knox")),
                 adj = 2),
            list(layers = list(
                     orthography = list(not = TRUE, pattern = "xxx")))))
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)
    
    expect_equal(length(matches$MatchId), 1)
})

test_that("getMatches works with 1x1 orthographic search using simple structure", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(orthography = "knox")
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)

    expect_equal(length(matches$MatchId), 1)
    expect_equal(as.vector(matches$Text)[[1]], "Knox")
})

test_that("getMatches works with 2x1 orthographic search using simple structure", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(list(orthography = "knox"), list(orthography = "church"))
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)

    expect_equal(length(matches$MatchId), 1)
    expect_equal(as.vector(matches$Text)[[1]], "Knox Church .")
})

test_that("getMatches works with 2x3 non-orthographic search using full structure", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     phonemes = list(pattern = "nQks"),
                     frequency = list(min = "1", max = "2")),
                 adj = 2),
            list(layers = list(
                     orthography = list(pattern = "h.*")))))
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)
    
    ## check dataframe columns
    expect_equal(length(matches$MatchId), 1)
    expect_equal(length(matches$Transcript), 1)
    expect_equal(length(matches$Line), 1)
    expect_equal(length(matches$LineEnd), 1)
    expect_equal(length(matches$Text), 1)
    expect_equal(length(matches$Target.transcript), 1)
    expect_equal(length(matches$Target.transcript.start), 1)
    expect_equal(length(matches$Target.transcript.end), 1)

    ## check dataframe column types
    expect_false(is.numeric(matches$MatchId))
    expect_false(is.numeric(matches$Transcript))
    expect_true(is.numeric(matches$Line))
    expect_true(is.numeric(matches$LineEnd))
    expect_false(is.numeric(matches$Text))
    expect_false(is.numeric(matches$Target.transcript))
    expect_true(is.numeric(matches$Target.transcript.start))
    expect_true(is.numeric(matches$Target.transcript.end))

    expect_equal(as.vector(matches$Text)[[1]], "Knox Church . had")
})

test_that("getMatches works with 2x3 non-orthographic search using simple structure", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(
        list(phonemes = "nQks",
             frequency = list(min = "1", max = "2")),
        list(orthography = "ch.*"))
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)

    expect_equal(as.vector(matches$Text)[[1]], "Knox Church .")
})

test_that("getMatches works with complex, multi-match searches", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    labbcatTimeout(60)

    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     orthography = list(pattern = "the")),
                 adj = 2),
            list(layers = list(
                     phonemes = list(not=TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\$@].*"),
                     frequency = list(max = "2")))))
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, no.progress=T)
    
    ## check dataframe columns
    expect_true(length(matches$MatchId) >= 1400)
    expect_true(length(matches$Transcript)>= 1400)
    expect_true(length(matches$Line)>= 1400)
    expect_true(length(matches$LineEnd)>= 1400)
    expect_true(length(matches$Text)>= 1400)
    expect_true(length(matches$Target.transcript)>= 1400)
    expect_true(length(matches$Target.transcript.start)>= 1400)
    expect_true(length(matches$Target.transcript.end)>= 1400)

    ## check dataframe column types
    expect_false(is.numeric(matches$MatchId))
    expect_false(is.numeric(matches$Transcript))
    expect_true(is.numeric(matches$Line))
    expect_true(is.numeric(matches$LineEnd))
    expect_false(is.numeric(matches$Text))
    expect_false(is.numeric(matches$Target.transcript))
    expect_true(is.numeric(matches$Target.transcript.start))
    expect_true(is.numeric(matches$Target.transcript.end))
})

test_that("getMatches includes segment info when segments layer searched", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     segments = list(pattern = "I")))))
    
    ## get matches
    matches <- getMatches(labbcat.url, pattern, participantId="UC427_ViktoriaPapp_A_ENG",
                          no.progress=T)
    
    ## check dataframe columns
    expect_true(length(matches$MatchId) >= 140)
    expect_true(length(matches$Transcript)>= 140)
    expect_true(length(matches$Line)>= 140)
    expect_true(length(matches$LineEnd)>= 140)
    expect_true(length(matches$Text)>= 140)
    expect_true(length(matches$Target.transcript)>= 140)
    expect_true(length(matches$Target.transcript.start)>= 140)
    expect_true(length(matches$Target.transcript.end)>= 140)
    expect_true(length(matches$Target.segments)>= 140)
    expect_true(length(matches$Target.segments.start)>= 140)
    expect_true(length(matches$Target.segments.end)>= 140)

    ## check dataframe column types
    expect_false(is.numeric(matches$MatchId))
    expect_false(is.numeric(matches$Transcript))
    expect_true(is.numeric(matches$Line))
    expect_true(is.numeric(matches$LineEnd))
    expect_false(is.numeric(matches$Text))
    expect_false(is.numeric(matches$Target.transcript))
    expect_true(is.numeric(matches$Target.transcript.start))
    expect_true(is.numeric(matches$Target.transcript.end))
})

test_that("filter parameters of getMatches work", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## get matches
    matchesAll <- getMatches(labbcat.url, list(orthography="the"), main.participant=F, no.progress=T)
    matchesMainParticipant <- getMatches(labbcat.url, list(orthography="the"), main.participant=T, no.progress=T)
    matchesOneParticipant <- getMatches(labbcat.url, list(orthography="the"), participantId="UC427_ViktoriaPapp_A_ENG", no.progress=T)
    
    ## check the number of results
    expect_true(length(matchesAll$MatchId) > length(matchesMainParticipant$MatchId))
    expect_true(length(matchesMainParticipant$MatchId) > length(matchesOneParticipant$MatchId))
})

test_that("words.context parameter of getMatches works", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     orthography = list(pattern = "knox")))))

    ## search for "knox", which has only one hit
    noContext <- getMatches(labbcat.url, list(orthography="knox"), words.context=0, no.progress=T)
    oneWord <- getMatches(labbcat.url, list(orthography="knox"), words.context=1, no.progress=T)
    fiveWords <- getMatches(labbcat.url, list(orthography="knox"), words.context=5, no.progress=T)
    wholeLine <- getMatches(labbcat.url, list(orthography="knox"), words.context=-1, no.progress=T)

    ## check the amount of context
    expect_true(is.na(noContext$Before.Match)[[1]])
    expect_true(is.na(noContext$After.Match)[[1]])

    expect_true(nchar(as.vector(oneWord$Before.Match)[[1]]) > 0)
    expect_true(nchar(as.vector(oneWord$After.Match)[[1]]) > 0)

    expect_true(nchar(as.vector(oneWord$Before.Match)[[1]])
                < nchar(as.vector(fiveWords$Before.Match)[[1]]))
    expect_true(nchar(as.vector(oneWord$After.Match)[[1]])
                < nchar(as.vector(fiveWords$After.Match)[[1]]))

    expect_true(nchar(as.vector(fiveWords$Before.Match)[[1]])
                < nchar(as.vector(wholeLine$Before.Match)[[1]]))
    expect_true(nchar(as.vector(fiveWords$After.Match)[[1]])
                < nchar(as.vector(wholeLine$After.Match)[[1]]))
})

test_that("getMatches pagination works", {
    skip_on_cran() # only simple searches on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## create pattern
    pattern <- list(
        columns = list(
            list(layers = list(
                     segments = list(pattern = "I")))))
    
    ## get matches - the total (as above) is >= 140, but we ask for the first 5
    matches <- getMatches(labbcat.url, pattern, maxMatches=5, no.progress=T)
    
    expect_equal(length(matches$MatchId), 5)
})
