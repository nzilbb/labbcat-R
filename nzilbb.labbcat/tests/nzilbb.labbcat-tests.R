require(nzilbb.labbcat)

## define the LaBB-CAT URL
labbcat.url <- "https://labbcat.canterbury.ac.nz/demo/"

## specify the username/password in the script
## (only use labbcatCredentials for scripts that must execute unsupervised!)
labbcatCredentials(labbcat.url, "demo", "demo")

## Get ID of LaBB-CAT instance
getId(labbcat.url)

## Get names of all layers
getLayerIds(labbcat.url)

## Get definitions of all layers
getLayers(labbcat.url)

## Get the definition of the orthography layer
getLayer(labbcat.url, "orthography")

## List corpora
getCorpusIds(labbcat.url)

## Get the media tracks configured in LaBB-CAT
getMediaTracks(labbcat.url)

## List all speakers
getParticipantIds(labbcat.url)

## List all transcripts
getGraphIds(labbcat.url)

## List transcripts in the QB corpus
getGraphIdsInCorpus(labbcat.url, "QB")

## List transcripts in which UC427_ViktoriaPapp_A_ENG speaks
getGraphIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")

## Get all transcripts whose names start with "BR"
getMatchingGraphIds(labbcat.url, "id MATCHES 'BR.+'")
 
## Get the first twenty transcripts in the "QB" corpus
getMatchingGraphIds(labbcat.url, "my('corpus').label = 'QB'", 20, 0)

## Get the second transcript that has "QB247_Jacqui" as a speaker
getMatchingGraphIds(labbcat.url, "'QB247_Jacqui' IN labels('who')", 1, 0)

## Get all transcripts whose names start with "BR" and have "QB247_Jacqui" as a speaker,
## in word-count order 
getMatchingGraphIds(labbcat.url, "my('corpus').label = 'QB' AND 'QB247_Jacqui' IN labels('who')",
                    1, 0, "my('transcript_word count').label")

## Count the number of words in UC427_ViktoriaPapp_A_ENG.eaf
countAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")

## Get all the orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography")

## Get the first 20 orthography tokens in UC427_ViktoriaPapp_A_ENG.eaf
orthography <- getAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography", 20, 0)
orthography

## Get the start anchors for the above tokens TODO returns all the same anchor
getAnchors(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", orthography$startId)

## List the media files available for BR2044_OllyOhlson.eaf
getAvailableMedia(labbcat.url, "BR2044_OllyOhlson.eaf")

## Get URL for the WAV file for BR2044_OllyOhlson.eaf
getMedia(labbcat.url, "BR2044_OllyOhlson.eaf")

## Get URL for the 'QuakeFace' video file for BR2044_OllyOhlson.eaf
getMedia(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4")

## Get the 5 seconds starting from 10s after the beginning of a recording
wav.file <- getSoundFragments(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0)
wav.file
file.remove(wav.file)
 
## Get the 5 seconds starting from 10s as a mono 22kHz file
wav.file <- getSoundFragments(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0, 22050)
wav.file
file.remove(wav.file)

## simulate some results
results <- data.frame(
    Transcript=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
    Line=c(10.0, 20.0, 30.0),
    LineEnd=c(15.0, 25.0, 35.0))
 
## Get a list of fragments
wav.files <- getSoundFragments(labbcat.url, results$Transcript, results$Line, results$LineEnd)
wav.files
file.remove(wav.files)
file.remove("fragments")

## Get a list of fragments with no progress bar
wav.files <- getSoundFragments(
    labbcat.url, results$Transcript, results$Line, results$LineEnd, no.progress=TRUE)
wav.files
file.remove(wav.files)
file.remove("fragments")

## simulate some results
results <- data.frame(
    MatchId=c("g_6;em_12_419;n_9243-n_9245;p_14;#=ew_0_7260;[0]=ew_0_7260",
              "g_6;em_12_429;n_9263-n_9265;p_14;#=ew_0_7487;[0]=ew_0_7487",
              "g_6;em_12_440;n_9285-n_9287;p_14;#=ew_0_7704;[0]=ew_0_7704"))

## Get the topic annotations for the matches
getAnnotationLabels(labbcat.url, results$MatchId, "topic")

## List the dictionaries available
getDictionaries(labbcat.url)

## get the pronunciations according to CELEX
getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)",
                     c("the", "quick", "brown", "fox"))
