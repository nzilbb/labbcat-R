# nzilbb.labbcat package for R <img src="logo.png" align="right" />

[![DOI](https://zenodo.org/badge/175465781.svg)](https://zenodo.org/badge/latestdoi/175465781)
[![CRAN
status](https://www.r-pkg.org/badges/version/nzilbb.labbcat)](https://CRAN.R-project.org/package=nzilbb.labbcat)

This R package provides functionality for querying and extracting data
from [LaBB-CAT](https://labbcat.canterbury.ac.nz/) servers, directly from R.

``` R
library(nzilbb.labbcat)
labbcat.url <- "http://localhost:8080/labbcat/"

# search for tokens of the KIT vowel
m <- getMatches(labbcat.url, list(segment="I")) |>
  # get morphology of the word, and the participant's gender
  appendLabels(c("morphology", "participant_gender")) |>
  # extract F1, 2, and 3 from the mid-point of each vowel
  appendFromPraat(
    Target.segment.start, Target.segment.end,
    praatScriptFormants(c(1,2,3)), 
    window.offset=0.5)
```

LaBB-CAT is a web-based linguistic annotation store that stores audio or video
recordings, text transcripts, and other annotations.

This package provides access to basic corpus structure data, pattern-based
search, annotation, audio, TextGrid (and other format) extraction, and server-side
acoustic measurement with Praat.

**Online documentation is available at https://nzilbb.github.io/labbcat-R**

## Basic usage instructions

### Getting started

To install the latest version of the package in CRAN:

``` R
install.packages("nzilbb.labbcat")
```

To use it:

``` R
library(nzilbb.labbcat)
```

For all functions, the first parameter is the URL to the LaBB-CAT instance you want
to interact with - e.g. "https://labbcat.canterbury.ac.nz/demo/".

If the instance is password-protected, you'll be prompted for the username and password
the first time you invoke a function for that instance.

### Basic informational functions

There are some basic functions that provide information about the LaBB-CAT instance you're using.

``` R
labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

id <- getId(labbcat.url)
layers <- getLayerIds(labbcat.url)
corpora <- getCorpusIds(labbcat.url)

paste("LaBB-CAT instance", id, "has", length(layers), "layers. The corpora are:")
corpora
```

### Accessing specific transcript and participant IDs

You can get a complete list of participants and transcripts:

``` R
participants <- getParticipantIds(labbcat.url)
transcripts <- getTranscriptIds(labbcat.url)

paste("There are", length(participants), "participants. The first one is", participants[1])
paste("There are", length(transcripts), "transcripts. The first one is", transcripts[1])
```

There are also ways to get a filtered list of transcripts:

``` R
## Transcripts in the UC corpus:
getTranscriptIdsInCorpus(labbcat.url, "UC")

## Transcripts featuring the participant QB1602:
getTranscriptIdsWithParticipant(labbcat.url, "QB1602")

## Transcripts with 'YW' in their name:
getMatchingTranscriptIds(labbcat.url, "/.*YW.*/.test(id)")
```

### Accessing Media

Given a transcript ID you can access information about what media it
has available: 

``` R
## Download the default WAV file
wav.file <- getMedia(labbcat.url, "AP2515_ErrolHitt.eaf")

## Get information about all media available
media <- getAvailableMedia(labbcat.url, "AP2515_ErrolHitt.eaf")

## All media file names with their track suffixes and content types
media[,c("name","trackSuffix","mimeType")]

## Download a specific media file
quake.face.video.file <- getMedia(labbcat.url, "AP2515_ErrolHitt.eaf",
                                  track.suffix = "_face", mime.type = "video/mp4")

## tidily delete the files we just downloaded
file.remove(wav.file)
file.remove(quake.face.video.file)

```

### Media fragments

You can access a selected fragment of a wav file with `getSoundFragments`. The function
downloads a wav file to the current working directory, and returns the name of the file: 

``` R
wav.file <- getSoundFragments(labbcat.url, "AP2505_Nelson.eaf", 10.0, 15.0)

paste("The third 5 seconds is in this file:", wav.file)

## tidily delete the file we just downloaded
file.remove(wav.file)
```

`getSoundFragments` also accepts vectors for the `id`, `start`, and `end` parameters:

``` R
results <- data.frame(
  id=c("AP2505_Nelson.eaf", "AP2512_MattBlack.eaf", "AP2512_MattBlack.eaf"),
  start=c(10.0, 20.0, 30.0),
  end=c(15.0, 25.0, 35.0))

wav.files <- getSoundFragments(labbcat.url, results$id, results$start, results$end, no.progress = TRUE)

wav.files

## tidily delete the files we just downloaded
file.remove(wav.files)
```

This means that, if you have a results csv file exported from LaBB-CAT, which identifies
segment tokens, you can iterate through the rows, downloading the corresponding wav files,
something like: 

``` R
## load the results from the CSV file
results <- read.csv("results.csv", header=T)

## download all the segment WAV files
wav.files <- getSoundFragments(
    labbcat, results$Transcript, results$segment.start, results$segment.end)
```

### Getting annotations from other layers

If you have search results in a CSV file, and would like to retrieve annotations from some other
layer, you can use the `getMatchLabels` function, providing the *MatchId* column (or the
*URL* column) that indentifies the token, and the desired layer name: 

``` R
results <- read.csv("results.csv", header=T)
phonemes <- getMatchLabels(labbcat.url, results$MatchId, c("participant_age", "phonemes"))
```

If you want alignment information - i.e. start and end time -- you can use `getMatchAlignments`:

``` R
results <- read.csv("results.csv", header=T)
phonemes <- getMatchAlignments(labbcat.url, results$MatchId, "syllables")
```

### Search

Searching for matching tokens can be achieved using the `getMatches` function.

A basic search can be achieved with a simple, single-layer pattern like:

``` R
# all words starting with "ps..."
results <- getMatches(labbcat.url, list(orthography = "ps.*"))
```

More complex patterns, across multiple tokens an multiple layers, is possible by specifying
a more complex structure:

``` R
# the word 'the' followed immediately or with one intervening word by
# a hapax legomenon (word with a frequency of 1) that doesn't start with a vowel
results <- getMatches(labbcat.url, list(columns = list(
     list(layers = list(
            orthography = list(pattern = "the")),
          adj = 2),
     list(layers = list(
            phonemes = list(not = TRUE, pattern = "[cCEFHiIPqQuUV0123456789~#\\{\\$@].*"),
            frequency = list(max = "2"))))))
```

The data frame that's returned contains columns that can be used as parameters for other
functions: 

``` R
# get all instances of the KIT vowel
results <- getMatches(labbcat.url, list(segment = "I")) 

# get phonemic transcription for the whole word
phonemes  <- getMatchLabels(labbcat.url, results$MatchId, "phonemes")

# download all the segment WAV files
wav.files <- getSoundFragments(
    labbcat.url, results$Transcript, results$Target.segment.start, results$Target.segment.end)
```

### Looking up dictionaries

LaBB-CAT maintains a number of
[dictionaries](https://labbcat.canterbury.ac.nz/demo/dictionaries)
it uses to look things
up.  These include access to CELEX, LIWC, and other lexicons that might be set up in the
LaBB-CAT instance. 

You can list the available dictionaries using:

``` R
dictionaries <- getDictionaries(labbcat.url)
```

With one of the returned layer manager ID and dictionary ID pairs, you can look up
dictionary entries for a list of keys: 

``` R
words <- c("the", "quick", "brown", "fox")
pronunciation <- getDictionaryEntries(labbcat.url, "CELEX-EN", "Phonology (wordform)", words)
```

### Process with Praat

This function instructs the LaBB-CAT server to invoke Praat for a set of sound
intervals, in order to extract acoustic measures.

The exact measurements to return depend on the `praat.script` that is invoked. This is a
Praat script fragment that will run once for each sound interval specified.

There are functions to allow the generation of a number of pre-defined praat scripts
for common tasks such as formant, pitch, intensity, and centre of gravity:

``` R
# Perform a search
results <- getMatches(labbcat.url, list(segment="I"))

# get F1 and F2 for the mid point of the vowel
formants <- processWithPraat(
       labbcat.url,
       results$MatchId, results$Target.segment.start, results$Target.segment.end,
       praatScriptFormants(),
       no.progress=TRUE)
```

You can provide your own script, either by building a string with your code, or loading
one from a file.

``` R
# execute a custom script loaded form a file
acoustic.measurements <- processWithPraat(
       labbcat.url,
       results$MatchId, results$Target.segment.start, results$Target.segment.end,
       readLines("acousticMeasurements.praat"))
```

### Retrieving transcript and participant attributes

Transcript attributes can be retrieved like this: 

``` R
# Get language, duration, and corpus for transcripts starting with 'BR'
attributes <- getTranscriptAttributes(labbcat.url,
            getMatchingTranscriptIds(labbcat.url, "/BR.+/.test(id)"),
            c('transcript_language', 'transcript_duration', 'corpus'))
```

Similarly, participant attributes can also be accessed:

``` R
# Get gender and age for all participants
attributes <- getParticipantAttributes(labbcat.url,
            getParticipantIds(labbcat.url),
            c('participant_gender', 'participant_age'))
```

# Developers

## Prerequesites

Developer tools:

```
R -e "install.packages('devtools')"
```

For building the documentation with pkgdown:

```
apt install pandoc
R -e "install.packages('pkgdown')"
```

## Building the package and documentation

The package can be built from the source code using using:  
```
./build.sh
```

## Running automated tests

Unit tests use the 'testthat' package, which requires a one-time installation:

```
R -e "install.packages('testthat')"
```

The tests assume access to at least one LaBB-CAT server, with URL and credentials defined
by environment variables, so you must create a `.Renviron` file something like:

```
TEST_READ_LABBCAT_URL=https://labbcat.canterbury.ac.nz/demo/
TEST_READ_LABBCAT_USERNAME=demo
TEST_READ_LABBCAT_PASSWORD=demo
TEST_ADMIN_LABBCAT_URL=http://localhost:8080/labbcat/
TEST_ADMIN_LABBCAT_USERNAME=labbcat
TEST_ADMIN_LABBCAT_PASSWORD=labbcat
```

Then you can use the following commands to run unit tests: 

```
R -e "devtools::test()"
```

Specific tests can be run like this:

```
R -e "devtools::test(filter='getId')"
```

