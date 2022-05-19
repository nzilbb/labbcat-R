# nzilbb.labbcat 1.1-1

Minimum LaBB-CAT version *20220401.1842*

## Enhancements

- *praatScriptPitch* and *praatScriptIntensity* have additional parameters
  + *sample.points* - `Get value at time` for points spread through the duration of the sample
  + *interpolation* - interpolation to use for `Get value at time`
  + *skip.errors* - how to handle errors when Praat can't create objects

# nzilbb.labbcat 1.1-0

Minimum LaBB-CAT version *20220401.1842*

## Enhancements

## New functions:

- *updateFragment* : Upload an uttereance file (e.g. TextGrid) to update labels and/or alignments.
- *generateLayer* : Generate annotations on a given layer for all transcripts in the corpus.
- *generateLayerUtterances* : Generate a layer for a given utterances.
- *getAnnotatorDescriptor* : Get information about an annotator.
- *annotatorExt* : Retrieve an annotator extension resource.
- Manage annotation layers
  + *newLayer* : Add a new annotation layer.
  + *saveLayer* : Save a new definition for an existing annotation layer.
  + *deleteLayer* : Delete an existing annotation layer.
- Manage Flat Lexicon Tagger lexicons
  + *uploadLexicon* : Upload a lexicon file for the Flat Lexicon Tagger.
  + *deleteLexicon* : Delete a previously uploaded lexicon from the Flat Lexicon Tagger.
- Manage dictionary entries
  + *addLayerDictionaryEntry*
  + *removeLayerDictionaryEntry*
  + *addDictionaryEntry*
  + *removeDictionaryEntry*

# nzilbb.labbcat 1.0-1

Minimum LaBB-CAT version *20210601.1528*

## Enhancements

- All function parameters are now named with dot.case instead of camelCase, for internal
  and external consistency.
- Re-instate *no.progress* parameter for
  + *getAllUtterances*
  + *getMatchAlignments*
  + *getMatchLabels*
  + *getSoundFragments*
  + *newTranscript*
  + *updateTranscript*
  + *processWithPraat*
- Break some requests into chunks to avoid timeouts for very large result sets:
  + *getMatches*
  + *getMatchLabels*
  + *getMatchAlignments*
- Introduce *include.match.ids* parameter for manual matching of prior results if required for
  + *getMatchLabels*
  + *getMatchAlignments*
- Introduce *page.length* parameter for tuning request sizes on some functions for
  + *getAnchors*
  + *getAnnotations*
  + *getMatchingTranscriptIds*
  + *getMatches*
  + *getMatchLabels*
  + *getMatchAlignments*
- CSV-related bug fixes.
- Improve progress bar handling.
- Improve documentation of matching expression language.

## New functions:

- *getMatchingParticipantIds*: Gets a list of IDs of participants that match a particular pattern.

# nzilbb.labbcat 0.7-1

Minimum LaBB-CAT version *20210210.2032*

## Enhancements

- remove no.progress parameter (progress bar will appear automatically only in interactive mode).
- getMatches now supports *overlap.threshold* parameter.

## New functions:

- *newTranscript* : Upload a new transcript (with media) to the server.
- *updateTranscript* : Upload a new version of an existing transcript to the server.
- *deleteTranscript* : Delete a transcript (and its media) from the server.
- *getAllUtterances* : Get all utterances of given participants.
- *praatScriptFastTrack* : Adds support for using the FastTrack Praat plugin for formant analysis.

# nzilbb.labbcat 0.6-1

Minimum LaBB-CAT version *20200812.1253*

## New functions:

- *getMatchAlignments* : Gets temporal alignments of matches on a given layer.
- *getSerializerDescriptors* : Returns a list of serializers, which are modules that export
  annotation structures as a specific file format.
- *getDeserializerDescriptors* : Returns a list of deserializers, which are modules that
  import transcriptions and annotation structures from a specific file format.
- *getUserInfo* : Gets information about the current user.
- *getSystemAttribute* : Gets the value of the given system attribute.

# nzilbb.labbcat 0.5-1

Minimum LaBB-CAT version *20200608.1507*

## New functions:

- *getParticipantAttributes* : Gets participant attribute values for given participant IDs.
- *getTranscriptAttributes* : Gets transcript attribute values for given transcript IDs.
- *processWithPraat* : Process a set of intervals with Praat in order to extract acoustic
  measures, including functions for predefined measures:
- *praatScriptCentreOfGravity*
- *praatScriptFormants*
- *praatScriptIntensity*
- *praatScriptPitch*

## Enhancements

- getAnnotationLabels now supports
  + retrieval of multiple layers
  + labels for next/previous token

# nzilbb.labbcat 0.4-1

Minimum LaBB-CAT version *20200108.1025*

## Enhancements

- Added *getMatches* function for searching LaBB-CAT
- Improved handling of large lists passed to getAnchors
- Increased default timeout period

# nzilbb.labbcat 0.3-1

Minimum LaBB-CAT version *20191022.1827*

## New functions:

- *getFragments* : for exporting fragments as Praat TextGrids.
- *labbcatTimeout* : for specifying the request timeout (for those with slow internet)

## Enhancements

- *path* parameter for the *getSoundFragments* function
