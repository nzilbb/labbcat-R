# Changes

## v0.6-1

Minimum LaBB-CAT version: *20200812.1253*

New functions:

- *getMatchAlignments* : Gets temporal alignments of matches on a given layer.
- *getSerializerDescriptors* : Returns a list of serializers, which are modules that export
  annotation structures as a specific file format.
- *getDeserializerDescriptors* : Returns a list of deserializers, which are modules that
  import transcriptions and annotation structures from a specific file format.
- *getUserInfo* : Gets information about the current user.
- *getSystemAttribute* : Gets the value of the given system attribute.

## v0.5-1

Minimum LaBB-CAT version: *20200608.1507*

New functions:

- *getParticipantAttributes* : Gets participant attribute values for given participant IDs.
- *getTranscriptAttributes* : Gets transcript attribute values for given transcript IDs.
- *processWithPraat* : Process a set of intervals with Praat in order to extract acoustic
  measures, including functions for predefined measures:
- *praatScriptCentreOfGravity*
- *praatScriptFormants*
- *praatScriptIntensity*
- *praatScriptPitch*

getAnnotationLabels now supports

- retrieval of multiple layers
- labels for next/previous token

## v0.4-1

Minimum LaBB-CAT version: *20200108.1025*

- Added *getMatches* function for searching LaBB-CAT
- Improved handling of large lists passed to getAnchors
- Increased default timeout period

## v0.3-1

Minimum LaBB-CAT version is *20191022.1827*

New functions:

- *getFragments* : for exporting fragments as Praat TextGrids.
- *labbcatTimeout* : for specifying the request timeout (for those with slow internet)

Also:
- *path* parameter for the *getSoundFragments* function
