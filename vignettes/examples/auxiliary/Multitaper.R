# Author: Patrick Reidy
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.





# Quietly load the dependencies.
suppressPackageStartupMessages(library('multitaper'))



################################################################################
# Class definition                                            Multitaper class #
################################################################################

setClass(
  Class = 'Multitaper',
  contains = c('Spectrum'),
  slots    = c(tapers = 'DPSS',
               k      = 'numeric',
               nw     = 'numeric'))



################################################################################
# Object construction                                       Multitaper objects #
################################################################################

setGeneric(
  name = 'Multitaper',
  def  = function(x, k, nw, ...) standardGeneric('Multitaper'))

setMethod(
  f   = 'Multitaper',
  sig = c(x = 'Waveform', k = 'numeric', nw = 'numeric'),
  def = function(x, k, nw) {
    # Use attributes of the Waveform x to determine Spectrum attributes.
    .nyquist     <- sampleRate(x) / 2
    .bin.width   <- sampleRate(x) / length(samples(x))
    .frequencies <- seq(from = 0, to = .nyquist, by = .bin.width)
    # Compute the DPSS tapers.
    .dpss    <- DPSS(x, k, nw)
    # Make k tapered copies of the Waveform x.
    .tapered <- apply(tapers(.dpss), 2, `*`, samples(x))
    # Compute the k eigenspectra of the tapered copies of x.
    .eigen   <- (1 / (N(x)*samplePeriod(x))) * abs(fft(.tapered))^2
    .eigen   <- .eigen[1:length(.frequencies), ]
    # Average to eigenspectra to compute the multitaper spectrum.
    .multitaper <- rowMeans(.eigen)
    # Construct a new Multitaper object.
    new(Class = 'Multitaper',
        values   = .multitaper,
        binWidth = .bin.width,
        nyquist  = .nyquist,
        k        = k,
        nw       = nw,
        tapers   = .dpss)
  })



################################################################################
# Methods                                                  Multitaper methods  #
################################################################################

#############################################################################
# @k get-method                                     MTS order parameter (k) #
#############################################################################

# k
if (! isGeneric('k'))
  setGeneric(
    name = 'k',
    def  = function(x) standardGeneric('k'))

setMethod(
  f   = 'k',
  sig = c(x = 'Multitaper'),
  def = function(x) x@k)

# nTapers
if (! isGeneric('nTapers'))
  setGeneric(
    name = 'nTapers',
    def  = function(x) standardGeneric('nTapers'))

setMethod(
  f   = 'nTapers',
  sig = c(x = 'Multitaper'),
  def = function(x) k(x))


#############################################################################
# @nw get-method                         DPSS time-bandwidth parameter (nw) #
#############################################################################

# nw
if (! isGeneric('nw'))
  setGeneric(
    name = 'nw',
    def  = function(x) standardGeneric('nw'))

setMethod(
  f   = 'nw',
  sig = c(x = 'Multitaper'),
  def = function(x) x@nw)


#############################################################################
# @tapers get-method                                            DPSS tapers #
#############################################################################

# tapers
if (! isGeneric('tapers'))
  setGeneric(
    name = 'tapers',
    def  = function(x) standardGeneric('tapers'))

setMethod(
  f   = 'tapers',
  sig = c(x = 'Multitaper'),
  def = function(x) x@tapers)
