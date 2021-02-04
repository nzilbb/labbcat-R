# Author: Patrick Reidy
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.





################################################################################
# Class definition                                                  DPSS class #
################################################################################

setClass(
  Class = 'DPSS',
  contains = c(),
  slots    = c(tapers = 'matrix', 
               k      = 'numeric',
               nw     = 'numeric',
               N      = 'numeric')
  )



################################################################################
# Object construction                                            DPSS objects  #
################################################################################

setGeneric(
  name = 'DPSS',
  def  = function(x, k, nw) standardGeneric('DPSS'))

setMethod(
  f   = 'DPSS',
  sig = c(x = 'numeric', k = 'numeric', nw = 'numeric'),
  def = function(x, k, nw)
    new(Class = 'DPSS',
        tapers = dpss(n = x, k = k, nw = nw)$v,
        k      = k,
        nw     = nw,
        N      = x))

setMethod(
  f   = 'DPSS',
  sig = c(x = 'Waveform', k = 'numeric', nw = 'numeric'),
  def = function(x, k, nw)
    new(Class = 'DPSS',
        tapers = tapers(ZeroPad(DPSS(x = N(x), k = k, nw = nw),
                                lengthOut = N(x) + padded(x))),
        k      = k,
        nw     = nw,
        N      = N(x))
  )



################################################################################
# Methods                                                        DPSS methods  #
################################################################################

#############################################################################
# @k get-method                                    DPSS order parameter (k) #
#############################################################################

# k
if (! isGeneric('k'))
  setGeneric(
    name = 'k',
    def  = function(x) standardGeneric('k'))

setMethod(
  f   = 'k',
  sig = c(x = 'DPSS'),
  def = function(x) x@k)

# nTapers
if (! isGeneric('nTapers'))
  setGeneric(
    name = 'nTapers',
    def  = function(x) standardGeneric('nTapers'))

setMethod(
  f   = 'nTapers',
  sig = c(x = 'DPSS'),
  def = function(x) k(x))


#############################################################################
# @N get-method                          Non-padded length of DPSS sequence #
#############################################################################

# N
if (! isGeneric('N'))
  setGeneric(
    name = 'N',
    def  = function(x) standardGeneric('N'))

setMethod(
  f   = 'N',
  sig = c(x = 'DPSS'),
  def = function(x) x@N)


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
  sig = c(x = 'DPSS'),
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
  sig = c(x = 'DPSS'),
  def = function(x) x@tapers)


#############################################################################
# Zero-padding DPSS tapers                                          ZeroPad #
#############################################################################

# ZeroPad
if (! isGeneric('ZeroPad'))
  setGeneric(
    name = 'ZeroPad',
    def  = function(x, ...) standardGeneric('ZeroPad'))

setMethod(
  f   = 'ZeroPad',
  sig = c(x = 'DPSS'),
  def = function(x, lengthOut, everyHz = NULL) {
    if (! is.null(everyHz)) {
      lengthOut <- sampleRate(x) / everyHz
      if (lengthOut %% 1 != 0) {
        lengthOut <- ceiling(lengthOut)
        message(sprintf('Padding to %d samples (i.e., every %f Hz)'),
                lengthOut, sampleRate(x)/lengthOut)
      }
    }
    lengthOut <- max(lengthOut, N(x))
    .pad      <- matrix(0, nrow = lengthOut - N(x), ncol = nTapers(x))
    x@tapers  <- rbind(x@tapers, .pad)
    return(x)
  })

# padded
if (! isGeneric('padded'))
  setGeneric(
    name = 'padded',
    def  = function(x) standardGeneric('padded'))

setMethod(
  f   = 'padded',
  sig = c(x = 'DPSS'),
  def = function(x) nrow(tapers(x)) - N(x))
