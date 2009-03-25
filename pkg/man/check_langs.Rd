\name{check_langs}
\alias{check_langs}
\title{ Methods for Function checklangs in Package langid }
\description{Identifies the language of all documents within a \pkg{tm}-corpus.
}
\usage{
check_langs(icorpus, profX = profX, omw, numw, numng, splits = 5, method)
}
\arguments{
  \item{icorpus}{ the name of the corpus with the text documents to be tested,
  generated using \pkg{tm} }
  \item{profX}{ the name of the language profile matrix, generated using \code{make_profX}. }
  \item{omw}{ number of words to be ommited at the beginning of the text document. }
  \item{numw}{ number of words to be used for identifying a  documents language. }
  \item{numng}{ number of the most frequent n-grams that are to be used for building
  the n-gram distribution of the text document. }
  \item{splits}{ the upper boundary of the range of \code{n} }
  \item{method}{ method used for testing document profile against language profiles -
  \code{"re"},\code{"mce"} or customized.}
}
\details{
Only the amount of \code{numw} words is being used for each document to identify
its language. They are cut out of the text as a block, starting with the word
at position \code{omw} + 1. This gives the opportunity to skip abstracts or other
features of a document, which might not be best suited for identifying the language.
From those selected words, n-grams will be generated. Of those n-grams, only the
top \code{numng} n-grams will be used for building the frequency distribution needed
for language identification.

Two methods have allready been implemented: relative entropy("re") and
mutual cross entropy ("mce"). Customized functions can be implemented easily.
}
\value{
A Vector with the ISO 639-3 codes of the identified languages.}
\author{ Johannes Rauch }
\seealso{ \code{\link{generate_ngrams}},\code{\link{make_profX}}}
\examples{
require("tm")
data("profX_ger_eng") #see help("generate_profX")
profX<-profX_ger_eng

identifythis<-c(
"Wer rastet, der rostet",
"A rolling stone gathers no moss.",
"Was Haenschen nicht lernt, lernt Hans nimmermehr",
"You cant teach an old dog new tricks") #documents to be categorised

icorpus<-Corpus(VectorSource(identifythis)) #make corpus
lang_ids<-check_langs(icorpus,profX,omw=0,numw=100,
numng=700,splits=5,method="re") #identify languages
lang_ids #vector with ISO 639-3 language names
}
