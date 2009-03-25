\name{make_profX}
\alias{make_profX}
\title{Methods for Function makeprofX in Package langid}
\description{This function provides an easy-to-use method to generate
 a new language profile or to augment an existing one.
}
\usage{
make_profX(icorpus, div = 0.25, profX = "new", profXlen = 1000, splits = 5, langcode = stop("name must be specified"))
}
\arguments{
    \item{icorpus}{the name of the corpus with the trainingdata, generated using \pkg{tm}.}
    \item{div}{ specifies how many percent of the most frequent n-grams generated from each
    of the documents within the corpus, should be included in the language-profile. }
    \item{profX}{ if a new language profile should be created \code{profX}
    should be set to \code{new}. If an existing profile language matrix should be
    augmented by a new language, \code{profX} should be given the name of this
    existing \code{profX}.}
    \item{profXlen}{ only the top \code{profXlen} n-grams of all the trainingdate in
    \code{icorpus} will be included in the \code{profX}. }
    \item{splits}{ the upper boundary of the range of \code{n}}
    \item{langcode}{ the ISO 639-3 code for the language which should be included
    in the \code{profX}.}
}  \details{
  A language profile contains the \code{profXlen} most frequently occuring n-grams
  of a specific language. It is used as a modell against which unknown languages will
  be tested in order to classify them.

  The \code{icorpus} should contain all the trainingdata for ONE specific language.
  The trainingdata MUST have been saved in UTF-8.

  Across different language-profiles within one \code{profX}, the \code{profXlen}-value
   should always be the same.

  For \code{langcode}, the ISO 639-3 abreviation MUST be used.
  (see: \url{http://www.sil.org/ISO639-3/codes.asp} )
  }
\value{
A matrix. One line for each language, one column for each n-gram.}
\author{ Johannes Rauch }
\seealso{ \code{\link{generate_ngrams}} }
\examples{
  require("tm")
  data("td_eng") #english trainingdata
  data("td_ger") #german trainingdata

    icorpus<-Corpus(VectorSource(td_eng)) #generate englisch Corpus using tm
    profX<-make_profX(icorpus,div=0.5,profX="new",profXlen=1000,splits=5,langcode="eng")

    icorpus<-Corpus(VectorSource(td_ger)) #generate german Corpus using tm
    profX<-make_profX(icorpus,div=0.5,profX=profX,profXlen=1000,splits=5,langcode="ger")

    dim(profX)
    profX[,1:10]
}