\name{generate_ngrams}
\alias{generate_ngrams}
\title{Methods for Function generatengrams in Package langid}
\description{
Generates n-grams from the word \code{x}.
}
\usage{
generate_ngrams(x, splits)
}
\arguments{
  \item{x}{the word from wich the n-grams will be generated}
  \item{splits}{the upper boundary of the range of \code{n}}
}
\details{
  N-grams are strings cut out of a word, following specific rules. The word is
  automatically padded with the necessary number of blanks.
}
\value{A vector with the n-grams
}
\author{Johannes Rauch}
\examples{
(tst<-generate_ngrams(x="hallo",splits=5))
}
