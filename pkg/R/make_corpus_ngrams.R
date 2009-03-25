`make_corpus_ngrams` <-
function(traindata,div,profX,splits) { #corpusngs:the table with absolute frequencies of n-grams from ALL documents
  corpusngs<-NULL
  for (i in seq_len(length(traindata))) {
    doc<-unlist(strsplit(traindata[[i]]," ")) #make list
    docngs<-unlist(lapply(doc,generate_ngrams,splits))#generate n-grams
    corpusngs<-sort(consolidate_ngrams(corpusngs,docngs,div),decreasing=TRUE) #generate corpusngs
    corpusngs
  }
}

