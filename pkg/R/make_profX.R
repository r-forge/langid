`make_profX` <-
function(icorpus,div=0.25,profX="new",profXlen=1000,splits=5,langcode=stop("name must be specified")) {
  traindata<-modify_icorpus(icorpus) #transform input corpus
  corpusngs<-make_corpus_ngrams(traindata,div,profX,splits)#the ngs of ALL the words in ALL the documents of traindata
  profX<-add_ngs_to_profX(corpusngs,profX,profXlen,langcode) #make new or augment existing profX
  profX
}

