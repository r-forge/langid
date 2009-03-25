`check_langs` <-
function(icorpus,profX=profX,omw,numw,numng,splits=5,method){
testdoc<-modify_icorpus(icorpus) #transform input corpus
lid<-NULL #vector will contain all ISO 639-3 codes... when done
  for (i in seq_len(length(testdoc))) {
    docprof<-NULL # document profile: table with all the numng n-grams
    doc<-unlist(strsplit(testdoc[[i]]," ")) #make list
    doc<-doc[max((min(((omw+1)+numw),length(doc))-numw),1):min(((omw+1)+numw),length(doc))]#only words from omw to omw+numw
    docngs<-unlist(lapply(doc,generate_ngrams,splits))#generate ngs from each of the 1:numw words
    docprof<-sort(table(docngs),decreasing = TRUE)[1:numng] #generate document profile
    lid<-c(lid,match_langs(profX,docprof,method))#chose language which fits best
    lid
   }
}

