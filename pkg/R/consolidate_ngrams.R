`consolidate_ngrams` <-
function(corpusngs,docngs,div){
  ngtbl<-sort(table(docngs),decreasing = TRUE) #ng-table from document
    mit<-match(names(ngtbl)[seq_len(length(ngtbl)*div)],names(corpusngs),nomatch=-2)#takes the top 1:(1*div)
      corpusngs[mit[which(mit!=-2)]]<- corpusngs[mit[which(mit!=-2)]] + ngtbl[c(seq_len(length(ngtbl)))[which(mit!=-2)]] #if ng exists-> increase
      corpusngs<-c(corpusngs,ngtbl[which(mit==-2)]) #if ng doesn't exist:add as new ng
}

