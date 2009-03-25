`match_langs` <-
function(profX,docprof, method){
  docprof<-as.matrix(docprof[which(match(names(docprof),colnames(profX),nomatch=-1)!=-1)])#only take those ngs from docprof which exist in profX
  if(length(docprof)>0){ #are there ngs from docprof, which exist in profX?
    docprof<-docprof/sum(docprof) #calculate relative frequency of ngs within docprof
      langsubset<-subset(profX,select=match(rownames(docprof),colnames(profX))) #subset of profX... only ngs from docprof
      if ((nrow(langsubset) - length(which(rowSums(langsubset)==(ncol(langsubset)*0)))) >1) { #is there more than 1 language left?
          langsubset<-langsubset[which(rowSums(langsubset)!=(ncol(langsubset)*0)),] #languages wiht 0match are excluded
        langsubset<-t(apply(langsubset,1,function(x) {x/sum(x)})) #calculate relative frequency of ngs within langsubset
          if(!is.function(method)){ #predefined function is chosen
            lang<-switch(method,  mce = fun_mce(langsubset, docprof),
                                      re  = fun_re(langsubset, docprof))#match docprof against languages
          } else {
            lang<-method(langsubset,docprof) #match, using own function
          }
      } else {
       lang<-rownames(profX)[which(rowSums(langsubset)!=(ncol(langsubset)*0))] #if only 1 language fits... take name
      }
      } else {
        lang<- "zxx"} # empty:Input is not included in profX ... document can not be evaluated
        lang
  }

