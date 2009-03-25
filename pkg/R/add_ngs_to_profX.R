`add_ngs_to_profX` <-
function(corpusngs,profX,profXlen,langcode){
  if(length(profX)==1){ #if a new language profile set is to be created
    profX<-matrix(corpusngs[seq_len(min(length(corpusngs),profXlen))],nrow=1,dimnames=list(langcode,names(corpusngs)[seq_len(min(length(corpusngs),profXlen))]))#add first lang
    }else{ #augment an existing language profiles set
     corpusngs<-corpusngs[seq_len(min(length(corpusngs),profXlen))] #only take top 1:profXlen ngs of corpusngs - table
     profX<-rbind(profX,rep(0,ncol(profX))) #all 0 will be replaced by positive values, if corresponding ng exists
     rownames(profX)[nrow(profX)]<-langcode
      mit<-match(names(corpusngs),colnames(profX),nomatch=-2) #which ngs do allready exist in profX?
        if (length(which(mit!=-2))>0) { #the ng exists allready, the value is written in the language - line
          profX[nrow(profX),mit[which(mit!=-2)]]<-corpusngs[c(seq_len(length(corpusngs)))[which(mit!=-2)]]}
        if (length(which(mit==-2)>0)) { #the ng doesn't exist yet ... new columns are added
          newmx<-matrix(c(rep(0,length(which(mit==-2))*(nrow(profX)-1)),corpusngs[which(mit==-2)]),
          nrow=nrow(profX),byrow=TRUE,dimnames=list(NULL,names(corpusngs)[which(mit==-2)]))}
        profX<-cbind(profX,newmx)
    }
  profX
}

