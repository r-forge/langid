`fun_mce` <-
function(langsubset,docprof){
  langsubset[which(langsubset==0)]<-0.000001
  matchres<-(-(log2(langsubset)%*%docprof)+-(langsubset%*%log2(docprof)))
  lang<-rownames(langsubset)[which(matchres==min(matchres))] #take language which fits best
  if(length(lang)>1) lang<-"und" #if 1< languages have a minimum value
  lang
}

