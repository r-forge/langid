`generate_ngrams` <-
function(x,splits){ #x is the word to be split into n= 1:splits
k<-nchar(x) #number of characters of x
x<-paste(" ",x,paste(c(rep( " ",(splits-1))),sep="",collapse=""),sep="",collapse="") #word padded with blanks
ng<-min(k,splits)

  if(k>1) {
    from<-rep(1:(k+1),ng) # cut characters from:to within x
    to  <- rep(1:(k+1),ng) + rep((0:(ng-1)),each =(k+1) )
    ngtot<-substring(x,from,to) #all possible n-grams.. the traditional way
    if((k+1)<splits) ngtot<-c(ngtot, substring(x,1,k+2)) #short words can be added as a whole "_ x _"

    rule1<-((1:(ng))*(k+1))-((k+1)-2) #n-grams where info: _x is missing
    rule2<-((k+1):((k+1)-(ng-1)))+((0:(ng-1))*(k+1)) #n-grams where info: x_ is missing
    if(ng>2) rule3<-rep(((3:ng)*(k+1))+1,(1:(ng-2)))-sequence(c(1:(ng-2))) else rule3<-NULL #redundant endings
    ngram<-ngtot[-c(1,rule1,rule2,rule3)]#those n-grams are deleted from ngtot

  } else {
    if (k==1) {
      ngram<- substring(x,1,3)
      } else {
         ngram <-""}}
}

