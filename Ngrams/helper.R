library(data.table)

unigrams = readRDS("../data/unigrams.Rds")
bigrams = readRDS("../data/bigrams.Rds")
trigrams = readRDS("../data/trigrams.Rds")
quadgrams = readRDS("../data/quadgrams.Rds")
pentagrams = readRDS("../data/pentagrams.Rds")
wordscodes = readRDS("../data/wordscodes.Rds")

clean<-function(phrase){
    words<-unlist(strsplit(tolower(phrase)," "))
    words<-gsub("^[12]{1}[0-9]{3}$","YYYY",words)
    words<-gsub("^\\d+(,\\d*)*\\.*\\d*$","numeric",words)
    words<-gsub("[[:punct:]]","",words)
    words<-gsub("[^\x01-\x7F]+","",words)
    words<-words[!(words=="")]
    return(words)
}

wordtocode<-function(w){
    setkey(wordscodes,w1)
    wlist<-list(w)
    wordscodes[wlist]$c1
}
codetoword<-function(c){
    temp<-setkey(wordscodes,c1)
    clist<-list(c)
    wordscodes[clist]$w1
}

unigrampredict<-function(prevc,n){
    selection<-unigrams[!(c1 %in% prevc)][order(pc1,decreasing = TRUE)]
    newc<-selection$c1[1:n]
    newp<-selection$pc1[1:n]
    prediction<-list(codes = newc,probs = newp)
    return(prediction)
}

bigrampredict<-function(input,prevc,n){
    if(any(!is.na(bigrams[input][!(c2 %in% prevc)]$f12))){
        selection<-bigrams[input][!(c2 %in% prevc)][order(pc12,decreasing = TRUE)]
        newc<-selection$c2[1:min(n,nrow(selection))]
        newp<-selection$pc12[1:min(n,nrow(selection))]
        
        alpha<-selection$alpha[1]
        newc<-c(newc,unigrampredict(c(prevc,newc),n)$codes)
        newp<-c(newp,unigrampredict(c(prevc,newc),n)$probs*alpha)
    }
    else {
        newc<-unigrampredict(prevc,n)$codes
        newp<-unigrampredict(prevc,n)$probs
    }
    prediction<-list(codes = newc,probs = newp)
    return(prediction)
}

trigrampredict<-function(input,prevc,n){
    x1<-input[1]
    x2<-input[2]
    if(any(!is.na(trigrams[.(x1,x2)][!(c3 %in% prevc)]$f123))){
        selection<-trigrams[.(x1,x2)][!(c3 %in% prevc)][order(pc123,decreasing = TRUE)]
        newc<-selection$c3[1:min(n,nrow(selection))]
        newp<-selection$pc123[1:min(n,nrow(selection))]
        
        alpha<-selection$alpha[1]
        newc<-c(newc,bigrampredict(x2,c(prevc,newc),n)$codes)
        newp<-c(newp,bigrampredict(x2,c(prevc,newc),n)$probs*alpha)
    }
    else {
        newc<-bigrampredict(x2,prevc,n)$codes
        newp<-bigrampredict(x2,prevc,n)$probs
    }
    prediction<-list(codes = newc,probs = newp)
    return(prediction)
}

quadgrampredict<-function(inputc,prevc,n){
    x1<-inputc[1]
    x2<-inputc[2]
    x3<-inputc[3]
    if(any(!is.na(quadgrams[.(x1,x2,x3)][!(c4 %in% prevc)]$f1234))){
        selection<-quadgrams[.(x1,x2,x3)][!(c4 %in% prevc)][order(pc1234,decreasing = TRUE)]
        newc<-selection$c4[1:min(n,nrow(selection))]
        newp<-selection$pc1234[1:min(n,nrow(selection))]
        
        alpha<-selection$alpha[1]
        newc<-c(newc,trigrampredict(.(x2,x3),c(prevc,newc),n)$codes)
        newp<-c(newp,trigrampredict(.(x2,x3),c(prevc,newc),n)$probs*alpha)
    }
    else {
        newc<-trigrampredict(.(x2,x3),prevc,n)$codes
        newp<-trigrampredict(.(x2,x3),prevc,n)$probs
    }
    prediction<-list(codes = newc,probs = newp)
    return(prediction)
}

pentagrampredict<-function(inputc,prevc,n){
    x1<-inputc[1]
    x2<-inputc[2]
    x3<-inputc[3]
    x4<-inputc[4]
    if(any(!is.na(pentagrams[.(x1,x2,x3,x4)][!(c5 %in% prevc)]$f12345))){
        selection<-pentagrams[.(x1,x2,x3,x4)][!(c5 %in% prevc)][order(pc12345,decreasing = TRUE)]
        newc<-selection$c5[1:min(n,nrow(selection))]
        newp<-selection$pc12345[1:min(n,nrow(selection))]
        
        alpha<-selection$alpha[1]
        newc<-c(newc,quadgrampredict(.(x2,x3,x4),c(prevc,newc),n)$codes)
        newp<-c(newp,quadgrampredict(.(x2,x3,x4),c(prevc,newc),n)$probs*alpha)
    }
    else {
        newc<-quadgrampredict(.(x2,x3,x4),prevc,n)$codes
        newp<-quadgrampredict(.(x2,x3,x4),prevc,n)$probs
    }
    prediction<-list(codes = newc,probs = newp)
    return(prediction)
}

predictword<-function(phrase,n=5){
    words<-clean(phrase)
    nwords<-length(words)
    codes<-wordtocode(words)
    prevc<-NULL
    prediction<-NULL
    if(nwords>=4)
        prediction<-pentagrampredict(codes[(n-3):n],prevc,n)
    else if (nwords==3)
        prediction<-quadgrampredict(codes,prevc,n)
    else if (nwords==2)
        prediction<-trigrampredict(codes,prevc,n)
    else if (nwords==1)
        prediction<-bigrampredict(codes,prevc,n)
    else
        prediction<-unigrampredict(prevc,n)
    prediction <- list(words = codetoword(prediction$codes), 
                          probs = prediction$probs)
    return(prediction)
}
