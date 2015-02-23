indexInVector<-function(Vector=NA,Element=NA){
    options(warn=-1)
    if (!is.na(Vector)){
        VL<-length(Vector)
        indexByName<-(1:VL)
        names(indexByName)<-Vector
        if (!is.na(Element)){
            index<-indexByName[Element]
            if (!is.na(index)){
                names(index)<-NULL
                options(warn=0)
                return(index)
            }
        }
    }
    ## either Vector or Element were NA, or Element is not in Vector
    options(warn=0)
    return(NA)
}
