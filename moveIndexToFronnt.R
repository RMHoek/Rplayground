moveIndexToFront<-function(Matrix=NA, index=0){
    options(warn=-1)
    if (!is.na(Matrix)){
        len<-dim(Matrix)[1]
        if (index!=0){
            if (index==1){
                options(warn=0)
                return(Matrix)
            }
            NCN<-""
            if (length(colnames(Matrix))!=0){
                CN<-colnames(Matrix)
                NCN<-c(CN[index],CN[1:(index-1)])
                if (index<length(colnames(Matrix))){
                    NCN<-c(NCN,CN[(index+1):len])
                }
            }
            leftOfIndex<-Matrix[,1:(index-1)]
            colAtIndex<-Matrix[,index]
            if (index<len){
                rightOfIndex<-Matrix[,(index+1):len]
                NMatrix<-cbind(colAtIndex,leftOfIndex,rightOfIndex)
            } else {
                NMatrix<-cbind(colAtIndex,leftOfIndex)
            }
            topOfIndex<-NMatrix[1:(index-1),]
            rowAtIndex<-NMatrix[index,]
            if (index<len){
                bottomOfIndex<-NMatrix[(index+1):len,]
                Matrix<-rbind(rowAtIndex,topOfIndex,bottomOfIndex)
            } else {
                Matrix<-rbind(rowAtIndex,topOfIndex)
            }
            if (NCN!=""){
                colnames(Matrix)<-NCN
            }
        }
        options(warn=0)
        rownames(Matrix)<-NULL
        return(Matrix)
    }
    options(warn=0)
    return(NA)
}
