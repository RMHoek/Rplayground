maintainMedian<-function(){
    source("minHeap.R")
    source("maxHeap.R")
    options(warn=-1) ## suppress warnings temporarily, to change mortality data back to numeric
    theData<-readLines("median.txt", n=-1)
    ##unlink("median.txt")
    theData<- as.integer(theData)
    options(warn=0) ## now turn on warnings again
    loHeap<-maxHeap()
    hiHeap<-minHeap()
    loHeap$insert(theData[1])
    grandTotal<-theData[1]
    ##runningMedian<-theData[1]
    for (i in 2:length(theData)){
        maxLo<-loHeap$inspect(1)
        if ((i-1)%%2==0){
            if (theData[i]>maxLo){
                hiHeap$insert(theData[i])
                loHeap$insert(hiHeap$extract())
            } else {
                loHeap$insert(theData[i])
            }
        } else {
            if (theData[i]>maxLo){
                hiHeap$insert(theData[i])
            } else {
                loHeap$insert(theData[i])
                hiHeap$insert(loHeap$extract())
            }
        }
        grandTotal<-grandTotal+loHeap$inspect(1)
        ##runningMedian<-c(runningMedian,as.integer(loHeap$inspect(1)))
    }
    grandTotal %% 10000
    ##runningMedian
}
