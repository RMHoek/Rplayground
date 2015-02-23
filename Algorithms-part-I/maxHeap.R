maxHeap <- function(x = numeric()) {
    Heap<-NULL

    insert <- function(y){
        if (length(y)==0) return()
        for (i in 1:length(y)){
            Heap<<-c(Heap,y[i])
            bubbleUp(length(Heap))
        }
    }

    extract <- function(){
        if (length(Heap)==0) return(NA)
        out<-Heap[1]
        delete(1)
        out
    }

    delete <- function(pos){
        if (length(pos)>1){
            print("Warning: can only delete one element at a time!")
            print("Delete was ignored!")
            return()
        }
        if (pos<1|pos>length(Heap)) {
            print("Warning: heap pointer out of bounds!")
            print("Delete was ignored!")
            return()
        }
        if (pos<length(Heap)){
            lastValue<-Heap[length(Heap)]
            Heap[pos]<<-lastValue
            Heap<<-head(Heap, -1)
            parent<-floor(pos/2)
            if (pos==1) {
                bubbleDown(pos)
            }else if (Heap[parent]>lastValue){
                bubbleDown(pos)
            } else {
                bubbleUp(pos)
            }
        } else {
            Heap<<-head(Heap, -1)
        }
    }

    inspect <- function(pos){
        if (length(Heap)==0) return(NA)
        if (length(pos)==1){
            Heap[pos]
        } else{
            out<-NULL
            for (i in 1:length(pos)){
                out<-c(out, Heap[pos[i]])
            }
            return(out)
        }
    }

    n.elements <- function(){
        length(Heap)
    }

    bubbleUp <- function(pos){
        parent<-floor(pos/2)
        if (parent>0){
            pValue<-Heap[parent]
            cValue<-Heap[pos]
            if (cValue>pValue){
                Heap[pos]<<-pValue
                Heap[parent]<<-cValue
                bubbleUp(parent)
            }
        }
    }

    bubbleDown <- function(pos){
        bubbleTo<--1
        leftChild<-2*pos
        rightChild<-2*pos+1
        if (leftChild>length(Heap)) return()
        pValue<-Heap[pos]
        if (rightChild<=length(Heap)){
            if (Heap[pos]<Heap[leftChild]|Heap[pos]<Heap[rightChild]){
                if (Heap[leftChild]>Heap[rightChild]){
                    bubbleTo<-leftChild
                } else {
                    bubbleTo<-rightChild
                }
            }
        } else {
            if (Heap[pos]<Heap[leftChild]){
                bubbleTo<-leftChild
            }
        }
        if (bubbleTo>-1){
            cValue<-Heap[bubbleTo]
            Heap[pos]<<-cValue
            Heap[bubbleTo]<<-pValue
            bubbleDown(bubbleTo)
        }
    }
    list(insert=insert, extract=extract,
        delete=delete, inspect=inspect,
        n.elements=n.elements,
        bubbleUp=bubbleUp, bubbleDown=bubbleDown)
}
