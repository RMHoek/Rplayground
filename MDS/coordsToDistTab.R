coordsToDistTab<-function (Coords){
    N<-dim(Coords)[1]
    Mx<-matrix(0,N,N)
    My<-matrix(0,N,N)
    for (i in 1:N){
        Mx[,i]<-Coords[,1]-Coords[i,1]
        My[,i]<-Coords[,2]-Coords[i,2]
    }
    M<-sqrt((Mx*Mx)+(My*My))
    return(M)
}
