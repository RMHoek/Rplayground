MultiDimensionalScaling<-function(
    matrix,
    originAtIndex=1,originAtName="",
    nextIndexWest=2,westByName="",
    nextIndexNorth=3,northByName="",
    withPlot=TRUE){
    
    #############################################
    #         MultiDimensionalScaling           #
    #         R function by R.M. Hoek           #
    #############################################
    #        This function returns              #
    #         a matrix with a list of           #
    # coordinates for all the elements in the   #
    #      distance-table passed to it as       #
    #               a matrix                    #
    # it uses two other functions:              #
    # 'indexInVector' and 'moveIdexToFront'     #
    #############################################
    # supply a distance table as matrix, than   #
    # choose a column (by col name or index) to #
    # be at the origin, and a column (by col    #
    # name or index) with x>0 and y=0 and       #
    # choose a column (by col name or index)    #
    # with y>0.                                 #
    #############################################
    
    M<-matrix*matrix
    cn<-colnames(matrix)
    OAI<-0
    if (originAtName!="" & length(cn)!=0){
        OAI<-indexInVector(cn,originAtName)
        if (is.na(OAI)){OAI<-0}#element'originAtName' is not in colnames(M)
    }
    if (OAI==0 & (originAtIndex!=0|originAtIndex>dim(M)[1])){OAI<-originAtIndex}
    if (OAI==0){OAI<-1}
    #at this stage neither originAtIndex or originAtName were valid so first
    #element in M is used for origin
    NIW<-0
    if (westByName!="" & length(cn)!=0){
        NIW<-indexInVector(cn,westByName)
        if (is.na(NIW)){NIW<-0}#element'westByName' is not in colnames(M)
    }
    if (NIW==0 & (nextIndexWest!=0|nextIndexWest>dim(M)[1])){NIW<-nextIndexWest}
    if (NIW==0){NIW<-2}
    #at this stage neither nextIndexWest or westByName were valid so second
    #element in M is used for west
    NIN<-0
    if (northByName!="" & length(cn)!=0){
        NIN<-indexInVector(cn,northByName)
        if (is.na(NIN)){NIN<-0}#element'northByName' is not in colnames(M)
    }
    if (NIN==0 & (nextIndexNorth!=0|nextIndexNorth>dim(M)[1])){NIN<-nextIndexNorth}
    if (NIN==0){NIWN-3}
    #at this stage neither nextIndexNorth or northByName were valid so third
    #element in M is used for north
    CW<-0
    CO<-0
    if(NIW<NIN){CW<-CW+1}
    if(OAI<NIN){CO<-CO+1}
    if(OAI<NIW){CO<-CO+1}
    NIW<-NIW+CW
    OAI<-OAI+CO
    if (!(OAI==3&NIW==3&NIN==3)){ #no swap needed if all ==3!
        M<-moveIndexToFront(M,NIN)
        M<-moveIndexToFront(M,NIW)
        M<-moveIndexToFront(M,OAI)
    }
    cn<-colnames(M)
    ML<-dim(M)[1]
    L<-matrix(data=0,nrow=ML,ncol=2)
    a<-M[1,2]#the distance from the origin to the second point due west
    L[2,1]<-sqrt(a)
    for(i in 3:ML){
        b<-M[1,i] #the distance from the origin to the i-th point
        c<-M[2,i] #the distance from the point due west of origin to i-th point
        d<-M[3,i] #the distance from the point north of origin to i-th point
        x<-((a+b-c)/(2*sqrt(a))) #the x-coord of the i-th point rel to origin
        yp<-sqrt(abs(((4*a*c)-(a+c-b)^2)/(4*a))) #the poss y-coord of i-th point
        yn<-(-yp) #alternative y-coord of i-th point
        if(i>3){ #i==3 represents point due north
            tp<-sqrt((x-L[3,1])^2+(yp-L[3,2])^2) #distance of i-th point to point
            tn<-sqrt((x-L[3,1])^2+(yn-L[3,2])^2) #to north with either yp or yn
            diffp<-sqrt(d)-tp #diff in dist point north-> i-th point if yp
            diffn<-sqrt(d)-tn #diff in dist point north-> i-th point if yn
            if (abs(diffp)-abs(diffn)<0){ #either of these should be close to 0
                y<-yp
            } else {
                y<-yn
            }
        } else {
            y<-yp
        }
        L[i,1]<-x #add x-coord to table
        L[i,2]<-y #add y-coord to table
    }
    rownames(L)<-cn
    xcoord<-L[,1]
    ycoord<-L[,2]
    if (withPlot){
        locMap<-paste(rep("+", times=ML),cn,sep="")
        plot(xcoord,ycoord, type="n")
        text(xcoord,ycoord,locMap,cex=0.8)
    }
    return(L)
}
