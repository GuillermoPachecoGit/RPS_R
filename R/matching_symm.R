
#Lee del dir de trabajo un archivo (por defecto llamado "side.txt") que es una lista del lado de cada config de A (DEBE usar L y R para izquierda y derecha)

#' Description
#' -----------
#' This function obtains the individual resistant-symmetric shape for 2D matching
#' symmetry data. The input is an array A of size
#' n (landmarks) x p (dimensions) x 2k (objects: left-right sides each)
#' Configurations are ordered in this way: left side Object 1, right side Object 1,
#' left side Object 2, right side Object 2, etc
#' @param A input data, an array or matrix of size n x 2 x 2k
#' @param ctr s
#' @param side.file s
#' @param legend.loc s
#'
#' @return
#'
#' @author Guillermo A. Pacheco, Federico Lotto, Viviana Ferraggine, Sebastian Torcida
#' @export
matching.symm<-function(A,ctr="gmedian",side.file,legend.loc="topleft"){

  side<-read.table(side.file,sep=" ",header=F)
  p<-length(A[1,,1]) # The dimension of data. By default is p=2 (two dimensional shapes)
  n<-length(A[,1,1]) # The number of landmarks
  k<-dim(A)[3]/2     # The number of configurations (objects)

  # Initial centering----------------
  # Parameter "ctr" can take values:
  #   "gmedian"(the spatial or gemetric median)"
  #   "median" (the componentwise median)
  #   "mean" (the mean or average)
  Mc<-center(A,cent=ctr)

  #Side arrays are created----------
  Mcl<-Mc[,,side=="L"]
  Mcr<-Mc[,,side=="R"]

  dv<-Mcl-Mcr  # Difference vectors between corresponding left-right sided lanmarks

  ang.par<-atan(dv[,2,]/dv[,1,]) # Angle (in radians) between the diff. vectors and the horizontal axis
  par.mat<-apply(ang.par,c(1,2),function(x)if(x<0) {pi-abs(x)}else{x}) # Leaves angles in the range [0,pi]

  dr<-apply(par.mat,2,median) # The median angle of the diff. vectors
  vr<-cbind(cos(dr),sin(dr))  # Computes the spherical median direction

  # Computes the Househölder reflection matrix using the spherical median as mirror
  Ur<-array(0,dim(Mcl))
  for(i in 1:k){
    R<-diag(2) - 2*vr[i,]%*%t(vr[i,])
    Ur[,,i] <- Mcl[,,i]%*%R  # Performs the corresponding reflection
  }

  S<-array(t(c(Mcr,Ur)),c(n,2,(k*2)))
  print("please wait...",quote = F)
  sink("/dev/null")
  U<-robgit(S) # A resistan fit to filter out possible differences in size and/or orientation between sides
  sink()
  Ui<-U[,,1:k]
  Ud<-U[,,(k+1):(k*2)]

  # Computes each landmark contribution percentage (%) to total asymmetry
  distances<-dist.contrib(mc=Ui,mre=Ud)

  # Plots
  plot.result(mc=Ud, mre =Ui, nconf = k,legloc = legend.loc,object=F)
  return(list(distances,Ui-Ud))
}

center<-function(A,cent){
    ni<-dim(A)[3]
    switch(cent,
         mean={Mc<-apply(A,c(2,3), function(x) x-mean(x))},
         median={Mc<-apply(A,c(2,3), function(x) x-median(x))},
         gmedian={Mc<-array(0,c(dim(A)))
         gmed<-t(apply(A,c(3), function(x) Weiszfeld(x)))
         for(i in 1:ni){
           Mc[,,i]<-t(t(A[,,i])-c(gmed[[i]][[1]]))
         }
         }
  )
  return(Mc)
}


dist.contrib<-function(mc=NULL,mre=NULL){

  eudist<-sqrt(((mc[,1,]-mre[,1,])^2)+((mc[,2,]-mre[,2,])^2))

  suma<-colSums(eudist) # Computes total shape difference (asymmetry)
  perc<-round((eudist/suma)*100,digits=4) # Contribution percentage

  results<-array(0,c(dim(mc)))
  results[,1,]<-round(eudist,digits=6)
  results[,2,]<-perc

  print("Distance and contribution % to total (euclidean) distance between sides for each configuration:",quote = F)
  print(results)
  return(results)
}


plot.result<-function(mc=NULL,mre=NULL,mt=NULL,nconf,object=TRUE,legloc="topleft"){
  conf<-1:nconf
  if(object[1]){
    for(c in conf){
      plot(mc[,,c],asp=1,xlab = "",ylab = "",main = paste("Config",c)) # Original data
      points(0,0,pch=3) # Center in (0,0)
      points(mre[,,c],pch=20) # Reflected
      points((mt[,,c]+mre[,,c]),col="red",pch=20,cex=0.8) # Symmetric (residual + reflected)
      text(mc[,1,c],mc[,2,c],c(seq(1:(dim(mc)[1]))),pos=2,offset=0.5,cex=0.55) # Landmark number
      legend(x=legloc,c("Original","Reflected","Symmetric"),pch=c(1,20,20) ,col = c("black","black","red"), cex=0.7)
    }

  }else{

    for(c in conf){
      plot(mc[,,c],asp=1,xlab = "",ylab = "",main = paste("Config",c)) # original data
      points(0,0,pch=3) # Center in (0,0)
      points(mre[,,c],col="red",pch=20,cex=0.8) # Reflected
      text(mc[,1,c],mc[,2,c],c(seq(1:dim(mc)[1])),pos=2,offset=0.5,cex=0.55)
      legend(x=legloc,c("Left","Right"),pch=c(20,1) ,col = c("black","red"), cex=0.7)
    }
  }

}

