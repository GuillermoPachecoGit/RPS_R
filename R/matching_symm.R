#Esta funcion acepta un array A de dimensiones  pxkx2*n (n de landmarks, dimensiones, por ahora 2, y n de especimenes, cada uno con dos configuraciones)
#Las configuraciones deben estar ordenadas por individuo: lado I del ind 1, luego lado D del ind 1, etc...
#Lee del dir de trabajo un archivo (por defecto llamado "side.txt") que es una lista del lado de cada config de A (DEBE usar L y R para izquierda y derecha)
#ctr puede tomar los valores "gmedian"(mediana espacial", "median" (mediana cac) y "mean" (media))

#' Description
#'
#' @param A matrix input
#' @param ctr s
#' @param side.file s
#' @param legend.loc s
#'
#' @return
#'
#' @author Guillermo A. Pacheco, Viviana Ferraggine, Federico Lotto, Sebastian Torcida
#' @export
matching.symm<-function(A,ctr="gmedian",side.file,legend.loc="topleft"){

  side<-read.table(side.file,sep=" ",header=F)
  nd<-length(A[1,,1])#numero de dimensiones
  nl<-length(A[,1,1])#numero de landmarks
  n<-dim(A)[3]/2

  #--------------------------centrado----------------
  Mc<-center(A,cent=ctr)

  #---------- crea arrays por lado

  Mcl<-Mc[,,side=="L"]
  Mcr<-Mc[,,side=="R"]

  dv<-Mcl-Mcr

  ang.par<-atan(dv[,2,]/dv[,1,])#angulo en radianes
  par.mat<-apply(ang.par,c(1,2),function(x)if(x<0) {pi-abs(x)}else{x})#esto hace que los ángulos sean todos positivos entre 0 y pi

  dr<-apply(par.mat,2,median)#saca la dirección mediana (aún en radianes)
  vr<-cbind(cos(dr),sin(dr))#vectores unitarios definidos por sus proyecciones sobre x e y a partir de seno y coseno del angulo de menores proyecciones

  #calcula y aplica la matriz de Householder
  Ur<-array(0,dim(Mcl))
  for(i in 1:n){
    R<-diag(2) - 2*vr[i,]%*%t(vr[i,])
    Ur[,,i] <- Mcl[,,i]%*%R
  }

  S<-array(t(c(Mcr,Ur)),c(nl,2,(n*2)))
  print("please wait...",quote = F)
  sink("/dev/null")
  U<-robgit(S)#un ajuste robusto para eliminar pequeñas diferencias
  sink()
  Ui<-U[,,1:n]
  Ud<-U[,,(n+1):(n*2)]

  #saca contrib % de cada punto
  distances<-dist.contrib(mc=Ui,mre=Ud)

  #plots
  plot.result(mc=Ud, mre =Ui, nconf = n,legloc = legend.loc,object=F)
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

  suma<-colSums(eudist)#sumatoria de distancias
  perc<-round((eudist/suma)*100,digits=4)#porcentual por punto

  results<-array(0,c(dim(mc)))
  results[,1,]<-round(eudist,digits=6)
  results[,2,]<-perc

  print("Distance and % contribution to total (euclidean) distance between sides for each config:",quote = F)
  print(results)
  return(results)
}


plot.result<-function(mc=NULL,mre=NULL,mt=NULL,nconf,object=TRUE,legloc="topleft"){
  conf<-1:nconf
  if(object[1]){
    for(c in conf){
      plot(mc[,,c],asp=1,xlab = "",ylab = "",main = paste("Config",c))#original
      points(0,0,pch=3)#centro (0,0)
      points(mre[,,c],pch=20)#reflejada
      points((mt[,,c]+mre[,,c]),col="red",pch=20,cex=0.8)#simétrica (residuos + reflejada)
      text(mc[,1,c],mc[,2,c],c(seq(1:(dim(mc)[1]))),pos=2,offset=0.5,cex=0.55)#numero de landmark
      legend(x=legloc,c("Original","Reflected","Symmetric"),pch=c(1,20,20) ,col = c("black","black","red"), cex=0.7)
    }

  }else{

    for(c in conf){
      plot(mc[,,c],asp=1,xlab = "",ylab = "",main = paste("Config",c))#original
      points(0,0,pch=3)#centro (0,0)
      points(mre[,,c],col="red",pch=20,cex=0.8)#reflejada
      text(mc[,1,c],mc[,2,c],c(seq(1:dim(mc)[1])),pos=2,offset=0.5,cex=0.55)
      legend(x=legloc,c("Left","Right"),pch=c(20,1) ,col = c("black","red"), cex=0.7)
    }
  }

}

