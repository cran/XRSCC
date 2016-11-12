u_gr<-function(U){
  if (missing(U)){
    stop("No hay muestras para leer, No sample to read")
  } else {
    if (dim(U)[2]!=2){
      stop("La cantidad de variables no es la correcta, The number of variables is not correct")
    } else {
      if(((any(names(U)=="n")) & (any(names(U)=="d")))==FALSE){
        stop("Las variables no estan especificadas con el nombre correcto")
        } else {
          if(any(U$n==0)){
            stop("No puede existir alguna muestra igual a cero")
            } else {
              u.0 <- U
              m <- nrow(u.0)
              ui.0 <- u.0$d/u.0$n
              # Calculo de limites de control para la grafica u
              LCS.u.0<-expression(mean(ui.0)+3*sqrt(mean(ui.0)/mean(u.0$n)))
              LCI.u.0<-expression(mean(ui.0)-3*sqrt(mean(ui.0)/mean(u.0$n)))
              LC.u.0<-expression(mean(ui.0))
              if (eval(LCI.u.0)>0){
                LCI.u.0<-eval(LCI.u.0)
                } else {
                  LCI.u.0 <- 0
                  }
              u.pos<-which(ui.0 >= eval(LCI.u.0)  & ui.0 < eval(LCS.u.0))
              ui.1<-ui.0[u.pos]
              u.fi.0<-which(ui.0 < eval(LCI.u.0))
              u.fs.0<-which(ui.0 >= eval(LCS.u.0))
              bin.u<-if(length(u.pos)< m){
                bin.u<-1
                } else {
                  bin.u<-0
                  }
              #
              # Script para Grafica U inicial
              plot.u<-function(U=ui.0,type="b",col="blue",pch =19){
                plot(U, xlab= "Numero de muestra", ylab="Numero de inconformidades por unidad",
                     main="Grafica U, Control Estadistico de la Calidad",type=type, col=col,
                     ylim=c(eval(LCI.u.0)-mean(ui.0)*0.05, max(eval(LCS.u.0)*1.1, max(ui.0)*1.1)),
                     xlim=c(-0.05*m, 1.05*m), pch = pch)
                abline(h= c(eval(LCS.u.0), eval(LCI.u.0), eval(LC.u.0)),col="lightgray")
                text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.u.0),eval(LC.u.0),eval(LCI.u.0)),2),
                     c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.u.0),3), round(eval(LC.u.0),3),
                                                       round(eval(LCI.u.0),3))),
                     col="red") }
              plot.u()
              # Crea la lista de los resultados
              structure(list("in.control" = u.pos,
                             "out.control"= c(u.fi.0,u.fs.0),
                             "Iteraciones" = 1,
                             "data.0"= U,
                             "data.1"= ui.1,
                             "bin" = bin.u,
                             "Limites de Control Grafica U" = c("LCI.u"=eval(LCI.u.0),"LCS.u"=eval(LCS.u.0),
                                                                "LC.u"=eval(LC.u.0)),
                             "Conclusion del proceso"= c(if(length(u.pos)< m){
                               print("Proceso fuera de Control en Grafica U")
                               } else {
                                 print("El proceso esta bajo control en Grafica U")
                                 })))
            }
        }
    }
  }
}
