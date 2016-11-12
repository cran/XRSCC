np_gr<-function(D,n){
  if (missing(D)){
    stop("No hay muestras para leer, No sample to read")
  } else {
    if (missing(n)){
      stop("Debe fijar un ancho de la muestra")
    } else {
      p.0<-D[,1]/n
      np.0<-D[,1]
      if(max(p.0) >= 1){
        stop("No puede existir una proporcion mayor a la unidad")
      } else {
        m <-nrow(D)
        # Calculo de limites de control para la grafica P
        LCS.np.0<-expression(n*mean(p.0)+3*sqrt(n*mean(p.0)*(1-mean(p.0))))
        LCI.np.0<-expression(n*mean(p.0)-3*sqrt(n*mean(p.0)*(1-mean(p.0))))
        LC.np.0<-expression(n*mean(p.0))
        if (eval(LCI.np.0)>0){
          LCI.p.0<-eval(LCI.np.0)
        } else {
          LCI.np.0 <- 0
        }
        np.pos<-which(np.0 >= eval(LCI.np.0)  & np.0 < eval(LCS.np.0))
        np.1<-np.0[np.pos]
        np.fi.0<-which(np.0 < eval(LCI.np.0))
        np.fs.0<-which(np.0 >= eval(LCS.np.0)) # Solo valores positivos
        bin.np<-if(length(np.pos)< m){
          bin.np<-1
        } else {
          bin.np<-0
        }
        #
        # Script para Grafica p inicial
        plot.np<-function(NP=np.0,type="b",col="blue",pch =19){
          plot(NP, xlab= "Numero de muestra", ylab="Numero de No conformes",
               main="Grafica NP, Control Estadistico de la Calidad",type=type, col=col,
               ylim=c(eval(LCI.np.0)-mean(np.0)*0.05, max(eval(LCS.np.0)*1.1, max(np.0)*1.1)),
               xlim=c(-0.05*m, 1.05*m), pch = pch)
          abline(h= c(eval(LCS.np.0), eval(LCI.np.0), eval(LC.np.0)),col="lightgray")
          text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.np.0),eval(LC.np.0),eval(LCI.np.0)),2),
               c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.np.0),3), round(eval(LC.np.0),3),
                                                 round(eval(LCI.np.0),3))),
               col="red") }
        plot.np()
        # Crea la lista de los resultados
        structure(list("in.control" = np.pos,
                       "out.control"= c(np.fi.0,np.fs.0),
                       "Iteraciones" = 1,
                       "data.n"=n,
                       "data.0"= D,
                       "data.1"= np.1,
                       "bin" = bin.np,
                       "Limites de Control Grafica np" = c("LCI.np"=eval(LCI.np.0),"LCS.np"=eval(LCS.np.0),
                                                          "LC.np"=eval(LC.np.0)),
                       "Conclusion del proceso"= c(if(length(np.pos)< m){
                         print("Proceso fuera de Control en Grafica np")
                       } else {
                         print("El proceso esta bajo control en Grafica np")
                       })))
      }
    }
  }
}
