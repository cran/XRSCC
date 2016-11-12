c_gr<-function(C){
  if (missing(C)){
    stop("No hay muestras para leer, No sample to read")
  } else {
    c <- C
    c.0<-C[,1]
    m <- length(c.0)
    # Calculo de limites de control para la grafica c
          LCS.c.0<-expression(mean(c.0)+3*sqrt(mean(c.0)))
          LCI.c.0<-expression(mean(c.0)-3*sqrt(mean(c.0)))
          LC.c.0<-expression(mean(c.0))
          if (eval(LCI.c.0)>0){
            LCI.c.0<-eval(LCI.c.0)
          } else {
            LCI.c.0 <- 0
          }
          c.pos<-which(c.0 >= eval(LCI.c.0)  & c.0 < eval(LCS.c.0))
          c.1<-c.0[c.pos]
          c.fi.0<-which(c.0 < eval(LCI.c.0))
          c.fs.0<-which(c.0 >= eval(LCS.c.0))
          bin.c<-if(length(c.pos)< m){
            bin.c<-1
          } else {
            bin.c<-0
          }
          #
          # Script para Grafica c inicial
          plot.c<-function(C=c.0,type="b",col="blue",pch =19){
            plot(C, xlab= "Numero de muestra", ylab="Numero de inconformidades",
                 main="Grafica c, Control Estadistico de la Calidad",type=type, col=col,
                 ylim=c(eval(LCI.c.0)-mean(c.0)*0.05, max(eval(LCS.c.0)*1.1, max(c.0)*1.1)),
                 xlim=c(-0.05*m, 1.05*m), pch = pch)
            abline(h= c(eval(LCS.c.0), eval(LCI.c.0), eval(LC.c.0)),col="lightgray")
            text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.c.0),eval(LC.c.0),eval(LCI.c.0)),2),
                 c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.c.0),3), round(eval(LC.c.0),3),
                                                   round(eval(LCI.c.0),3))),
                 col="red") }
          plot.c()
          # Crea la lista de los resultados
          structure(list("in.control" = c.pos,
                         "out.control"= c(c.fi.0,c.fs.0),
                         "Iteraciones" = 1,
                         "data.0"= C,
                         "data.1"= c.1,
                         "bin" = bin.c,
                         "Limites de Control Grafica c" = c("LCI.c"=eval(LCI.c.0),"LCS.c"=eval(LCS.c.0),
                                                            "LC.p"=eval(LC.c.0)),
                         "Conclusion del proceso"= c(if(length(c.pos)< m){
                           print("Proceso fuera de Control en Grafica c")
                         } else {
                           print("El proceso esta bajo control en Grafica c")
                         })))
  }
}
