C_it<-function(prev.results){
  if (missing(prev.results)){
    stop("No hay muestras para leer, No sample to read")
  } else {
    if(prev.results$bin[1]==0){
      stop("El proceso ya esta bajo control, The process is already under control")
      } else {
        c.1 <- prev.results$data.1
        m <- length(c.1)
        # Calculo de limites de control para la grafica c
        LCS.c.1<-expression(mean(c.1)+3*sqrt(mean(c.1)))
        LCI.c.1<-expression(mean(c.1)-3*sqrt(mean(c.1)))
        LC.c.1<-expression(mean(c.1))
        if (eval(LCI.c.1)>0){
          LCI.c.1<-eval(LCI.c.1)
          } else {
            LCI.c.1 <- 0
            }
        c.pos<-which(c.1 >= eval(LCI.c.1)  & c.1 < eval(LCS.c.1))
        c.2<-c.1[c.pos]
        c.fi.1<-which(c.1 < eval(LCI.c.1))
        c.fs.1<-which(c.1 >= eval(LCS.c.1))
        bin.c<-if(length(c.pos)< m){
          bin.c<-1
          } else {
            bin.c<-0
            }
        #
        # Script para Grafica c inicial
    plot.c<-function(C=c.1,type="b",col="blue",pch =19){
      plot(C, xlab= "Numero de muestra", ylab="Numero de inconformidades",
           main="Grafica c, Control Estadistico de la Calidad",type=type, col=col,
           ylim=c(eval(LCI.c.1)-mean(c.1)*0.05, max(eval(LCS.c.1)*1.1, max(c.1)*1.1)),
           xlim=c(-0.05*m, 1.05*m), pch = pch)
      abline(h= c(eval(LCS.c.1), eval(LCI.c.1), eval(LC.c.1)),col="lightgray")
      text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.c.1),eval(LC.c.1),eval(LCI.c.1)),2),
           c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.c.1),3), round(eval(LC.c.1),3),
                                             round(eval(LCI.c.1),3))),
           col="red") }
    plot.c()
    # Crea la lista de los resultados
    structure(list("in.control" = c.pos,
                   "out.control"= c(c.fi.1,c.fs.1),
                   "Iteraciones" = prev.results$Iteraciones + 1,
                   "data.0"= C,
                   "data.1"= c.2,
                   "bin" = bin.c,
                   "Limites de Control Grafica c" = c("LCI.c"=eval(LCI.c.1),"LCS.c"=eval(LCS.c.1),
                                                      "LC.c"=eval(LC.c.1)),
                   "Conclusion del proceso"= c(if(length(c.pos)< m){
                     print("Proceso fuera de Control en Grafica c")
                   } else {
                     print("El proceso esta bajo control en Grafica c")
                   })))
      }
  }
}
