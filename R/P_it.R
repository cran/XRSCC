P_it<-function(prev.results){
  if (missing(prev.results)){
    stop("No elementos para iteracion, No elements for iteration")
  } else {
    if(prev.results$bin[1]==0){
      stop("El proceso ya esta bajo control, The process is already under control")
    } else {
      p.0<-prev.results$data.1
      m <-length(p.0)
      n <-prev.results$data.n
      # Calculo de limites de control para la grafica P
      LCS.p.0<-expression(mean(p.0)+3*sqrt((mean(p.0)*(1-mean(p.0)))/n))
      LCI.p.0<-expression(mean(p.0)-3*sqrt((mean(p.0)*(1-mean(p.0)))/n))
      LC.p.0<-expression(mean(p.0))
      if (eval(LCI.p.0)>0){
        LCI.p.0<-eval(LCI.p.0)
        } else {
          LCI.p.0 <- 0
          }
      p.pos<-which(p.0 > eval(LCI.p.0)  & p.0 < eval(LCS.p.0))
      p.1<-p.0[p.pos]
      p.fi.0<-which(p.0 < eval(LCI.p.0))
      p.fs.0<-which(p.0 >= eval(LCS.p.0))
      bin.p<-if(length(p.pos)< m){
        bin.p<-1
        } else {
          bin.p<-0
          }
      #
      # Script para Grafica p inicial
      plot.p<-function(P=p.0,type="b",col="blue",pch =19){
        plot(P, xlab= "Numero de muestra", ylab="Proporcion de los no conformes de cada muestra",
             main="Grafica P, Control Estadistico de la Calidad",type=type, col=col,
             ylim=c(eval(LCI.p.0)-mean(p.0)*0.05, max(eval(LCS.p.0)*1.1, max(p.0)*1.1)),
             xlim=c(-0.05*m, 1.05*m), pch = pch)
        abline(h= c(eval(LCS.p.0), eval(LCI.p.0), eval(LC.p.0)),col="lightgray")
        text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.p.0),eval(LC.p.0),eval(LCI.p.0)),2),
             c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.p.0),3), round(eval(LC.p.0),3),
                                               round(eval(LCI.p.0),3))),
             col="red") }
      plot.p()
      # Crea la lista de los resultados
      structure(list("in.control" = p.pos,
                     "out.control"= c(p.fi.0, p.fs.0),
                     "Iteraciones" = prev.results$Iteraciones + 1,
                     "data.n"= prev.results$data.n,
                     "data.0"= prev.results$data.0,
                     "data.1"= p.1,
                     "bin" = bin.p,
                   "Limites de Control Grafica p" = c("LCI.p"=eval(LCI.p.0),"LCS.p"=eval(LCS.p.0),
                                                      "LC.p"=eval(LC.p.0)),
                   "Conclusion del proceso"= c(if(length(p.pos)< m){
                     print("Proceso fuera de Control en Grafica p")
                     } else {
                       print("El proceso esta bajo control en Grafica p")
                       })))
    }
  }
}
