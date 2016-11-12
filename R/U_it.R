U_it<-function(prev.results){
  if (missing(prev.results)){
    stop("No elementos para iteracion, No elements for iteration")
  } else {
    if(prev.results$bin[1]==0){
      stop("El proceso ya esta bajo control, The process is already under control")
    } else {
          u.0 <- prev.results$data.0
          u.pos<-prev.results$in.control
          u.1<-u.0[u.pos,]
          m <- nrow(u.1)
          ui.1 <- u.1$d/u.1$n
          # Calculo de limites de control para la grafica u
          LCS.u.1<-expression(mean(ui.1)+3*sqrt(mean(ui.1)/mean(u.1$n)))
          LCI.u.1<-expression(mean(ui.1)-3*sqrt(mean(ui.1)/mean(u.1$n)))
          LC.u.1<-expression(mean(ui.1))
          if (eval(LCI.u.1)>0){
            LCI.u.1<-eval(LCI.u.1)
          } else {
            LCI.u.1 <- 0
          }
          u.pos<-which(ui.1 >= eval(LCI.u.1)  & ui.1 < eval(LCS.u.1))
          ui.2<-ui.1[u.pos]
          u.fi.1<-which(ui.1 < eval(LCI.u.1))
          u.fs.1<-which(ui.1 >= eval(LCS.u.1))
          bin.u<-if(length(u.pos)< m){
            bin.u<-1
          } else {
            bin.u<-0
          }
          #
          # Script para Grafica U inicial
          plot.u<-function(U=ui.1,type="b",col="blue",pch =19){
            plot(U, xlab= "Numero de muestra", ylab="Numero de inconformidades por unidad",
                 main="Grafica U, Control Estadistico de la Calidad",type=type, col=col,
                 ylim=c(eval(LCI.u.1)-mean(ui.1)*0.05, max(eval(LCS.u.1)*1.1, max(ui.1)*1.1)),
                 xlim=c(-0.05*m, 1.05*m), pch = pch)
            abline(h= c(eval(LCS.u.1), eval(LCI.u.1), eval(LC.u.1)),col="lightgray")
            text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.u.1),eval(LC.u.1),eval(LCI.u.1)),2),
                 c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.u.1),3), round(eval(LC.u.1),3),
                                                   round(eval(LCI.u.1),3))),
                 col="red") }
          plot.u()
          # Crea la lista de los resultados
          structure(list("in.control" = u.pos,
                         "out.control"= c(u.fi.1,u.fs.1),
                         "Iteraciones" = prev.results$Iteraciones + 1,
                         "data.0"= prev.results$data.0,
                         "data.1"= ui.2,
                         "bin" = bin.u,
                         "Limites de Control Grafica U" = c("LCI.u"=eval(LCI.u.1),"LCS.u"=eval(LCS.u.1),
                                                            "LC.p"=eval(LC.u.1)),
                         "Conclusion del proceso"= c(if(length(u.pos)< m){
                           print("Proceso fuera de Control en Grafica U")
                         } else {
                           print("El proceso esta bajo control en Grafica U")
                         })))
    }
  }
}
