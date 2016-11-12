X_it<-function(prev.results){
  # Validar la existencia del objeto con los resultados previos
  if (missing(prev.results)){
    stop("No elementos para iteracion, No elements for iteration")
    } else {
      if(prev.results$bin[1]==0){
        stop("El proceso ya esta bajo control, The process is already under control")
        } else {
          x.1<-prev.results$data.1
          m.1<-nrow(x.1)
          n.1<-ncol(x.1)
          X.prom.1<-apply(x.1,1,mean)
          f.rango.1<-function(x.1){
            f.rango.p.1<-range(x.1)
            return(f.rango.p.1[2]-f.rango.p.1[1])
            }
          X.range.1<-apply(x.1,1,f.rango.1)
          # Cargar las tablas para calcular los limites de control
          data(factor.a, envir = environment())
          # Calcular las expresiones de los limites de control
          # para ser evaluadas en las graficas
          #   Limites de control grafica X
          LCS.X.1<-expression(mean(X.prom.1) + factor.a$A2[n.1-1] * mean(X.range.1))
          LCI.X.1<-expression(mean(X.prom.1) - factor.a$A2[n.1-1] * mean(X.range.1))
          LC.X.1<-expression(mean(X.prom.1))
          X.pos.1 <- which(X.prom.1 > eval(LCI.X.1) & X.prom.1 < eval(LCS.X.1))
          x.2<-x.1[X.pos.1,]
          X.fs.1<-which(X.prom.1 >= eval(LCS.X.1))
          X.fi.1<-which(X.prom.1 <= eval(LCI.X.1))
          X.f<-c(X.fs.1, X.fi.1)
          X.pos<-as.numeric(X.pos.1)
          #   Limites de control grafica R
          LCS.R<-expression(mean(X.range.1)*factor.a$D4[n.1-1])
          LCI.R<-expression(mean(X.range.1)*factor.a$D3[n.1-1])
          LC.R<-expression(mean(X.range.1))
          #
          # Script para Grafica X Final
          plot.X.1<-function(x=X.prom.1,type="b",col="black",pch =19){
            plot(x=x, xlab= "Numero de muestra", ylab="Valores de cada muestra",
                 main="Grafica X, Muestra Final",type=type, col=col,
                 ylim=c(min(eval(LCI.X.1), min(X.prom.1)), max(eval(LCS.X.1), max(X.prom.1))),
                 xlim=c(-0.05*m.1, 1.05*m.1), pch = pch)
            abline(h= c(eval(LCS.X.1), eval(LCI.X.1), eval(LC.X.1)),col="lightgray")
            text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.X.1),eval(LC.X.1),eval(LCI.X.1)),2),
                 c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.X.1),2),
                                                   round(eval(LC.X.1),2),
                                                   round(eval(LCI.X.1),2))), col="red", cex=1.25)
            }
          dev.off()
          R.pos <- which(X.range.1 > eval(LCI.R) & X.range.1 < eval(LCS.R))
          R.pos<- as.numeric(R.pos)
          X.range.2<-X.range.1[R.pos]
          bin.X<-if(length(X.pos)< m.1){
            bin.X<-1
          } else {
            bin.X<-0
          }
          bin.R<-if(length(R.pos)< m.1){
            bin.R<-1
          } else {
            bin.R<-0
          }
          plot.X.1()
          structure(list("in.control" = X.pos,
                         "R.in.control" = R.pos,
                         "out.control" = X.f,
                         "data.0" = prev.results$data.0,
                         "data.1" = x.2,
                         "data.r.1" = X.range.2,
                         "bin" = c(bin.X,bin.R,0),
                         "Iteraciones"= prev.results$Iteraciones + 1,
                         "LX"= c("LCI"=eval(LCI.X.1),  "LC"=eval(LC.X.1),"LCS"=eval(LCS.X.1)),
                         "LR"= c("LCI"=eval(LCI.R), "LC"=eval(LC.R), "LCS"=eval(LCS.R)),
                         "Limites Grafica X" = c("LCI.X"=eval(LCI.X.1),"LC.X"=eval(LC.X.1),"LCS.X"=eval(LCS.X.1)),
                         "Limites Grafica R" = c("LCI.R"=eval(LCI.R), "LC.R"=eval(LC.R), "LCS.R"=eval(LCS.R)),
                         "Conclusion del proceso"= c(if(length(X.pos)< m.1){
                           print("Proceso fuera de Control en Grafica X")
                           } else {
                             print("El proceso esta bajo control en Grafica X")
                             }, if(length(R.pos)< m.1){
                               print("Proceso fuera de control en Grafica R")
                               } else {
                                 print("El proceso esta bajo control en Grafica R")
                               })))
        }
    }
}
