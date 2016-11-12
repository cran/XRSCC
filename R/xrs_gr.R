xrs_gr<-function(X){
  # Validar la existencia del objeto con las muestras
  if (missing(X)){
    stop("No hay muestras para leer, No sample to read")
  } else {
    x<-X
    m<-nrow(x)
    n<-ncol(x)
    X.prom<-apply(x,1,mean)
    f.rango<-function(x){
      f.rango.p<-range(x)
      return(f.rango.p[2]-f.rango.p[1])
    }
    X.range<-apply(x,1,f.rango)
    X.S<-apply(x,1,sd)
    # Cargar las tablas para calcular los limites de control
    data(factor.a, envir = environment())
    # Calcular las expresiones de los limites de control
    # para ser evaluadas en las graficas
    #   Limites de control grafica X
    LCS.X<-expression(mean(X.prom) + factor.a$A2[n-1] * mean(X.range))
    LCI.X<-expression(mean(X.prom) - factor.a$A2[n-1] * mean(X.range))
    LC.X<-expression(mean(X.prom))
    #   Limites de control grafica R
    LCS.R<-expression(mean(X.range)*factor.a$D4[n-1])
    LCI.R<-expression(mean(X.range)*factor.a$D3[n-1])
    LC.R<-expression(mean(X.range))
    #   Limites de control grafica S
    LCS.S<-expression(mean(X.S)*factor.a$B4[n-1])
    LCI.S<-expression(mean(X.S)*factor.a$B3[n-1])
    LC.S<-expression(mean(X.S))
    #
    # Marco de Graficas
    mat<-matrix(1:4,2,2,byrow=TRUE)
    layout(mat)
    layout.show(length(1:4))
    # Mostrar un histograma
    hist.X<-function(x=X.prom,breaks="Sturges"){
      hist(X.prom, breaks = breaks,
           xlab="Valores", ylab="Frecuencia",
           main="Histograma de los promedios")
      }
    # Script para Grafica X
    plot.X<-function(x=X.prom,type="b",col="blue",pch =19){
      plot(x=x, xlab= "Numero de muestra", ylab="Valores de cada muestra",
           main="Grafica X, Control Estadistico de la Calidad",type=type, col=col,
           ylim=c(min(eval(LCI.X), min(X.prom)), max(eval(LCS.X), max(X.prom))),
           xlim=c(-0.05*m, 1.05*m), pch = pch)
      abline(h= c(eval(LCS.X), eval(LCI.X), eval(LC.X)),col="lightgray")
      text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.X),eval(LC.X),eval(LCI.X)),2),
           c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.X),2),
                                             round(eval(LC.X),2),
                                             round(eval(LCI.X),2))), col="red")
      }
    # Script para Grafica R
    plot.R<-function(x=X.range,type="b",col="black",pch =15){
      plot(x=x, xlab= "Numero de muestra", ylab="Rangos de cada muestra",
           main="Grafica R, Control Estadistico de la Calidad",type=type, col=col,
           ylim=c(min(eval(LCI.R)-min(X.range)*0.05, min(X.range)*.95),
                      max(eval(LCS.R)+max(X.range)*0.05, max(X.range)*1.05)),
           xlim=c(-0.05*m, 1.05*m), pch = pch)
      abline(h= c(eval(LCS.R), eval(LCI.R), eval(LC.R)), col="lightgray")
      text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.R),eval(LC.R),eval(LCI.R)),2),
           c(c("LCS = ","LC = ","LCI = "),
             c(round(eval(LCS.R),2), round(eval(LC.R),2),
               round(eval(LCI.R),2))), col="red")
      }
    # Script para Grafica S
    plot.S<-function(x=X.S,type="b",col="gray",pch =15){
      plot(x=x, xlab= "Numero de muestra", ylab="Desviacion estandar de cada muestra",
           main="Grafica S, Control Estadistico de la Calidad",type=type, col=col,
           ylim=c(min(eval(LCI.S)-min(X.S)*0.05, min(X.S)*0.95),
                  max(eval(LCS.S)+max(X.S)*0.05, max(X.S)*1.05)),
           xlim=c(-0.05*m, 1.05*m), pch = pch)
      abline(h= c(eval(LCS.S), eval(LCI.S), eval(LC.S)), col="lightgray")
      text(c(rep(1,3),rep(7,3)), rep(c(eval(LCS.S),eval(LC.S),eval(LCI.S)),2),
           c(c("LCS = ","LC = ","LCI = "),
             c(round(eval(LCS.S),2), round(eval(LC.S),2),
               round(eval(LCI.S),2))), col="red")
      }
    X.pos <- which(X.prom > eval(LCI.X) & X.prom < eval(LCS.X))
    x.1<-x[X.pos,]
    X.fs<- which(X.prom >= eval(LCS.X))
    X.fi<- which(X.prom <= eval(LCI.X))
    X.f<-c(X.fs,X.fi)
    R.pos <- which(X.range > eval(LCI.R) & X.range < eval(LCS.R))
    X.range.1<-X.range[R.pos]
    S.pos <- which(X.S > eval(LCI.S) & X.S < eval(LCS.S))
    bin.X<-if(length(X.pos)< m){
      bin.X<-1
    } else {
      bin.X<-0
    }
    bin.R<-if(length(R.pos)< m){
      bin.R<-1
    } else {
      bin.R<-0
    }
    bin.S<-if(length(S.pos)< m){
      bin.S<-1
    } else {
      bin.S<-0
    }
    hist.X()
    plot.X()
    plot.R()
    plot.S()
  }
  structure(list("in.control" = X.pos,
                 "R.in.control" = R.pos,
                 "out.control" = X.f,
                 "Iteraciones" = 1,
                 "data.0"= x,
                 "data.1"= x.1,
                 "data.r.1" = X.range.1,
                 "bin" = c(bin.X, bin.R, bin.S),
                 "LX"= c("LCI"=eval(LCI.X),  "LC"=eval(LC.X),"LCS"=eval(LCS.X)),
                 "LR"= c("LCI"=eval(LCI.R), "LC"=eval(LC.R), "LCS"=eval(LCS.R)),
                 "LS"= c("LCI"=eval(LCI.S), "LC"=eval(LC.S), "LCS"=eval(LCS.S)),
                 "Limites Grafica X" = c("LCI.X"=eval(LCI.X),  "LC.X"=eval(LC.X),"LCS.X"=eval(LCS.X)),
                 "Limites Grafica R" = c("LCI.R"=eval(LCI.R), "LC.R"=eval(LC.R), "LCS.R"=eval(LCS.R)),
                 "Limites Grafica S" = c("LCI.S"=eval(LCI.S), "LC.S"=eval(LC.S), "LCS.S"=eval(LCS.S)),
                 "Conclusion del proceso"= c(if(length(X.pos)< m){
                   print("Proceso fuera de Control en Grafica X")
                 } else {
                   print("El proceso esta bajo control en Grafica X")
                 }, if(length(R.pos)< m){
                   print("Proceso fuera de control en Grafica R")
                 } else {
                   print("El proceso esta bajo control en Grafica R")
                 }, if(length(S.pos)< m){
                   print("Proceso fuera de control en Grafica S")
                 } else {
                   print("El proceso esta bajo control en Grafica S")
                   })))
}
