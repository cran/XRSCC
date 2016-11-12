Beta.X<-function(k,n){
  beta.x1<-function(k,n,L=3){
    phi1 <- pnorm(c(L-k*sqrt(n)), mean=0, sd=1, lower.tail=TRUE)
    phi2 <- pnorm(c(-L-k*sqrt(n)), mean=0, sd=1, lower.tail=TRUE)
    beta.1<-phi1-phi2
    return(beta.1)
  }
  beta.2<-beta.x1(k,n)
  k.1<-seq(0,k*1.5,length.out=100)
  beta.x2<-beta.x1(k.1,n)
  ARL.x2<-1/(1-beta.x2)
  #Marco de graficos
  mat<-matrix(1:2,1,2,byrow=TRUE)
  layout(mat)
  layout.show(length(1:2))
  #CO
  plot(k.1, beta.x2, type = "l", lty = 1, lwd = 2,
       lend = par("lend"),
       col = "green", cex = 2, bg = NA,
       xlab = expression(kappa), ylab = expression(beta),
       xlim = c(0, k*1.5),
       ylim = c(0,1),
       pch = 19,
       main ="Curva Caracteristica de Operacion
       Corrimiento de la Media")
  # Agrega opciones de graficas de bajo nivel
  grid(10, 10, lwd = 0)
  segments(x0=0, y0=0, x1=k*1.5, y1=0, col="black",lwd=1)
  segments(x0=0, y0=0, x1=0, y1=1, col="black",lwd=1)
  segments(x0=k, y0=0, x1=k, y1=eval(beta.2), col=2,lwd=2)
  segments(x0=0, y0=eval(beta.2), x1=k, y1=eval(beta.2), col=2,lwd=2)
  text(k*1.1,eval(beta.2),expression(beta), cex = 1)
  text(k*1.15,eval(beta.2),paste(" = "), cex = 1)
  text(k*1.25,eval(beta.2),paste(round(beta.2,3)), cex = 1)
  #ARL
  plot(k.1, ARL.x2, type = "l", lty = 1, lwd = 2,
       lend = par("lend"),
       pch = NULL,
       col = "red", cex = 2, bg = NA,
       xlab = expression(kappa), ylab = paste("ARL"),
       xlim = c(0, k*1.5), ylim = c(0,eval(1/(1-beta.2))*1.2),
       main ="Ancho medio de la corrida
       para detectar corrimiento de la Media")
  # Agrega opciones de graficas de bajo nivel
  grid(10, 10, lwd = 0)
  segments(x0=0, y0=0, x1=k*1.5, y1=0, col="black",lwd=1)
  segments(x0=0, y0=0, x1=0, y1=eval(1/(1-beta.2))*1.2, col="black",lwd=1)
  segments(x0=k, y0=0, x1=k, y1=eval(1/(1-beta.2)), col="blue",lwd=2)
  segments(x0=0, y0=eval(1/(1-beta.2)), x1=k, y1=eval(1/(1-beta.2)), col="blue",lwd=2)
  text(k*1.05,eval(1/(1-beta.2)),paste("ARL"), cex = 1)
  text(k*1.10,eval(1/(1-beta.2)),paste(" = "), cex = 1)
  text(k*1.25,eval(1/(1-beta.2)),paste(round(eval(1/(1-beta.2)),3)), cex = 1)
  structure(list("beta"= beta.2,
                 "ARL"=1/(1-beta.2)))
}
