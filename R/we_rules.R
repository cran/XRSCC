we_rules<-function(prev.results){
  if (missing(prev.results)){
    stop("No elementos para evaluar")
  } else {
    data(factor.a, envir = environment())
    n.1<-ncol(prev.results$data.1)
    LCR<-as.numeric(prev.results$LR[2])
    X.sigma<-expression(eval(LCR)/((factor.a$d2[n.1-1])*sqrt(n.1)))
    x.1<-prev.results$data.1
    m.1<-nrow(x.1)
    X.prom.1<-apply(x.1,1,mean)
    f.rango.1<-function(x.1){
      f.rango.p.1<-range(x.1)
      return(f.rango.p.1[2]-f.rango.p.1[1])
    }
    X.range.1<-apply(x.1,1,f.rango.1)
    LCI.X<-prev.results$LX[1]
    LC.X<-prev.results$LX[2]
    LCS.X<-prev.results$LX[3]
    #
    # Script para Grafica X con zonas
    plot.Xzones<-function(x=X.prom.1,type="b",col="blue",pch =19){
      plot(x=x, xlab= "Numero de muestra",
           ylab="Valores de cada muestra",
           main="Grafica X, Control Estadistico de la Calidad",
           sub="Zonas para el analisis de las reglas Western Electric",
           type=type, col=col,
           ylim=c(min(eval(LCI.X)-mean(X.range.1)*0.05, LC.X -3*eval(X.sigma)*1.05),
                  max(eval(LCS.X)+mean(X.range.1)*0.05, LC.X +3*eval(X.sigma)*1.05)),
           xlim=c(-0.05*m.1, 1.05*m.1), pch = pch)
      # Agregar opciones de graficos de bajo nivel
      abline(h= c(eval(LCS.X), eval(LCI.X), eval(LC.X)),col="lightgray")
      text(c(rep(0,3),rep(7,3)), rep(c(eval(LCS.X),eval(LC.X),eval(LCI.X)),2),
           c(c("LCS = ","LC = ","LCI = "), c(round(eval(LCS.X),3),
                                             round(eval(LC.X),3), round(eval(LCI.X),3))),
           col="red",cex=1)
      # Agregar zonas de + / - 3 sigmas
      abline(h= c(c(-3:-1,1:3)*eval(X.sigma)+eval(LC.X)), col="green")
      text(rep(m.1,6), c(c(-3:-1,1:3)*eval(X.sigma)+eval(LC.X)),
           c(expression(-3*hat(sigma)),expression(-2*hat(sigma)),
             expression(-1*hat(sigma)),expression(+1*hat(sigma)),
             expression(+2*hat(sigma)), expression(+3*hat(sigma))),
           col="black",cex=1)
      # Agregar nombre a las zonas
      text(rep(m.1,6),c(c(-2.5,-1.5,-0.5,0.5,1.5,2.5)*eval(X.sigma)+eval(LC.X)),
           c("A","B","C","C","B","A"), cex=2,col="gray")
    }
    # Funcion para encontrar zonas
    limites.zona <- seq(-3, 3)
    # Crea el conjunto de zonas para cada punto de la carta X
    zonas <- sapply(limites.zona,
                    function(i) {
                      i * rep(eval(X.sigma), m.1)
                    })
    zonas<-zonas + eval(LC.X)
    colnames(zonas) <- paste("zona", -3:3, sep="_")
    # Evaluar las zonas
    x.zonas <- rowSums(X.prom.1 > zonas)
    # Pasar por cada valor de 1 hasta m.1
    for(i in 1:m.1){
      # Cualquier valor mas alla +/- 3 sigmas
      v1 <- x.zonas[i]
      regla1 <- ifelse(any(v1 == 7), 1,
                       ifelse(any(v1 == 0),1, 0))
      #
      # Dos de cada tres puntos en la zona A
      v2 <- x.zonas[max(i-3, 1):i]
      regla2 <- ifelse(sum(v2 == 6) >= 2, 1,
                       ifelse(sum(v2 == 1) >= 2, 1, 0))
      # Cuatro de cada cinco puntos mas alla de la zona C
      v3 <- x.zonas[max(i-5, 1):i]
      regla3 <- ifelse(sum(v3 >= 5) >= 4, 1,
                       ifelse(sum(v3 <= 2) >= 4, 1, 0))
      # Ocho puntos consecutivos de un lado de la
      # linea de control central
      v4 <- x.zonas[max(i-8, 1):i]
      regla4 <- ifelse(all(v4 >= 4), 1,
                       ifelse(all(v4 <= 3), 1, 0))
    }
    bin.w<- if(sum(c(regla1,regla2,regla3,regla4))> 0){
      bin.w<-1
    } else {
      bin.w<-0
    }
    # Ejecutar grafico
    dev.off()
    plot.Xzones()
    # vector de resultados
    structure(list("Resultados del analisis" = if(bin.w>0){
      print("El proceso esta fuera de control por las Reglas Western Electric")
      print("Las siguientes reglas tienen al menos un grupo que viola la regla")
      c("Regla 1"= regla1,
        "Regla 2"= regla2,
        "Regla 3"= regla3,
        "Regla 4"= regla4)
    } else {
      print("El proceso esta bajo control por las reglas Western Electric")
    }))
  }
}
