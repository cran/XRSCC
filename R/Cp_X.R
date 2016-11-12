Cp_X<-function(prev.results,LES,LEI,mu){
  if (missing(prev.results)){
    stop("No elementos para evaluar")
  } else {
    if(missing(LES) | missing(LEI)){
      stop("Al menos uno de los limites de especificacion no esta definido")
    } else {
      data(factor.a, envir = environment())
      n<-ncol(prev.results$data.1)
      LCR<-as.numeric(prev.results$LR[2])
      if (missing(mu)){
        mu<-prev.results$LX[2]
        X.sigma<-expression(eval(LCR)/(factor.a$d2[n-1]*sqrt(n)))
      } else {
        X.sigma<-expression(eval(LCR)/(factor.a$d2[n-1]))
      }
      #Sequencia
      sd<-eval(X.sigma)
      .x<-seq(mu -4*sd,mu +4*sd, length = 1000)
      fx0<-expression((1/(sd*sqrt(2*pi)))*exp(-0.5*(((.x-mu)/sd)^2)))
      plot(eval(fx0)~.x,type="l", xlab="X", ylab="Densidad",
           main=paste("Distribucion Normal: Normalizacion Grafica X"),
           xlim=c(min(mu -4*sd, LEI), max(mu +4*sd,LES)))
      x0<-c(mu -3*eval(X.sigma), mu,  mu +3*eval(X.sigma))
      y0<-rep(0,3)
      # Definir la funcion de densidad
      fx0<-expression((1/(sd*sqrt(2*pi)))*exp(-0.5*(((x0-mu)/sd)^2)))
      segments(x0, y0, x1=x0, y1=eval(fx0), col=2,lwd=2)
      abline(a=0,b=0)
      # Se agregan las siguientes lineas que los limites de especificacion
      # bajo la misma funcion de densidad
      x0<-c(eval(LEI), eval(LES))
      xt.1<-eval(LES)
      y0<-rep(0,2)
      #Densidades
      fx1<-expression((1/(sd*sqrt(2*pi)))*exp(-0.5*(((x0-mu)/sd)^2)))
      fx2<-expression((1/(sd*sqrt(2*pi)))*exp(-0.5*(((xt.1-mu)/sd)^2)))
      fx3<-expression((1/(sd*sqrt(2*pi)))*exp(-0.5*(((0)/sd)^2)))
      segments(x0, y0, x1=x0,y1=eval(fx1), col=4,lwd=2)
      # Definir el texto de los limites para mostrar
      text(c(mu -3*sd, mu,  mu +3*sd,
             eval(LEI), eval(LES),mu -3*sd, mu +3*sd, mu), c(rep(eval(fx3)*0.3,3),
                                                         rep(eval(fx3)*0.15,2),
                                                         rep(eval(fx3)*0.25,3)),
           c(expression(-3*hat(sigma)),expression(mu), expression(+3*hat(sigma)),
             paste(c("LEI =","LES ="), c(round(eval(LEI),1), round(eval(LES),1))),
             paste(c(round(mu -3*sd,3), round(mu +3*sd,3), round(mu,3)))), cex = 1, col="blue")
      # Define la ecuacion simbolica de la funcion de densidad
      text(mu -3*sd,eval(fx3)*0.9,
           expression(f(x) == paste(frac(1, sigma * sqrt(2 * pi)), " ", e^{frac(-(x - mu)^2, 2 * sigma^2)})),
           cex = 1.25, col="black")
      # Muestra la funcion simbolica de la capacidad del proceso
      text(mu + 3*sd,eval(fx3)*0.9,
           expression(Cp == paste(frac("LES - LEI", 6 * hat(sigma)))),cex = 1.25, col="black")
      # Define el indicador de la Capacidad del proceso Cp
      Cp <- (LES - LEI)/(6 * sd)
      Cpk<- min(LES - mu, mu - LEI)/(3 * sd)
      # Muestra El resultado de la capacidad del proceso
      text(mu + 3*sd,eval(fx3)*0.7, paste("Cp =", round(eval(Cp),2)))
      # Define el indicador del porcentaje del uso de los limites de especificacion
      P.cp <- (1/eval(Cp))*100
      # Muestra El resultado del porcentaje del uso de los limites de especificacion
      text(mu + 3*sd,eval(fx3)*0.6, paste("P =", round(eval(P.cp),2), "%"))
      #
      # Crear la lista de los resultados Cp y P
      structure(list("Cp"=eval(Cp), "Cpk" = eval(Cpk), "P"=eval(P.cp), "X.sigma"= eval(X.sigma)),
                "Concluision del Proceso" = if(eval(Cp)>1){
                  print("Los limites naturales estan dentro de los limites de especificacion")
                } else {
                  if(eval(Cp)<1){
                    print("Los limites naturales sobrepasan  los limites de especificacion")
                  } else {
                    print("Los limites naturales son iguales a los limites de especificacion")
                    }
                })
    }
  }
}
