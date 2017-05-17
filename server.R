
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(splines)
library(stats4)
library(VGAM)
library(readxl)

#Cargamos las funciones necesarias para llevar a cabo el test de dominancia de segundo orden
S_est<-function(X,Y)
{
  Fn<-ecdf(X)
  Gm<-ecdf(Y)
  XY<-sort(unique(c(X,Y)))
  int<-integrate(Gm,0,XY[1],rel.tol = 1.5e-7,subdivisions = 3000)$value-integrate(Fn,0,XY[1],rel.tol = 1.5e-7,subdivisions = 3000)$value
  for (i in 2:length(XY))
  {
    int2<-integrate(Gm,0,XY[i],rel.tol = 1.5e-7,subdivisions =3000)$value-integrate(Fn,0,XY[i],rel.tol = 1.5e-7,subdivisions = 3000)$value
    if (int2>int){int<-int2}
    
  }
  output<-int
  output
}


Sr_est<-function(X,Y)
{
  XY<-sort(unique(c(X,Y)))
  
  n<-length(X)
  m<-length(Y)
  
  #Generamos las muestras aleatorias
  Xr<-sample(X,n,replace=TRUE)
  Yr<-sample(Y,m,replace=TRUE)
  
  Fnr<-ecdf(Xr)
  Fn<-ecdf(X)
  Gmr<-ecdf(Yr)
  Gm<-ecdf(Y)
  
  intG<-integrate(Gmr,0,XY[1],rel.tol = 1.5e-7,subdivisions = 3000)$value-integrate(Gm,0,XY[1],rel.tol = 1.5e-7,subdivisions = 3000)$value
  intF<-integrate(Fnr,0,XY[1],rel.tol = 1.5e-7,subdivisions = 3000)$value-integrate(Fn,0,XY[1],rel.tol = 1.5e-7,subdivisions = 3000)$value
  int<-intG-intF
  for (i in 2:length(XY))
  {
    intG2<-integrate(Gmr,0,XY[i],rel.tol = 1.5e-7,subdivisions = 3000)$value-integrate(Gm,0,XY[i],rel.tol = 1.5e-7,subdivisions = 3000)$value
    intF2<-integrate(Fnr,0,XY[i],rel.tol = 1.5e-7,subdivisions = 3000)$value-integrate(Fn,0,XY[i],rel.tol = 1.5e-7,subdivisions = 3000)$value
    int2<-intG2-intF2
    if (int2>int){int<-int2}
    
  }
  output<-int
  output
}

SSD.test<-function(X,Y,R=15,nds=0.05)
{
  p<-0
  S<-S_est(X,Y)
  for (r in 1:R){
    if (Sr_est(X,Y)>S){p<-p+1/R
    if (p>nds){output<-c("X es dominada por Y en el sentido del segundo orden","El p-valor con el que para el programa es:",p)
    break}
    }
  }
  if(p<nds){output<-c("X no es dominada por Y en el sentido del segundo orden","El p-valor del test es:",p)}
  output
}

shinyServer(function(input, output) {
  ###EJEMPLOS PARAMÉTRICOS###
 #Creamos la muestra de la variable X
  xsample <- eventReactive(input$update, {
    
    if (isolate(input$popDistx == "normalx") ) {
      rsample <- isolate(rnorm(n = input$n,mean=as.numeric(input$xmean),sd=as.numeric(input$xsd)) )
    } else if (isolate(input$popDistx == "lognormalx") )  {
      rsample <- isolate(rlnorm(n = input$n,meanlog=as.numeric(input$xmeanlog),sdlog=as.numeric(input$xsdlog)) )
    } else if (isolate(input$popDistx == "gammax") )  {
      rsample <- isolate(rgamma(n = input$n,shape=as.numeric(input$xshape),scale=as.numeric(input$xscale)) )
    } else if (isolate(input$popDistx == "weibullx") )  {
      rsample <- isolate(rweibull(n = input$n,shape=as.numeric(input$xshape),scale=as.numeric(input$xscale)) )
    }else if (isolate(input$popDistx == "paretox") )  {
      rsample <- isolate(rpareto(n = input$n,shape=as.numeric(input$xshape),scale=as.numeric(input$xscale)) )
    }   })
   
 x <- reactive({
   matrix(xsample(),nrow=1 )
  })
  
  #Creamos la muestra de la variable Y
  ysample <- eventReactive(input$update, {
    
    if (isolate(input$popDisty == "normaly") ) {
      rsample <- isolate(rnorm(n = input$m,mean=as.numeric(input$ymean),sd=as.numeric(input$ysd)) )
    } else if (isolate(input$popDisty == "lognormaly") )  {
      rsample <- isolate(rlnorm(n = input$m,meanlog=as.numeric(input$ymeanlog),sdlog=as.numeric(input$ysdlog)) )
    } else if (isolate(input$popDisty == "gammay") )  {
      rsample <- isolate(rgamma(n = input$m,shape=as.numeric(input$yshape),scale=as.numeric(input$yscale)) )
    } else if (isolate(input$popDisty == "weibully") )  {
      rsample <- isolate(rweibull(n = input$m,shape=as.numeric(input$yshape),scale=as.numeric(input$yscale)) )
    }else if (isolate(input$popDisty == "paretoy") )  {
      rsample <- isolate(rpareto(n = input$m,shape=as.numeric(input$yshape),scale=as.numeric(input$yscale)) )
    }   })
  
  y <- reactive({
    matrix(ysample(),nrow=1 )
  })
  
 
  #Realizamos el gráfico de las funciones de distribución
  output$graf<-renderPlot({
    curve((if(input$popDistx == "normalx"){pnorm(x,mean = as.numeric(input$xmean),sd = as.numeric(input$xsd))}
           else if(input$popDistx == "lognormalx"){plnorm(x,meanlog=as.numeric(input$xmeanlog),sdlog=as.numeric(input$xsdlog))}
           else if (input$popDistx=="gammax"){pgamma(x,shape=as.numeric(input$xshape),scale=as.numeric(input$xscale))}
           else if (input$popDistx=="weibullx"){pweibull(x,shape=as.numeric(input$xshape),scale=as.numeric(input$xscale))}
           else if (input$popDistx=="paretox"){ppareto(x,shape=as.numeric(input$xshape),scale=as.numeric(input$xscale))}),
          from=min(x(),y()),to=max(x(),y()),ylim=c(0,1), xlab="x",ylab="F(x)", col="blue")
    curve((if(input$popDisty == "normaly"){pnorm(x,mean = as.numeric(input$ymean),sd = as.numeric(input$ysd))}
           else if(input$popDisty == "lognormaly"){plnorm(x,meanlog=as.numeric(input$ymeanlog),sdlog=as.numeric(input$ysdlog))}
           else if (input$popDisty=="gammay"){pgamma(x,shape=as.numeric(input$yshape),scale=as.numeric(input$yscale))}
           else if (input$popDisty=="weibully"){pweibull(x,shape=as.numeric(input$yshape),scale=as.numeric(input$yscale))}
           else if (input$popDisty=="paretoy"){ppareto(x,shape=as.numeric(input$yshape),scale=as.numeric(input$yscale))}), 
          col="red",add=TRUE)
    title("Funciones de distribución")
    legend("topright",c("FDA de X","FDA de Y"),col=c("blue","red"),lty=1:1)
  })
  
  #Realizamos el test de la dominancia de primer orden
  output$test<-renderTable({
    
   ks<-ks.test(x(),y(),alternative="less")
   if (ks$p.value>input$alpha){ksout1<-c("X es dominada por Y en el sentido del primer orden")}
   else {ksout1<-c("X no es dominada por Y en el sentido del primer orden")}
   ksout<-matrix(c(
     'P-valor igual a:', 'Conclusión:',
     ks$p.value, ksout1
   ), 2)
   colnames(ksout) = c('FSD test', 'Resultados')
   ksout  
  })
  
  #Realizamos el test de la dominancia de segundo orden
  output$ssd<-renderTable({
    #Realizamos las transformaciones necesarias en el caso de tener normales
    if (input$popDistx=="normalx"){x<-exp(x())}
    if (input$popDisty=="normaly"){y<-exp(y())}
    
    #Aplicamos el test con el alpha correspondiente
    s<-SSD.test(x(),y(),R=input$RMonteCarlo1,nds=input$alpha)
    ssdout<-matrix(c(
      'P-valor mayor o igual que:','Conclusión:',
      s[3],s[1]
    ),2)
    colnames(ssdout)=c('SSD test', 'Resultados')
    ssdout
   })

  ###CONJUNTO DE DATOS###
 #Creamos la muestra de la variable X
   filedatax<-eventReactive(input$update2,{
      inFile <- input$file1
      if(is.null(inFile)) return(NULL)
      file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
      read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    })
   xdatos<-reactive({as.matrix(filedatax())})
   
   #Creamos la muestra de la variable Y
   filedatay<-eventReactive(input$update2,{
     inFile <- input$file2
     if(is.null(inFile)) return(NULL)
     file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
     read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
   })
  ydatos<-reactive({as.matrix(filedatay())})

  #Realizamos el gráfico de las funciones de distribución
  output$grafd<-renderPlot({
    plot(ecdf(xdatos()),xlim=c(min(xdatos(),ydatos()),max(xdatos(),ydatos())),ylim=c(0,1), xlab="x",ylab="F(x)",main="Funciones de distribución", col="blue")
    plot(ecdf(ydatos()),col="red", from=0,add=TRUE)
    legend("topright",c("FDA de X","FDA de Y"),col=c("blue","red"),lty=1:1)
  })
  
  #Realizamos el test de la dominancia de primer orden
  output$testd<-renderTable({
    ks<-ks.test(xdatos(),ydatos(),alternative="less")
    if (ks$p.value>input$alpha){ksout1<-c("X es dominada por Y en el sentido del primer orden")}
    else {ksout1<-c("X no es dominada por Y en el sentido del primer orden")}
    ksout<-matrix(c(
      'P-valor igual a:', 'Conclusión:',
      ks$p.value, ksout1
    ), 2)
    colnames(ksout) = c('FSD test', 'Resultados')
    ksout
  })
  
  #Realizamos el test de la dominancia de segundo orden
  output$ssdd<-renderTable({
    s<-SSD.test(xdatos(),ydatos(),R=input$RMonteCarlo2,nds=input$alpha)
    ssdout<-matrix(c(
      'P-valor mayor o igual que:','Conclusión:',
      s[3],s[1]
    ),2)
    colnames(ssdout)=c('SSD test', 'Resultados')
    ssdout
  })
  
  
})
