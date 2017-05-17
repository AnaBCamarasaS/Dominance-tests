
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage("Contastes para la dominancia de primer y segundo orden",

  tabPanel("Ejemplos Paramétricos",
           fluidPage(sidebarPanel(
      numericInput("alpha","Nivel de significación de los tests", value=0.05, min=0, max=1),
      numericInput("RMonteCarlo1","Número de réplicas en el proceso de Monte Carlo", value=15, min=1,max=50),
      selectInput("popDistx", "Distribución de X",
                  choices=c("Normal" = "normalx",
                       "Log-normal" = "lognormalx",
                       "Gamma" = "gammax",
                       "Weibull" = "weibullx",
                       "Pareto"="paretox"
                  )),  
      numericInput("n","Tamaño muestral de X",value=50, min=1, max=200),
      conditionalPanel(condition = 'input.popDistx == "normalx"', 
                       sliderInput("xmean", "Media de X", min=0, max=200, value=0) ,
                       sliderInput("xsd","Desviación Típica de X", min=0.1, max=20, value=1, step=.1)
      ),
      conditionalPanel(condition = 'input.popDistx == "lognormalx"', 
                       sliderInput("xmeanlog", "Media de X", min=0, max=200, value=0) ,
                       sliderInput("xsdlog","Desviación Típica de X", min=0.1, max=10, value=1, step=0.05)
      ),
      conditionalPanel(condition = 'input.popDistx == "gammax"', 
                       sliderInput("xshape", "Forma de X", min=0.1, max=10, value=1, step=0.05) ,
                       sliderInput("xscale","Escala de X", min=0.1, max=10, value=1, step=0.05)
      ),
      conditionalPanel(condition = 'input.popDistx == "weibullx"', 
                       sliderInput("xshape", "Forma de X", min=0.1, max=10, value=1, step=0.05) ,
                       sliderInput("xscale","Escala de X", min=0.1, max=10, value=1, step=0.05)
      ),
      conditionalPanel(condition = 'input.popDistx == "paretox"', 
                       sliderInput("xshape", "Forma de X", min=0.1, max=10, value=1, step=0.05) ,
                       sliderInput("xscale","Escala de X", min=0.1, max=10, value=1, step=0.05)
      ),
      selectInput("popDisty", "Distribución de Y",
                  choices=c("Normal" = "normaly",
                            "Log-normal" = "lognormaly",
                            "Gamma" = "gammay",
                            "Weibull" = "weibully",
                            "Pareto"="paretoy"
                  )), 
      numericInput("m", "Tamaño muestral de Y",value=50, min=1, max=200),
      conditionalPanel(condition = 'input.popDisty == "normaly"', 
                       sliderInput("ymean", "Media de Y", min=0, max=200, value=0) ,
                       sliderInput("ysd","Desviación Típica de Y", min=0.1, max=20, value=1, step=.1)
      ),
      conditionalPanel(condition = 'input.popDisty == "lognormaly"', 
                       sliderInput("ymeanlog", "Media de Y", min=0, max=200, value=0) ,
                       sliderInput("ysdlog","Desviación Típica de Y", min=0.1, max=20, value=1, step=.1)
      ),
      conditionalPanel(condition = 'input.popDisty == "gammay"', 
                       sliderInput("yshape", "Forma de Y", min=0.1, max=20, value=1, step=.1) ,
                       sliderInput("yscale","Escala de Y", min=0.1, max=20, value=1, step=.1)
      ),
      conditionalPanel(condition = 'input.popDisty == "weibully"', 
                       sliderInput("yshape", "Forma de Y", min=0.1, max=20, value=1, step=.1) ,
                       sliderInput("yscale","Escala de Y", min=0.1, max=20, value=1, step=.1)
      ),
      conditionalPanel(condition = 'input.popDisty == "paretoy"', 
                       sliderInput("yshape", "Forma de Y", min=0.1, max=20, value=1, step=.1) ,
                       sliderInput("yscale","Escala de Y", min=0.1, max=20, value=1, step=.1)
      ),
     
    actionButton("update", "Procesamos"),
   br(),
   br(),
   helpText("Información: El número de réplicas en el proceso de Monte Carlo es necesario para calcular el p-valor del SSD test. A mayor número de réplicas menor es el error, 1/(4R)^0.5, que estamos cometiendo al tomar la decisión de si hay o no dominancia. Sin embargo, a valores grandes de réplicas, más tiempo necesitamos para ver la solución."),
   img(src='logo.png', align = "center"),
   br(),
   br(),
    p("Aplicación realizado por", a("Ana Belén Camarasa", href="mailto: abcs3@alu.ua.es",
        target="_blank"),"para el TFG del Grado de Matemáticas", style = "font-family: 'times'")
    ),
         column(8,  plotOutput("graf"),
           tableOutput("test"),
           tableOutput("ssd")
           ))),
    
    tabPanel("Conjunto de datos", fluidPage(sidebarPanel(
      numericInput("RMonteCarlo2","Número de réplicas en el proceso de Monte Carlo", value=15, min=1,max=50),
      numericInput("alpha","Nivel de significación de los tests", value=0.05, min=0, max=1),
  
  fileInput('file1', 'Elige fichero xlsx para la variable X',
            accept = c(".xlsx")
  ),
  fileInput('file2', 'Elige fichero xlsx para la variable Y',
            accept = c(".xlsx")
  ),
  h4(actionButton("update2","Procesamos")),
  helpText("Información 1: Ambos ficheros deben tener formato xlsx y, además, en ambos ficheros deben aparecer solo los datos que deseamos estudiar siendo los decimales denotamos por comas."),
  helpText("Información 2: El número de réplicas en el proceso de Monte Carlo es necesario para calcular el p-valor del SSD test. A mayor número de réplicas menor es el error, 1/(4R)^0.5, que estamos cometiendo al tomar la decisión de si hay o no dominancia. Sin embargo, a valores grandes de réplicas, más tiempo necesitamos para ver la solución."),
  br(),
  img(src='logo.png', align = "center"),
  br(),
  br(),
  p("Aplicación realizado por", a("Ana Belén Camarasa", href="mailto: abcs3@alu.ua.es",
                                  target="_blank"),"para el TFG del Grado de Matemáticas", style = "font-family: 'times'")),
  
  
  column(8, plotOutput("grafd"),
           tableOutput("testd"),
           tableOutput("ssdd")
           
  )
  )
  )
)
)