#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)
library(tidyverse)
library(forcats)
library(stringr)
library(lubridate)

load("df_resumen.RData")

theme_set(theme_bw(base_size = 17))



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("Violencia Sexual en Adolescentes"),
  h3("Visualizador de indicadores"),
  
  br(),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
   sidebarPanel(width = 4,
  
  h4("Selecciona un indicador y un rango de edad."),
  br(),
  
        selectInput(inputId = "redad.filt", 
                    label = "Rango de Edad",
                    selected = c("[15 a 19]"),
                    choices = c("[10 a 14]", 
                                "[12 a 14]",
                                "[15 a 19]")) ,
        selectInput(inputId = "indicador.filt", 
                    label = "Indicador",
                    selected = c("nosolt"),
                    choices = c("Num. mujeres casadas o unidas" = "nosolt",
                                "Porc. mujeres casada o unidas" = "nosoltp",
                                "Num. embarazadas alguna vez" = "embalgv",
                                "Porc. embarazadas alguna vez" = "embalgvp",
                                "Num. pérdida o aborto alguna vez" = "aborto",
                                "Aborto alg vez por embarazadas alg vez" = "abortopemb",
                                "Num. embarazadas actualmente" = "embact",
                                "Porc. embarazadas actualmente" = "embactp",
                                "Edad promedio primera relación sexual" = "edadprsprom",
                                "Porc. sexualmente activas por total mujeres" = "prs",
                                "Num. nacimientos" = "nacims",
                                "Edad promedio del padre" = "edad_padn_avg",
                                "Población (CONAPO)" = "pob_conapo",
                                "Tasa nacimientos 1000 habs." = "nacims_mil",
                                "Num. muerte materna" = "mmat",
                                "Porc. muerte materna por total defunciones" = "mmatpm",
                                "Tasa de muerte materna 100000 mujeres" = "mmatp",
                                "Num. muerte por aborto" = "mabor",
                                "Porc. muerte por aborto por total defunciones" = "maborpm",
                                "Tasa de muerte por aborto 100000 mujeres" = "maborp",
                                "Num. egresos hosp. causas obstetricas" = "egreobs",
                                "Porc. egresos hosp. causas obstetricas" = "egreobsp",
                                "Porc. egresos hosp. causas obstetricas por total egresos" = "egreobspe",
                                "Num. egresos hosp por aborto " = "egreporab",
                                "Porc. egresos por aborto del total de egresos" = "egreporabpe",
                                "Porc. egresos por aborto del total de mujeres" = "egreporabp",
                                "Num. egresos hosp. por abuso sexual" = "egreporviolsex",
                                "Porc. egresos por abuso sexual por total egresos" = "egreporviolsexpe",
                                "Porc. egresos por abuso sexual por total mujeres" = "egreporviolsexp",
                                "Total primera consulta por embarazo anuales" = "pconsemb",
                                "Total consulta por embarazo por embarazo anuales" = "consemb",
                                "Promedio mensual primera consulta por embarazo" = "pconsembp",
                                "Promedio mensual consulta por embarazo" = "consembp",
                                "Porc. consulta por embarazo por total consultas anuales" = "porconsembc")
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput("titleindicador.filt")),
        h4(textOutput("titletema.filt")),
        "Información disponible de indicadores por fuente de 2010 a 2016.",
        br(),
        br(),
        
        tabsetPanel(
          tabPanel("Gráfica", 
                   br(),
                   "Visualización de indicador seleccionado",
                   br(),
                   plotOutput("ggplot.filt")),
          tabPanel("Tabla", 
                   br(),
                   "Tabla puntual de indicador seleccionado",
                   br(),
                   dataTableOutput("datatab.filt"))
        )
      ) # main panel

  
   ), # sidebarLayout
  
  
  hr(),
  HTML('<p style="text-align:center">
       <b> Creado por </b>
       <br>
       <img src="logo-CAD.png", width="90", height="35">
       </p>')
  
) # fluidpage

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  tabFilt <- reactive({
    df_resumen %>% 
      mutate(Valor = round(Valor, 4)) %>% 
      spread(Year, Valor) %>% 
      filter(Indicador == input$indicador.filt, 
             Adolescencia == input$redad.filt) 
  })
  
  output$titleindicador.filt <- renderText({ 
    tab <- tabFilt()
    paste(unique(tab$Descripcion ))
  })
  
  output$titletema.filt <- renderText({ 
    tab <- tabFilt()
    paste("Tema:", str_to_title(unique(tab$Tema )))
  })
  
  output$datatab.filt <- renderDataTable({
    validate(
      need(nrow(tabFilt()) > 0, "Selecciona otro indicador o grupo de edad")
    )
    
    tabFilt() %>% 
      dplyr::select(-Tema, -Indicador, -Descripcion)
  }, options = list(dom = 't', searching = FALSE))
  
  output$ggplot.filt <- renderPlot({
    
    validate(
      need(nrow(tabFilt()) > 0, "Selecciona otro indicador o grupo de edad")
    )
    tab <- tabFilt()
    
    gg <- tab %>% 
      gather(year, value, -c(Fuente:Adolescencia)) %>% 
      ggplot(aes(x = year, y = value, 
                 color = Fuente,
                 group = Fuente)) + 
      geom_point(size = 4) +
      geom_line() + 
      ylab("indicador") + 
      xlab("años") + 
      scale_y_continuous(labels = function(x)format(x, big.mark=",")) + 
      ggtitle("" ,
               paste("Rango de edad:", unique(tab$Adolescencia)) )
    
    print(gg)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

