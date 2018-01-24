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

theme_set(theme_minimal(base_size = 14))



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("Violencia Sexual en Adolescentes"),
  h3("Visualizador de indicadores"),
  
  br(),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
   sidebarPanel(width = 3,
  
  h4("Selecciona un indicador y un rango de edad."),
  br(),
  
        selectInput(inputId = "redad.filt", 
                    label = "Rango de Edad",
                    selected = c("nosolt"),
                    choices = c("[10 a 14]", 
                                "[12 a 14]",
                                "[15 a 19]")) ,
        selectInput(inputId = "indicador.filt", 
                    label = "Indicador",
                    selected = c("nosolt"),
                    choices = c("nosolt" = "nosolt",
                                "nosoltp" = "nosoltp",
                                "egreobs" = "egreobs",
                                "egreobsp" = "egreobsp",
                                "embalgv" = "embalgv",
                                "embalgvp" = "embalgvp",
                                "aborto" = "aborto",
                                "abortopemb" = "abortopemb",
                                "embact" = "embact",
                                "embactp" = "embactp",
                                "edadprsprom" = "edadprsprom",
                                "prs" = "prs",
                                "nacims" = "nacims",
                                "edad_padn_avg" = "edad_padn_avg",
                                "pob_conapo" = "pob_conapo",
                                "nacims_mil" = "nacims_mil",
                                "mmat" = "mmat",
                                "mmatpm" = "mmatpm",
                                "mmatp" = "mmatp",
                                "mabor" = "mabor",
                                "maborpm" = "maborpm",
                                "maborp" = "maborp",
                                "egreobspe" = "egreobspe",
                                "egreporab" = "egreporab",
                                "egreporabpe" = "egreporabpe",
                                "egreporabp" = "egreporabp",
                                "egreporviolsex" = "egreporviolsex",
                                "egreporviolsexpe" = "egreporviolsexpe",
                                "egreporviolsexp" = "egreporviolsexp",
                                "pconsemb" = "pconsemb",
                                "consemb" = "consemb",
                                "pconsembp" = "pconsembp",
                                "consembp" = "consembp",
                                "porconsembc" = "porconsembc")
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput("titletema.filt")),
        h4(textOutput("titleindicador.filt")),
        wellPanel(
          plotOutput("ggplot.filt")),
        wellPanel(
          dataTableOutput("datatab.filt"))
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
    str_to_title(unique(tab$Tema ))
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
      geom_point() +
      geom_line() + 
      ggtitle( paste("Rango de edad:", unique(tab$Adolescencia)) )
    
    print(gg)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

