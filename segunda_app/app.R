# App para descargar datos de Yahoo Finance / FRED
# y visualizarlos

# Para crear secciones en un Rscript es CTRL/CMD + SHIFT + R


# pkgs --------------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(tidyquant)
library(plotly)
library(lubridate)
library(bslib)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  shinythemes::themeSelector(),
  # theme =  shinythemes::shinytheme("cerulean"),
  # theme = bs_theme(bootswatch = "quartz"),
  navbarPage(
      title = "Descarga y visualización de datos",

# * Configuración ---------------------------------------------------------

      tabPanel(
          title = "Configuración",
          textInput(
              inputId = "serie",
              label = "Escribe el/los nombre/s de la/s serie/activo a descargar. 
              En caso de ser más de una, separarlas por comas",
              value = "AAPL",
              placeholder = "Nombre serie, p. ej. AAPL"
          ),
          selectInput(
              inputId = "fuente",
              label = "Selecciona la fuente de los datos:",
              choices = c("Yahoo Finance" = "stock.prices",
                          "FRED" = "economic.data"),
              selected = "Yahoo Finance",
              multiple = FALSE
          ),
          conditionalPanel(
              "input.fuente=='stock.prices'",
              radioButtons(
                  inputId = "variable_yahoo",
                  label = "Selecciona la variable a graficar",
                  choices = c("open", "high", "low", "close", "volume", "adjusted"),
                  inline = TRUE,
                  selected = "close"
              )
          ),
          dateRangeInput(
              inputId = "fechas",
              label = "Elige el rango de fechas a utilizar:",
              start = "2019-01-01",
              end = today()-1,
              max = today(),
              startview = "decade"
          ),
          actionButton(
              inputId = "boton",
              label = "Aplicar los cambios",
              icon = icon("poo")
          )
      ),

# * Gráficas --------------------------------------------------------------

      tabPanel(
          title = "Gráficas",
          withSpinner(plotlyOutput(outputId = "grafica1"))
      ),

# * Tabla -----------------------------------------------------------------

      tabPanel(
          title = "Tabla",
          withSpinner(dataTableOutput(outputId = "tabla1"))
      )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
    # bslib::bs_themer()  
  
    datos <- eventReactive(input$boton,{
      tq_get(
          x = unlist(strsplit(gsub(" ","",input$serie), ",")),
          get = input$fuente,
          from = input$fechas[1],
          to = input$fechas[2]
      )
      
    })
    
    output$grafica1 <- renderPlotly({
        if (input$fuente == "stock.prices"){
            g1 <- datos() |> 
                ggplot(aes_string(y = input$variable_yahoo)) +
                aes(x = date, color = symbol) + 
                           geom_line() +
                           labs(
                               title = paste("Gráfica de los datos históricos de", 
                                             input$serie),
                               x = "",
                               y = paste(input$variable_yahoo, "price")
                           )
        } else{
            g1 <- datos() |> 
                ggplot(aes(x = date, y = price)) +
                           geom_line() +
                           labs(
                               title = paste("Gráfica de los datos históricos de", 
                                             input$serie),
                               x = "",
                               y = "Valor"
                           )
        }
        
        ggplotly(g1, dynamicTicks = TRUE) |> rangeslider()
    })
    
    output$tabla1 <- renderDataTable({
        datos()
    })
  
  
  
}

shinyApp(ui, server)