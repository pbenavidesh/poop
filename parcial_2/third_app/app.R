
# pkgs --------------------------------------------------------------------

library(shiny)
library(shinycssloaders)
library(bslib)
library(tidyverse)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"),
  titlePanel("Descarga y modelado de datos"),
  sidebarLayout(

# * sidebar ---------------------------------------------------------------

      sidebarPanel(
          h5("Configuración de la app"),
          tabsetPanel(
              tabPanel(
                  title = "Datos",
# ** datos ----------------------------------------------------------------
                  
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

# ** modelos --------------------------------------------------------------
              
              tabPanel(
                  title = "Modelos",
                  checkboxGroupInput(
                      inputId = "modelos",
                      label = "Selecciona los modelos a estimar",
                      choices = c("ARIMA", "ETS"),
                      selected = "ETS",
                      inline = TRUE
                  ),
                  # selección de métrica de error
                  # seleccionar modelos a combinar
                  # 
              )
              )

          ),

# * mainPanel -------------------------------------------------------------

      mainPanel(
          navbarPage(
              title = "",

# ** gráfica --------------------------------------------------------------
              
              tabPanel(
                  title = "Gráfica",
                  withSpinner(plotlyOutput(outputId = "grafica1"))
              ),

# ** tabla ----------------------------------------------------------------

              tabPanel(
                  title = "Tabla",
                  downloadButton("csv", label = "Exportar a .csv",
                                 icon = icon("download")),
                  withSpinner(dataTableOutput(outputId = "tabla1"))
                  
              )
          )
      )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
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
    
    output$csv <- downloadHandler(
        filename = "datos.csv",
        content = function(fname){
            write_csv(datos(), fname)
        }
    )
}

shinyApp(ui, server)