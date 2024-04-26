
# pkgs --------------------------------------------------------------------
# CTRL/CMD + SHIFT + R

library(tidyverse)
library(fpp3)
library(shiny)
library(tidyquant)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(
      "Descarga de datos de Yahoo Finance"
  ),
  sidebarLayout(
      sidebarPanel(
          selectInput(
              inputId = "ticker",
              label = "Escoge la acción a mostrar",
              choices = c(Cemex = "CEMEXCPO.MX", Disney = "DIS", 
                          Netflix = "NFLX", Tesla = "TSLA"),
              selected = "DIS"
          ),
          dateInput(
              inputId = "fecha",
              label = "Selecciona la fecha de inicio:",
              value = today() - 31,
              max = today() - 1,
              format = "dd-mm-yyyy",
              startview = "year",
              language = "es"
          )
      ),
      mainPanel(
          plotOutput(
            outputId = "grafica"
          ),
          p("Esto es un párrafo de texto.")
      )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  output$grafica <- renderPlot({
    tq_get(
      x = input$ticker,
      get = "stock.prices",
      from = input$fecha,
      to = today() - 1
    ) |> 
      ggplot(aes(x = date, open = open, high = high, 
                 low = low, close = close)) +
      geom_candlestick(aes())
  })
}

shinyApp(ui, server)