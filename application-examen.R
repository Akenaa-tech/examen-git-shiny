library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)
library(ggplot2)
library(plotly)


thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty" ),
  
  titlePanel("Exploration des Diamants"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(
        inputId = "filtrage", 
        label = "Colorier les points en rose ?",
        choices = list("Oui" = 1, "Non" = 2), 
        selected = 1
      ),
      
      selectInput(
        inputId = "couleur",
        label = "Choisir une couleur à filtrer :",
        choices = c("D","E","H","I","J"),
        selected = "J"
      ),
      
      sliderInput(
        inputId = "prix",
        label = "Prix maximum :",
        min = 300,
        max = 20000,
        value = 3000
      ),
      
      actionButton( 
        inputId = "boutton",
        label = "Visualiser le graphe"
      )
    ),
    
    mainPanel(
      textOutput(outputId = "DiamondsText"),
      plotlyOutput(outputId = "DiamondsPlots"),
      DT::DTOutput(outputId = "DiamondsTableau")
    )
  )
)


server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observeEvent(input$boutton, {
    
    showNotification(
      paste("Prix", input$prix, "& Color", input$couleur),
      type = "message"
    )
    
    rv$filtre <- diamonds %>% 
      filter(price < input$prix & color == input$couleur)  
    
    rv$couleur_points <- if (input$filtrage == 1) "pink" else "black"
    
    rv$graphique <- 
      ggplot(rv$filtre, aes(price, carat)) + 
      geom_point(color = rv$couleur_points) +
      labs(
        title = paste ("prix :", input$prix, "& color :", input$couleur)
      )
  })
  
  output$DiamondsPlots <- plotly::renderPlotly({
    req(rv$graphique)
    ggplotly(rv$graphique)
  })
  
  output$DiamondsTableau <- DT::renderDT({
    rv$filtre
  })
}

# Run the application
shinyApp(ui = ui, server = server)
