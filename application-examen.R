library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly" ),
  
  titlePanel("Exploration des Diamants"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(
        inputId = "colorier", 
        label = "Colorier les points en rose ?",
        choices = list("Oui" = 1, "Non" = 2), 
        selected = 1
      ),
      
      selectInput(
        inputId = "filtre",
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
      textOutput(outputId = "textstarwars"),
      plotOutput(outputId = "StarWarsPlot"),
      DT::DTOutput(outputId = "tableau")
    )
  )
)



server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observeEvent(input$boutton, {
    message(showNotification(
      "La valeur du slider a changé !",
      type = "message"
    ))
    
    rv$str <- starwars |> 
      filter(height > input$Taille & gender == input$gender)
    
    rv$plo <- rv$str |>
      ggplot(aes(x = height)) +
      geom_histogram(
        binwidth = 10, 
        fill = "pink", 
        color = "white") +
      labs(title=paste("Genre choisi :", input$gender))
    
  })
  
  
  output$StarWarsPlot <- renderPlot({
    rv$plo
  })
  
  #output$StarWarsPlot <- renderPlot({
  #if (is.null(graphs$plot)) {
  #  starwars
  #}
  #})
  
  output$textstarwars <- renderText ({
    nombre_lignes <- rv$str |>
      nrow() 
    
    paste("Nb lignes :", nombre_lignes)
  })
  
  output$tableau <- DT::renderDT({
    rv$str
  })
}




#Run the application
shinyApp(ui = ui, server = server)