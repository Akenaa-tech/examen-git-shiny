library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"),
  
  titlePanel("Exploration des Diamants"),

  sidebarLayout(
    
    sidebarPanel(
      radioButtons(
        inputId = "filtrage", 
        label="Colorier les points en rose ?", 
        choices = list("Oui"=1, "Non"=2), 
        selected = 1
      ),
      
      selectInput(
        inputId = "couleur",
        label = "Choisir une couleur à filtrer",
        choices = c("D","E", "F", "G", "H", "I", "J"),
        selected = "J"
      ),
      
      sliderInput(
        inputId = "prix",
        label = "Prix maximum",
        min = 300,
        max = 20000,
        value = 3000), 
      
      actionButton(
        inputId = "boutton", 
        label= "Visualiser le graphe"
      )
    ),
    
    mainPanel(
      textOutput(outputId = "DiamondsText"),
      plotOutput(outputId = "DiamondsPolts"),
      DT::DTOutput(outputId = "DiamondsTableau")
    )
  )
)



server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observeEvent(input$boutton, {
    
    message(showNotification(
      paste("Prix", inputId$prix, "& Color", inputId$couleur),
      type = "message"
    ))
    
    rv$str <- diamonds |> 
      filter(price > input$prix & color == input$couleur)
    
    rv$couleur_points <- if (input$filtrage ==1) "pink" else "black"
    
    
    
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