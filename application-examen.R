library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

# Recopie star wars et changement de thème


thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "quartz"),
  
  
  titlePanel("Star Wars"),
  h1("Star Wars Characters"),
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "boutton",
                   label = "Clique moi"
      ),
      sliderInput(inputId = "Taille",
                  label = "Taille des personnages",
                  min = 0,
                  max = 250,
                  value = 30),
      
      selectInput(inputId = "gender",
                  label = "Genre",
                  choices = c("feminine","masculine"),
                  selected = "feminine"
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