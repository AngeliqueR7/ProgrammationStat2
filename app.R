library(shiny)
library(shinythemes)
library(ggplot2)

ui <- fixedPage(theme = shinytheme("united"),
          h2("Rivière Angélique - 14/01/2025"),
          navbarPage("Ma première application Rshiny"),
          navlistPanel(
            "Applications",
            tabPanel(align="center","Salutations",
                     textInput("nom", "Quel est ton nom ?"),
                     textOutput("salutation")),
            
            tabPanel(align="center","Multiplication",
                     sliderInput("curseur1", label = "Si X est ", min = 1, max = 100, value = 30 ),
                     sliderInput("curseur2", label = "Si Y est ", min = 1, max = 100, value = 30 ), 
                     "alors X multiplié par Y est : ",
                     textOutput("le_texte")),
            
            tabPanel(align="center","Visualisations",            
                     selectInput("dataset", "Choix du dataset :", choices = c("economics", "faithfuld", "seals")),
                     verbatimTextOutput("summary"),
                     plotOutput("le_graphe"))
          )
)

server <- function(input, output, session) {
    output$salutation <- renderText({
      paste0("Bonjour ", input$nom)
    })
    output$le_texte <- renderText({ 
      paste0(input$curseur1 * input$curseur2)})
    
    dataset <- reactive({
      get(input$dataset, "package:ggplot2")
    })
    
    output$summary <- renderPrint({
      summary(dataset())
    })
    
    output$le_graphe <- renderPlot({
      plot(dataset())
    }, res = 96)
    
}

shinyApp(ui, server)