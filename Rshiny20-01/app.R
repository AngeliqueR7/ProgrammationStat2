library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

# Données
Etats <- row.names(USArrests)
DataCrimes <- USArrests %>% 
  mutate(Etats = as.factor(Etats))

# UI
ui <- fixedPage(theme = shinytheme("flatly"),
                h2("Rivière Angélique - 21/01/2025"),
                navbarPage("ShinyApp - Arrestations pour crimes violents en 1973",
                           tabPanel("Informations",            
                                    fluidPage(
                                      h3("Description"),
                                      p("Cette application permet de visualiser les données d'arrestations pour crimes violents aux États-Unis par État en 1973, selon le crime parmis les trois suivants: Meurtre, Agression et Viol."),
                                    )
                           ),
                           tabPanel("Visualisations",            
                                    fluidPage(
                                      # Sélection du crime
                                      p("Sélectionnez un crime pour voir les statistiques correspondantes."),
                                      radioButtons("crime", "Choix du crime", choices = c("Murder", "Assault", "Rape"), selected = "Murder"),
                                      plotOutput("histo")
                                    )
                           )
                )
)

server <- function(input, output, session) {
  
  # Sélection des données réactives
  slcdata <- reactive({
    DataCrimes %>%
      select(Etats, crime = .data[[input$crime]])
  })
  
  # Affichage de l'histogramme
  output$histo <- renderPlot({
    ggplot(slcdata(), aes(x = Etats, y = crime)) +
      geom_bar(stat = "identity", fill = "deepskyblue2", color = "deepskyblue4") +
      xlab("État") +
      ylab(paste("Crimes pour :", input$crime)) +
      ggtitle(paste("Nombre d'arrestations pour", input$crime)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
