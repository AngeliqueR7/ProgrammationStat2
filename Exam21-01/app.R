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
                                      tags$figure(
                                        tags$img(
                                          src = "crime.jpg",
                                          width = 700,
                                          style="display: block; margin-left: auto; margin-right: auto;")
                                        ),
                                      h3("Description"),
                                      p("Cette application permet de visualiser les statistiques d'arrestations pour les crimes violents aux États-Unis par État en 1973, selon le crime choisis parmis les trois suivants: Murder (Meurtre), Assault (Agression) et Rape (Viol)."),
                                      p("Ces données proviennent du dataset USArrests: Violent Crime Rates by US State, disponible sur Rstudio."),
                                    )
                           ),
                           tabPanel("Arrestations",            
                                    fluidPage(
                                      # Sélection du crime
                                      p("Sélectionnez un crime pour voir les statistiques correspondantes."),
                                      radioButtons("crime", "Choix du crime", choices = c("Murder", "Assault", "Rape"), selected = "Murder"),
                                      plotOutput("histocrime")
                                    )
                           ),
                           tabPanel("Population Urbaine",            
                                    fluidPage(
                                      p("Graphique représentant le pourcentage de population urbaine pour chaque états."),
                                      plotOutput("graphpop")
                                    )
                           )
                )
)

server <- function(input, output, session) {
  
  # Sélection des données réactives
  slcdata <- reactive({
    DataCrimes %>%
      select(Etats, crime = .data[[input$crime]], UrbanPop)
  })
  
  # Affichage de l'histogramme pour le crime sélectionné
  output$histocrime <- renderPlot({
    ggplot(slcdata(), aes(x = Etats, y = crime)) +
      geom_bar(stat = "identity", fill = "deepskyblue2", color = "deepskyblue4") +
      xlab("État") +
      ylab(paste("Sélection :", input$crime)) +
      ggtitle(paste("Nombre d'arrestations pour", input$crime)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$graphpop <- renderPlot({
    ggplot(slcdata(), aes(x = Etats)) +
      geom_line(aes(y = UrbanPop), group = 1, color = "darkorange", size = 1) +
      geom_point(aes(y = UrbanPop), color = "darkorange", size = 2) +
      xlab("État") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
}

# Lancer l'application Shiny
shinyApp(ui, server)
