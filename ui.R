
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Razor clam population dynamics"),
  column(2,
         h4("Natural Survival Rates"),
         sliderInput("survE", "Eggs:", value=0.003, min=0, max=1, step=0.001),
         sliderInput("survL", "Larva:", value=0.03, min=0, max=1, step=0.01), 
         sliderInput("survS", "Set:", value=0.05, min=0, max=1, step=0.01),
         sliderInput("survJ", "Juveniles:", value = 0.2, min=0, max=1, step=0.01),
         sliderInput("survA", "Adults:", value = 0.4, min=0, max=1, step=0.01),
         h4("Harvest Rates"),
         sliderInput("hrJ", "Juveniles:", value=0.7, min=0, max=1, step=0.01),
         sliderInput("hrA", "Adults:", value=0.8, min=0, max=1, step=0.01)
  ),
  column(2,
         h4("Transition Probabilities"),
         p(em("Each stage must sum to 1")),
         p(strong("Larva")),
         fluidRow(column(4, numericInput("LL", h5("Stay"), value=0)),
                  column(4, offset=1, numericInput("LS", h5("Transition"), value=1))),
         p(strong("Set")),
         fluidRow(column(4, numericInput("SS", h5("Stay"), value=0)),
                  column(4, offset=1, numericInput("SJ", h5("Transition"), value=1))),
         p(strong("Juvenile")),
         fluidRow(column(4, numericInput("JJ", h5("Stay"), value=0.3)),
                  column(4, offset=1, numericInput("JA", h5("Transition"), value=0.7))),
         p(strong("Adults")),
         numericInput("AL", h5("Proportion of adults reproducing annually"), value=1),
         h4("Other Factors"),
         sliderInput("fec", "Fecundity:", value=4e6, min=0, max=1e7, step=1e3),
         sliderInput("capacity", "Beach capacity:", value=1, min=0, max=4, step=0.01),
         sliderInput("quality", "Beach quality:", value=1, min=0, max=1, step=0.01)
  ),
  column(8, tabsetPanel(
    tabPanel("Get model to equilibrium", plotOutput("GrowthRate")),
    tabPanel("Stochastic simulation", 
        sidebarLayout(
          sidebarPanel(
            sliderInput("sd", "Variability:", value=0.6, min=0, max=1, step=0.05),
            numericInput("nsim", "Number of simulations",value=100, min=1, max=1000, step=1)
          ),
          mainPanel(plotOutput("Stochastic"))
  ))))


))
