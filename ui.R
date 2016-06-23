
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Razor clam population dynamics under scenarios of change"),
  
  column(2,
      h4("Baseline natural mortality"),
      numericInput("M_eggs", "Eggs:", value=1),
      numericInput("M_prerecruits", "Pre-recruits:", value=0.75),
      numericInput("M_recruits", "Recruits:", value=0.5),
      h4("Other population processes"),
      numericInput("plus_yrs", "Number of years as recruit:", value=3),
      numericInput("prop_spawners", "Proportion of recruits spawning:", value=1),
      numericInput("fecundity", "Number of eggs per adult (Hundred thousands):", value=3)
    ),
  column(10, 
      sidebarLayout(
        sidebarPanel(
          h4("Scenarios of change"),
          checkboxInput("inc_waves", "Increasing wave heights and storm surges in the Pacific Northwest", FALSE),
          checkboxInput("HABs", "Harmful algal blooms", FALSE),
          checkboxInput("pollution", "Pollution/oil spills", FALSE),
          checkboxInput("NIX", "NIX", FALSE),
          checkboxInput("hypoxia", "Hypoxia", FALSE),
          checkboxInput("dec_habitat", "Habitat destruction", FALSE)
          ),
        mainPanel(
          plotOutput("CatchOverTime")
          )
        )
         )
    )
  
  

)
