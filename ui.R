
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(RColorBrewer)
colors <- brewer.pal(4, "Set1")

shinyUI(fluidPage(
  titlePanel("Razor clam population dynamics under scenarios of change"),
  
  column(1,
      h4("Baseline natural mortality"),
      numericInput("M_eggs", "Eggs:", value=1),
      numericInput("M_prerecruits", "Pre-recruits:", value=0.79),
      numericInput("M_recruits", "Recruits:", value=0.3),
      h4("Other population processes"),
      numericInput("plus_yrs", "Number of years as recruit:", value=3),
      numericInput("prop_spawners", "Proportion of recruits spawning:", value=1),
      h4("Management"),
      numericInput("u_equil", "Baseline harvest rate:", value=0.3),
      numericInput("nyears", "Number of years to project:", value=20)
    ),
  column(2, 
          h4("Scenarios of change"),
          checkboxInput("inc_waves", div(strong("Increasing wave heights and storm surges"), style=paste0("color:", colors[1]))),
          conditionalPanel(
            condition = "input.inc_waves==true",
            radioButtons("inc_waves_strength", div(h5(em("Strength of increase:")), style=paste0("color:", colors[1])), choice=list("High", "Low"), selected="Low")
          ),
          checkboxInput("HABs", div(strong("Harmful algal blooms"), style=paste0("color:", colors[2]))),
          conditionalPanel(
            condition = "input.HABs==true",
            radioButtons("HABs_freq", div(h5(em("Closure frequency:")), style=paste0("color:", colors[2])), choices=list("High","Low"), selected="Low"),
            radioButtons("HABs_strength", div(h5(em("Open season harvest rate:")), style=paste0("color:", colors[2])), choice=list("Higher", "Business As Usual", "Lower"), selected="Business As Usual")
          ),
          checkboxInput("pollution", div(strong("Pollution/oil spills"), style=paste0("color:", colors[3]))),
          conditionalPanel(
            condition = "input.pollution==true",
            radioButtons("pollution_strength", div(h5(em("Intensity:")), style=paste0("color:", colors[3])), choice=list("High", "Low"), selected="Low")
          ),
          checkboxInput("dec_habitat", div(strong("Habitat destruction"), style=paste0("color:", colors[4]))),
          conditionalPanel(
            condition = "input.dec_habitat==true",
            radioButtons("dec_habitat_strength", div(h5(em("Rate of decrease:")), style=paste0("color:", colors[4])), choice=list("High", "Low"), selected="Low")
          )
     ),
     column(9,
          column(6, plotOutput("NoCatchByMeanCatch"),plotOutput("CatchOverTime")),
          column(6, plotOutput("TotalCatch"), plotOutput("RecruitsOverTime"))
          )
))  