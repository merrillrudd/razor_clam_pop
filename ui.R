
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
  
  conditionalPanel("$('li.active a').first().html()==='Population dynamics'",
                   column(2, img(src="RCpic1s.png", align="center"))
  ),
  conditionalPanel("$('li.active a').first().html()==='Explore dynamics'",
    column(2, 
           sliderInput("u", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
          sliderInput("yct", "Fishing closed:", value=rep(0,2), min=0, max=20, step=1))
    ),
  conditionalPanel("$('li.active a').first().html()==='Explore risks'",
      column(2, 
        sliderInput("u", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
         h4("Legend"),
         div(h5(strong("Status quo (SQ)"), style=paste0("color:", gray(0.5)))),
         div(h5(strong("Total impact from scenarios of change"), style=paste0("color:", gray(0.2)))),
          h4("Scenarios of change"),
          checkboxInput("inc_waves", div(strong("Increasing wave heights and storm surges"), style=paste0("color:", colors[1])), value=FALSE),
          conditionalPanel(
            condition = "input.inc_waves==true",
            div(h5("Implementation: decreased survival of pre-recruits over time"), style=paste0("color:", colors[1])),
            sliderInput("min_Spre", "Minimum pre-recruit survival", value=0.11, min=0, max=0.11, step=0.01)
          ),
          checkboxInput("HABs", div(strong("Harmful algal blooms"), style=paste0("color:", colors[2])), value=FALSE),
          conditionalPanel(
            condition = "input.HABs==true",
            div(h5("Implementation: fishing mortality drops to zero during HAB closure, harvest rate may be adjusted from status quo during open season"), style=paste0("color:", colors[2])),
            sliderInput("hab_yct", "Fishing closed:", value=rep(0,2), min=0, max=20, step=1)
          ),
          checkboxInput("pollution", div(strong("Pollution/oil spills"), style=paste0("color:", colors[3])), value=FALSE),
          conditionalPanel(
            condition = "input.pollution==true",
            div(h5("Implementation: increased mortality of pre-recruits and recruits"), style=paste0("color:", colors[3])),
            radioButtons("pollution_strength", div(h5(em("Intensity:")), style=paste0("color:", colors[3])), choice=list("High", "Low"), selected="Low")
          ),
          checkboxInput("dec_habitat", div(strong("Habitat destruction"), style=paste0("color:", colors[4])), value=FALSE),
          conditionalPanel(
            condition = "input.dec_habitat==true",
            div(h5("Implementation: increasing mortality of pre-recruits and recruits over time"), style=paste0("color:", colors[4])),
            sliderInput("capacity", "Beach capacity:", value=1, min=0,max=1,step=0.01)
           )
     )
  ),
  column(10,
    tabsetPanel(
      tabPanel("Population dynamics", column(6,img(src="razor_clam_pop_DD.png", align="center"))),
      tabPanel("Explore dynamics", plotOutput("CompareConstantHarvest")),
      tabPanel("Explore risks", plotOutput("ScenarioOutput"))
    )       
  )
))  