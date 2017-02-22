
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(RColorBrewer)
colors <- brewer.pal(4, "Set1")[c(2,3,1,4)]

shinyUI(fluidPage(
  titlePanel("Razor clam population dynamics under scenarios of change"),
  tabsetPanel(
    tabPanel("Population dynamics", 
             column(2, img(src="RCpic1s.png", align="center")),
             column(6,img(src="razor_clam_pop_DD.png", align="left"))),
    tabPanel("Explore dynamics", 
             column(2, 
                    sliderInput("u", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                    sliderInput("yct", "Fishing closed:", value=rep(0,2), min=0, max=20, step=1)),
             column(10, plotOutput("CompareConstantHarvest"))),
    tabPanel("Learn about risks",
             h4("Background information on the top four risks to the razor clam resource that the Quinault mentioned during interviews."),
             tabsetPanel(
               tabPanel("Harmful algal blooms", img(src="red_tide.png", align="center")),
               tabPanel("Storm surge", img(src="storm_surge.png", align="center")),
               tabPanel("Pollution", img(src="oil_spill.png", align="center")),
               tabPanel("Habitat destruction", img(src="beach_construction.png", align="center"))
             )
             ),
    tabPanel("Explore risks", 
             column(2, 
                    h4("Scenarios of change"),
                    checkboxInput("HABs", div(strong("Harmful algal blooms"), style=paste0("color:", colors[2])), value=FALSE),
                    conditionalPanel(
                      condition = "input.HABs==true",
                      div(h5("Harvest rate set to zero during HAB closures. Does not affect razor clam survival rates."), style=paste0("color:", colors[2]))
                    ),
                    checkboxInput("inc_waves", div(strong("Increasing wave heights and storm surges"), style=paste0("color:", colors[1])), value=FALSE),
                    conditionalPanel(
                      condition = "input.inc_waves==true",
                      div(h5("Decreasing survival of pre-recruits over time."), style=paste0("color:", colors[1]))
                    ),
                    checkboxInput("pollution", div(strong("Pollution/oil spills"), style=paste0("color:", colors[3])), value=FALSE),
                    conditionalPanel(
                      condition = "input.pollution==true",
                      div(h5("Decreasing survival of pre-recruits and recruits over time."), style=paste0("color:", colors[3]))
                    ),
                    checkboxInput("dec_habitat", div(strong("Habitat destruction"), style=paste0("color:", colors[4])), value=FALSE),
                    conditionalPanel(
                      condition = "input.dec_habitat==true",
                      div(h5("Decreased beach capacity leads to more density-dependence on recruits."), style=paste0("color:", colors[4]))
                    )
             ),
             column(2, 
                    sliderInput("u2", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                    conditionalPanel(
                      condition="input.inc_waves==true | input.pollution==true",
                      sliderInput("min_Spre", "Minimum pre-recruit survival", value=0.11, min=0, max=0.11, step=0.01)
                    ),
                    conditionalPanel(
                      condition="input.pollution==true",
                      sliderInput("min_Srec", "Minimum recruit survival", value=0.7, min=0, max=0.7, step=0.01)
                    ),
                    conditionalPanel(
                      condition="input.HABs==true",
                      numericInput("hab_nyears", "Fishing closed x times:", value=1, step=1),
                      numericInput("hab_freq", "Every x years:", value=5, step=1)
                    ),
                    conditionalPanel(
                      condition="input.dec_habitat==true",
                      sliderInput("capacity", "Beach capacity:", value=1, min=0,max=1,step=0.01)
                    )
             ),
             column(8, plotOutput("ScenarioOutput")))
  )  
  
))  