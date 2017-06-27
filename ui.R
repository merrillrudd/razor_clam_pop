
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
    tabPanel("Introduction",
      column(5, h3("Goals"),
             h4("Based on interviews with Quinault Indian Nation tribal members, natural resource managers, and scientists, we developed this tool to:"),
             h4("1. Provide information to tribal members about the biological mechanisms behind their perceived risks to the razor clam population and harvest."),
             h4("2. Explore potential impacts of perceived risks to the razor clam population and harvest using a simplified population model."),
             br(),
             h6("Authors: Merrill Rudd, Kate Crosman, Eleni Petrou, Michael Tillotson")),
      column(5, h3("Definitions"),
             h4("Please refer to the following terms used throughout this tool."),
             h4(strong("Pre-recruits:"),"razor clams below legal size for harvest."),
             h4(strong("Recruits:"), "razor clams above legal size for harvest."),
             h4(strong("Density-dependence:"), "slower population growth at large population sizes due to competition for space and food."),
             h4(strong("Survival:"), "the rate at which razor clams survive to the next year, avoiding both dying naturally and from harvest"),
             h4(strong("Harvest rate:"), "the amount of fishing pressure. For example, management aims to harvest 30% of legally-sized adult razor clams.")
      )),
    tabPanel("Population dynamics", 
             column(2, img(src="RCpic1s.png", align="center"),
                    h5(strong("Pre-recruits:"),"razor clams below legal size for harvest."),
                    h5(strong("Recruits:"), "razor clams above legal size for harvest."),
                    h5(strong("Density-dependence:"), "slower population growth at large population sizes due to competition for space and food."),
                    h5(strong("Survival:"), "the rate at which razor clams survive to the next year, avoiding both dying naturally and from harvest"),
                    h5(strong("Harvest rate:"), "the amount of fishing pressure. For example, management aims to harvest 30% of legally-sized adult razor clams.")
             ),
             column(6,img(src="razor_clam_pop_DD.png", align="left"))),
    tabPanel("Explore dynamics", 
             column(2, 
                    h5("If the harvest rate increases, catch will be initially higher but then decrease as the population size cannot sustain that level of fishing pressure."),
                    h5("If the harvest rate decreases, the population size will increase as it is not being fished as hard."),
                    sliderInput("u", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                    h5("If fishing is closed for one or many years, this gives the population some time to rebound, and the number of recruits and potential catch could be higher in the future."),
                    sliderInput("yct", "Fishing closed:", value=rep(0,2), min=0, max=20, step=1)),
             column(8, plotOutput("CompareConstantHarvest"))),
    tabPanel("Learn about risks",
             h3("Background information: Top four perceived risks to the razor clam resource"),
             tabsetPanel(
               tabPanel("Learning about risks", 
                        h4(strong("Goal of this section:"),"Provide background information on biological mechanisms for each of the top four risks to the razor clam resource identified by Quinault Indian Nation tribal members."),
                        h5("Based on key informant and group interviews with Quinault tribal members, the following four issues stood out to have the highest perceived risk to the razor clam population and harvest:"),
                        h5("1. Harmful algal blooms"),
                        h5("2. Storm surge and increasing wave height"),
                        h5("3. Pollution and oil spills"),
                        h5("4. Habitat destruction")),
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
             column(8, plotOutput("ScenarioOutput"))),
    tabPanel("Variation and uncertainty", 
             column(2, h4("We can explore expected outcomes on harvest and the razor clam population associated with each perceived risk, but it is not guaranteed that these expected outcomes will occur.")),
             column(2, 
                    numericInput("simyears", "Number of years to project into future:", value=20, min=0),
                    numericInput("nsims", "Number of simulations:", value=10, min=1),
                    sliderInput("recvar", "Recruitment variability", value=0, min=0, max=2, step=0.01)),
             column(8, plotOutput("SimulationOutput")))
    )
  )
  
)