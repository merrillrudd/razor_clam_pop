
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
      tabsetPanel(
        tabPanel("Welcome",
                 h3("Motivation", style=paste0("color: darkgoldenrod")),
                        h4("We conducted interviews with Quinault Indian Nation tribal members, natural resource members, and scientists between 2014-2016."),
                        h4("The tribal members identified many perceived risks to razor clam populations and their harvest, and even experienced drastic closures due to harmful algal blooms during the time of interviews."),
                        h4("Based on our interviews, there appepared to be a disconnect between the tribal members' experiences and perceived risks and the scientific mechanisms and uncertainty behind the potential impacts to razor clams."),
                        h3("Audience for this tool", style=paste0("color: darkgoldenrod")),
                        h4("Quinault tribal members and natural resource managers"),
                        h3("Objectives for use", style=paste0("color: darkgoldenrod")),
                        tags$ol(
                          h4(tags$li("Provide information to tribal members about the biological mechanisms behind their perceived risks to the razor clam population and harvest.")),
                          h4(tags$li("Help tribal members and managers explore potential impacts of perceived risks using a simplified population model."))
                        )),
        tabPanel("Glossary",
                 h3("Definitions", style=paste0("color: darkgoldenrod")),
                 h4("Please refer to the following terms used throughout this tool."),
                 tags$ul(
                   h4(strong("Pre-recruits:"),"razor clams below legal size for harvest."),
                   h4(strong("Recruits:"), "razor clams above legal size for harvest."),
                   h4(strong("Density-dependence:"), "slower population growth at large population sizes due to competition for space and food."),
                   h4(strong("Survival:"), "the rate at which razor clams survive to the next year, avoiding both dying naturally and from harvest"),
                   h4(strong("Harvest rate:"), "the amount of fishing pressure. For example, management aims to harvest 30% of legally-sized adult razor clams.")               
                 )),
        tabPanel("Contact us",
                 h4(strong("Authors:")),
                 h4("Kate Crosman, UW Evans School of Public Policy, katecros@uw.edu"),
                 h4("Eleni Petrou, UW School of Aquatic and Fishery Sciences, elpetrou@uw.edu"),
                 h4("Merrill Rudd, UW School of Aquatic and Fishery Sciences, mbrudd@uw.edu"),
                 h4("Mike Tillotson, UW School of Aquatic and Fishery Sciences, mdt3@uw.edu"),
                 br(),
                 h4(strong("This work was funded by the IGERT Program on Ocean Change")),
                 img(src="IGERT_logo.png", align="left")))),
    tabPanel("Population dynamics", 
             column(3, img(src="RCpic1s.png", align="center"),
                    h3("Glossary", style=paste0("color: darkgoldenrod")),
                    h4(strong("Pre-recruits:"),"razor clams below legal size for harvest."),
                    h4(strong("Recruits:"), "razor clams above legal size for harvest."),
                    h4(strong("Density-dependence:"), "slower population growth at large population sizes due to competition for space and food."),
                    h4(strong("Survival:"), "the rate at which razor clams survive to the next year, avoiding both dying naturally and from harvest"),
                    h4(strong("Harvest rate:"), "the amount of fishing pressure. For example, management aims to harvest 30% of legally-sized adult razor clams.")
             ),
             column(9,img(src="razor_clam_pop_DD.png", align="left"))),
    tabPanel("Explore dynamics", 
             column(3, 
                    h4("If the harvest rate increases, catch will be initially higher but then decrease as the population size cannot sustain that level of fishing pressure."),
                    h4("If the harvest rate decreases, the population size will increase as it is not being fished as hard."),
                    sliderInput("u", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                    h4("If fishing is closed for one or many years, this gives the population some time to rebound, and the number of recruits and potential catch could be higher in the future."),
                    sliderInput("yct", "Fishing closed:", value=rep(0,2), min=0, max=20, step=1)),
             column(9, plotOutput("CompareConstantHarvest"))),
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
        sidebarLayout(
          sidebarPanel(
            h3("Scenarios of change"),
            checkboxInput("HABs", h4(div(strong("Harmful algal blooms"), style=paste0("color:", colors[2]))), value=FALSE),
            conditionalPanel(
              condition = "input.HABs==true",
              div(h4("Harvest rate set to zero during HAB closures. Does not affect razor clam survival rates."), style=paste0("color:", colors[2]))
            ),
            checkboxInput("inc_waves", h4(div(strong("Increasing wave heights and storm surges"), style=paste0("color:", colors[1]))), value=FALSE),
            conditionalPanel(
              condition = "input.inc_waves==true",
              div(h4("Decreasing survival of pre-recruits over time."), style=paste0("color:", colors[1]))
            ),
            checkboxInput("pollution", h4(div(strong("Pollution/oil spills"), style=paste0("color:", colors[3]))), value=FALSE),
            conditionalPanel(
              condition = "input.pollution==true",
              div(h4("Decreasing survival of pre-recruits and recruits over time."), style=paste0("color:", colors[3]))
            ),
            checkboxInput("dec_habitat", h4(div(strong("Habitat destruction"), style=paste0("color:", colors[4]))), value=FALSE),
            conditionalPanel(
              condition = "input.dec_habitat==true",
              div(h4("Decreased beach capacity leads to more density-dependence on recruits."), style=paste0("color:", colors[4]))
            ),
            h4("Line color corresponds to direct impact of each risk."), width=3),
          mainPanel(
            tabsetPanel(
              tabPanel("Expectation",
                       column(2, 
                              sliderInput("u2", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                              conditionalPanel(
                                condition="input.inc_waves==true | input.pollution==true",
                                sliderInput("min_Spre", "Minimum pre-recruit survival", value=0.08, min=0, max=0.0888, step=0.01)
                              ),
                              conditionalPanel(
                                condition="input.pollution==true",
                                sliderInput("min_Srec", "Minimum recruit survival", value=0.4, min=0, max=0.4, step=0.01)
                              ),
                              conditionalPanel(
                                condition="input.HABs==true",
                                numericInput("hab_freq", "Closure every x years:", value=5, step=1)
                              ),
                              conditionalPanel(
                                condition="input.dec_habitat==true",
                                sliderInput("capacity", "Beach capacity:", value=1, min=0,max=1,step=0.01)
                              )
                       ),
                       column(8, plotOutput("ScenarioOutput"))),
              tabPanel("Variation and Uncertainty",
                       column(2,
                              numericInput("simyears", "Number of years to project into future:", value=20, min=0),
                              numericInput("nsims", "Number of simulations:", value=10, min=1),
                              sliderInput("recvar", "Recruitment variability", value=0, min=0, max=2, step=0.01)),
                       column(8, plotOutput("SimulationOutput")))
            )
          ))
          
        )
             
    )
  )
) 
