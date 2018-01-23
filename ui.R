
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(RColorBrewer)
colors <- brewer.pal(4, "Set1")[c(2,3,1,4)]

shinyUI(fluidPage(
  titlePanel(h1("Razor clam population dynamics under scenarios of change")),
  tabsetPanel(
    tabPanel("Introduction",
      tabsetPanel(
        tabPanel("Welcome",
                      h3("Motivation", style="color: darkgoldenrod"),
                      tags$ul(
                        tags$li(h4("We conducted interviews with Quinault Indian Nation tribal members, natural resource members, and scientists between 2014-2016.")),
                        tags$li(h4("The tribal members identified many perceived risks to razor clam populations and their harvest, and even experienced drastic closures due to harmful algal blooms during the time of interviews.")),
                        tags$li(h4("While we identified consensus amongst tribal members that razor clams and their harvest are at risk, there was no consensus on ", strong("how"), " each risk directly impacts the razor clams and management system."))
                      ),
                        h3("Audience for this tool", style="color: darkgoldenrod"),
                        h4("Quinault tribal members and natural resource managers"),
                        h3("What is a model?", style="color: darkgoldenrod"),
                      tags$ul(
                        tags$li(h4("Models describe a system (using mathematical concepts or words) to study the effects of different components and make predictions about system behavior.")),
                        tags$li(h4("Population models are simplified descriptions of the rate that individuals are born, move through their life cycle, and die, and can incorporate interactions with the environment and other individuals around them that would affect their birth, growth, and death.")),
                        tags$li(h4("Population models can be used to predict how heavily a population can be fished sustainably given how quickly they grow and reproduce, how long an endangered population may take to recover, and how a population may be impacted by habitat or other environmental changes."))
                      ),
                        h3("Objectives for this tool", style="color: darkgoldenrod"),
                        tags$ol(
                          h4(tags$li("Provide information to tribal members about the biological mechanisms behind their perceived risks to the razor clam population and harvest.")),
                          h4(tags$li("Use a population model to help tribal members and managers explore potential impacts of perceived risks."))
                        )),
        tabPanel("Glossary",
                 h3("Definitions", style="color: darkgoldenrod"),
                 h4("Please refer to the following terms used throughout this tool."),
                 tags$ul(
                   h4(strong("Recruitment:"), "the number of new individuals (babies) entering the population."),
                   h4(strong("Pre-recruits:"),"razor clams below legal size for harvest."),
                   h4(strong("Recruits:"), "razor clams above legal size for harvest."),
                   h4(strong("Density-dependence:"), "slower population growth at large population sizes due to competition for space and food."),
                   h4(strong("Survival:"), "the rate at which razor clams survive to the next year, avoiding both dying naturally and from harvest"),
                   h4(strong("Harvest rate:"), "the amount of fishing pressure. For example, management aims to harvest 30% of legally-sized adult razor clams."),  
                   h4(strong("Variability:"), "the degree of population size fluctuation over time, due to the biology of the species or environmental conditions")
                 )),
        tabPanel("Contact us",
                 h4(strong("Authors:")),
                 h4("Kate Crosman, UW Evans School of Public Policy, katecros@uw.edu"),
                 h4("Eleni Petrou, UW School of Aquatic and Fishery Sciences, elpetrou@uw.edu"),
                 h4("Merrill Rudd, UW School of Aquatic and Fishery Sciences, mbrudd@uw.edu"),
                 h4("Mike Tillotson, UW School of Aquatic and Fishery Sciences, mdt3@uw.edu"),
                 br(),
                 h4(strong("This work was funded by the IGERT Program on Ocean Change")),
                 img(src="IGERT_logo.png", align="center")))),
    tabPanel("Learn about risks",
             h2("Background information: Top four perceived risks to the razor clam resource"),
             tabsetPanel(
               tabPanel("Learning about risks", 
                        h4("Based on key informant and group interviews with Quinault tribal members, the following four issues stood out to have the highest perceived risk to the razor clam population and harvest:"),
                        h4("1. Harmful algal blooms"),
                        h4("2. Storm surge and increasing wave height"),
                        h4("3. Pollution and oil spills"),
                        h4("4. Habitat destruction"),
                        h3("Click through to learn about the science behind each of these risks.")),
               tabPanel("Harmful algal blooms", img(src="red_tide.png", align="center")),
               tabPanel("Storm surge", column(8,
                        h3("Storm surge"),
                        h4("Young razor that have recently settled on beaches can be displaced by large waves that move sand. These displaced clams are likely to be eaten by fish that feed in the surf, and so strong waves can reduce razor clam survival. During our study, we heard reports that strong storms and high surf have become more common in recent years.  Warming in the ocean and atmosphere may be changing the paths of storms across the Pacific, or allowing for more intense storms to develop. In any case, if high surf continues to become more common, the survival of young razor clams might be reduced. To explore the impact of this change, increasing wave heights and storm surges in the model causes pre-recruit survival to decline consistently through time."),
                        img(src="storm_surge.png", align="center")
                        )),
               tabPanel("Pollution", column(8,
                        h3("Pollution"),
                        h4("Razor clams can be hurt by a variety of man-made pollutants including oil and runoff from roads and parking lots. During our study, we heard concerns about the potential for oil spills during ocean transportation, and increasing contamination from new development near beaches. Oil and other contaminants are toxic to marine life such as razor clams, and are likely to kill a portion of the population if they increase to dangerous levels. Ongoing monitoring confirms that razor clams are not presently contaminated, but if pollution increases in the future this may change. To explore this possibility, including increased pollution in the model will reduce both recruit and pre-recruit survival through time."),
                        img(src="oil_spill.png", align="center")
                        )),
               tabPanel("Habitat destruction", column(8,
                        h3("Habitat destruction"),
                        h4("Razor clams need beaches to live and grow on, and a loss of beach habitat is therefore likely to result in fewer clams. During our study we heard about changes in the extent and slope of clamming beaches. Increased storm activity, rising sea levels, dredging and coastal development that limits natural erosion may all impact the amount of beach habitat available for razor clams. As more and more young clams settle on a shrinking beach they compete for food which can reduce growth and survival. In the model, habitat destruction leads to decreased beach capacity and higher competition between clams which reduces survival when there are many clams."),
                        img(src="beach_construction.png", align="center")
                        ))
             )
    ),
    tabPanel("Population dynamics",
             h2("Population model structure and details"),
             column(3, img(src="RCpic1s.png", align="center"),
                    h3("Glossary", style=paste0("color: darkgoldenrod")),
                    tags$ul(
                      h4(strong("Recruitment:"), "the number of new individuals (babies) entering the population."),
                      h4(strong("Pre-recruits:"),"razor clams below legal size for harvest."),
                      h4(strong("Recruits:"), "razor clams above legal size for harvest."),
                      h4(strong("Density-dependence:"), "slower population growth at large population sizes due to competition for space and food."),
                      h4(strong("Survival:"), "the rate at which razor clams survive to the next year, avoiding both dying naturally and from harvest"),
                      h4(strong("Harvest rate:"), "the amount of fishing pressure. For example, management aims to harvest 30% of legally-sized adult razor clams."),  
                      h4(strong("Variability:"), "the degree of population size fluctuation over time, due to the biology of the species or environmental conditions")
                    )),
             column(9,img(src="razor_clam_pop_DD.png", align="left"))),
    tabPanel("Explore dynamics",
             h2("Test the model: How may the razor clam population change under higher or lower harvest rate? Years of harvest closures?"),
             column(3, 
                    h4("If the harvest rate increases, catch will be initially higher but then decrease as the population size cannot sustain that level of fishing pressure."),
                    h4("If the harvest rate decreases, the population size will increase as it is not being fished as hard."),
                    sliderInput("u", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                    h4("If fishing is closed for one or many years, this gives the population some time to rebound, and the number of recruits and potential catch could be higher in the future."),
                    sliderInput("yct", "Fishing closed:", value=rep(0,2), min=0, max=20, step=1)),
             column(9, plotOutput("CompareConstantHarvest"))),
    tabPanel("Explore risks",
        h2("Explore expected impact of each risk on the razor clam population"),
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
            h4("Line color corresponds to direct impact of each risk."), 
            br(), 
            h4("Advanced option:", style="color: #AA0000"),
            checkboxInput("explore_var", h4(div(strong("Explore variability"), style="color: #AA0000")), value=FALSE), 
            conditionalPanel(
              condition = "input.explore_var==true",
              h4("Uncheck the 'Explore variability' box to go back to exploring predicted impacts without variability.", style="color: #AA0000")),
            width=3),
            
          mainPanel(
                       column(2, 
                              sliderInput("u2", "Harvest rate:", value=0.3, min=0, max=1, step=0.05),
                              conditionalPanel(
                                condition="input.inc_waves==true | input.pollution==true",
                                sliderInput("min_Spre", "Pre-recruit survival in final year", value=0.08, min=0, max=0.0888, step=0.01)
                              ),
                              conditionalPanel(
                                condition="input.pollution==true",
                                sliderInput("min_Srec", "Recruit survival in final year", value=0.35, min=0, max=0.4, step=0.01)
                              ),
                              conditionalPanel(
                                condition="input.HABs==true",
                                numericInput("hab_freq", "Closure every x years:", value=5, step=1)
                              ),
                              conditionalPanel(
                                condition="input.dec_habitat==true",
                                sliderInput("capacity", "Beach capacity in final year:", value=1, min=0,max=1,step=0.01)
                              ),
                              conditionalPanel(
                                condition = "input.explore_var==true",
                                numericInput("simyears", "Number of years to project into future:", value=20, min=0),
                                numericInput("nsims", "Number of simulations:", value=50, min=1),
                                sliderInput("recvar", "Recruitment variability", value=0.3, min=0, max=2, step=0.01)
                              )
                       ),
                       conditionalPanel(
                         condition = "input.explore_var==false",
                         column(8, plotOutput("ScenarioOutput"))                         
                       ),
                       conditionalPanel(
                         condition = "input.explore_var==true",
                         column(8, plotOutput("SimulationOutput"))                         
                       )
            )
          ))
          
        )
             
    )
  )
