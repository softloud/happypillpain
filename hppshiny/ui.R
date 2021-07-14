fluidPage(
  # applies accross panels --------------------------------------------------
  
  
  theme = shinytheme("flatly"),
  h1("Treating chronic pain with antidepressants"),
  h2(
    "Outcome-specific random-effects network meta-analysis"
  ),
  h3("Preliminary results")
  
  ,

  
  sidebarLayout(
    # sidebar -----------------------------------------------------------------
    
    sidebarPanel(
      width = 2,
      selectInput(
        inputId = "outcome",
        label = "Select an outcome:",
        choices = outcome_choices,
        selected = "pain"
      ),
      
      selectInput(
        inputId = "subgroup",
        label = "Subgroup by condition or class",
        choices = subgroup_choices,
        selected = "NA"
      ),
      
      selectInput(
        inputId = "subgroup_value",
        label = "Subgroup value",
        choices = "all",
        selected = "all"
      ),
      
      
      h3("Scope of preliminary results"),
      p("- post-intervention"),
      p("- random-effects model"),
      p("- parallel design"),
      p("- arm-based analysis"),
      p("- direction of improvement: higher")
    ),
    
    
    # main panel --------------------------------------------------------------
    
    
    mainPanel(tabsetPanel(

# network panel -----------------------------------------------------------

      
      tabPanel("network",
               fluidRow(column(
                 width = 4,
                 p(
                   "A network meta-analysis extends on pairwise meta-analysis,
                     allowing for the comparison of multiple treatments.
                     The method takes an incomplete set of pairwise comparisons,
                     shown in this network diagram. The algorithm estimates
                     the missing comparisons and uses the estimated complete
                     network to produce a set of comparisons of each treatment
                     from placebo."
                 )
               ),
               column(width = 6,
                      p("net"),
                      plotOutput("net"),
                      NULL
                      )
               )),
      
tabPanel("effects",
         fluidRow(
           column(
           width = 8,
           plotOutput("forest")
         ),
         column(
           width = 4,
           tableOutput("forest_dat")
         )
         
         )),


      
      tabPanel("data",
               fluidRow(column(
                 width = 6,
                 tableOutput("dat")
               )))
    ))
    
  )) # close fluid page
  
  
  #   # column(width = 12,
  #   #        plotOutput("network"))
  #   NULL
  #
  #   )
  # )#,
  #   tabPanel("estimated difference from placebo",
  #            fluidRow(
  #              column(
  #                width = 4,
  #                p(
  #                  "This provides estimates of the variations between the studies (upper segment) and
  #                       the contrasts (lower segment)."
  #                )
  #              ),
  #              column(width = 5,
  #                     plotOutput("estimates"))
  #            )),
  #   tabPanel(
  #     "plausible values, before and after",
  #     p(
  #       "This is a comparative way of inspecting the results shown in the estimated
  #                         difference from placebo tab. This shows the difference of the model's estimation (filled) of
  #                         the treatment's effect in comparison with placebo with our prior expectations (lines)."
  #     )
  #     ,
  #     plotOutput("prior_post")
  #   )
  # )),
  # NULL))
  # )
  # )
  