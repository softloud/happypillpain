library(tidyverse)
library(shinythemes)
library(glue)

withr::with_dir(here::here(), {
  tar_load(outcomes_of_interest)
})

outcomes_text <-
  paste0(outcomes_of_interest, collapse = "; ")

fluidPage(
  theme = shinytheme("flatly"),
  h1("Treating chronic pain with antidepressants"),
  h2(
    "Outcome-specific fixed-effects network meta-analysis preliminary results for parallel post-intervention studies"
  ),
  
  sidebarLayout(
    sidebarPanel(
      p(
        glue(
          "These are network meta-analysis results for fixed-effects post-intervention results for: {outcomes_text}."
        )
      ),
      
      selectInput(
        inputId = "outcome",
        label = "Select an outcome:",
        choices = outcomes_of_interest,
        selected = "pain"
      ),
      
      h2("Scope of preliminary results"),
      p("- post-intervention"),
      p("- fixed-effects model"),
      p("- parallel design"),
      p("- arm-based analysis")
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("network",
               fluidRow(
                 column(
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
                 column(width = 12,
                        plotOutput("network"))
               ))#,
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
    # NULL
  )))
)

