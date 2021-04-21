library(tidyverse)
library(multinma)
conflicted::conflict_prefer("filter", "dplyr")
library(targets)

# pretty sure I can do some static stuff that doesn't run
# every time someone changes something in the app
# I think this runs when the app is first spun up

# get data
withr::with_dir(here::here(), {
  tar_load(models)
})

function(input, output) {
  outcome_nma <- reactive({
    models %>%
      keep(~ .$outcome == input$outcome) %>%
      pluck(1)
  })
  
  output$choices <- reactive({
    models %>% 
      map_chr("outcome")
  })

  output$network <- renderPlot({
    plot(outcome_nma()$network)
  })

  output$estimates <- renderPlot({
    plot_input <- outcome_nma()

    plot(plot_input)
  })

  output$prior_post <- renderPlot({
    plot_prior_posterior(outcome_nma())
  })

}
