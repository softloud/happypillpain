function(input, output) {
  output$subgroup <- reactive({
    key %>% 
      filter(outcome == input$outcome) %>% 
      pull(subgroup) %>% 
      unique()
  })
  
  observeEvent(input$outcome, {
    subgroup_choices <- key %>% 
      filter(outcome == input$outcome) %>% 
      pull(subgroup) %>% 
      unique()
    updateSelectInput(inputId = "subgroup", choices = subgroup_choices)
  })
  
  observeEvent(input$subgroup, {
    values <- key %>% 
      filter(outcome == input$outcome, subgroup == input$subgroup) %>% 
      pull(subgroup_value) %>% 
      unique()
    updateSelectInput(inputId = "subgroup_value", choices = values)
  })
  
  this_model <- reactive({
  
    key %>% 
      filter(
        outcome == input$outcome,
        subgroup == input$subgroup,
        subgroup_value == input$subgroup_value
      ) %>% pluck("model_index") %>% 
      models[.] %>% pluck(1, 'result')
  
  })
  
  output$net <- renderPlot({
    this_model() %>% pluck("network") %>% plot()
  })
  
  rel_effects <- reactive({
    effects <- this_model()$network$treatments
    
    ref <- if ("placebo" %in% effects ) "placebo" else NULL
    
    this_model() %>% 
      relative_effects(trt_ref = ref, probs = c(0.025, 0.975))
  })
  
  output$forest <- renderPlot(
    height = 600,
    {
    rel_effects() %>% plot(ref_line = 0) +
        theme(axis.text = element_text(size = 20))
  })
  
  output$forest_dat <- renderTable({
    rel_effects() %>% as_tibble() %>% select(1:5) %>% 
      mutate(
        parameter = str_replace(parameter, "d\\[", "") %>% 
          str_replace("\\]", "")
      )
  }, 
  striped = TRUE,
  hover = TRUE
  ) 

  output$dat <- renderTable({
    
    this_model() %>% pluck("network", "agd_arm") %>% 
      select(-contains("criteria"), 
             -contains("sponsorship"), 
             -contains("total_number")) %>% 
      gt() %>% 
      hpp_tab()
  },
  striped = TRUE,
  hover = TRUE
  )
  
}
