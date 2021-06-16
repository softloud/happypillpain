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

  output$dat <- renderTable({
    
    this_model() %>% pluck("network", "agd_arm") %>% 
      select(-contains("criteria")) %>% 
      gt() %>% 
      hpp_tab()
  })
  
}
