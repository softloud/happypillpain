function(input, output) {
  #  apply selection filters ------------------------------------------------
  obs <- reactive({
    obs_dat %>%
      filter(outcome == input$outcome_select,
             timepoint == input$timepoint,
             str_detect(design, "parallel"),
             str_detect(type, "antidepressant|placebo")
             ) %>% 
      group_by(study) %>% 
      mutate(
        study_count = length(study)
      ) %>% 
      arrange(study) %>% 
      select(study_count, everything()) %>% 
      filter(study_count > 1)  %>% 
      ungroup() %>% 
      select(-outcome)
    
  })
  

# selection choices -------------------------------------------------------

output$studies <- reactive({
  obs() %>% pull(study) %>% unique()
})  
  
  
  mod_type <- reactive({
    m_key %>%
      filter(outcome == input$outcome_select) %>%
      pull(model_type)
  })
  
  # summary of findings -----------------------------------------------------
  sof_tab <- reactive({
    obs() %>%
      group_by(condition) %>% 
      summarise(
        participants = sum(n),
        studies = n_distinct(study),
        intervention = unique(intervention) %>% paste(collapse = ", ")
      ) %>% 
      mutate(
        intervention = str_remove_all(intervention, "NA, |placebo, ")
      ) %>%
      ungroup() %>% 
      gt(
      ) %>% 
      hpp_tab(vertical_divider = "condition")
    
  })
  
  output$sof <- render_gt(sof_tab())
  

# network meta-analysis ---------------------------------------------------

nma_net <- reactive({
  if (mod_type() == "smd") {
    set_agd_arm(
      data = obs_mod,
      study = study,
      trt = intervention,
      y = mean,
      se = se,
      sample_size = n,
      trt_ref = "placebo"
      # ,
      # trt_class = intervention
    )
  } else if (mod_type() == "lor") {
    set_agd_arm(
      data = obs_mod,
      study = study,
      trt = intervention,
      r = r,
      n= n,
      sample_size = n,
      trt_ref = "placebo"
      # ,
      # trt_class = intervention
    )
    
  }
})
  
  nma_mod <- reactive({
    # nma(nma_net(), trt_effects = "random")
    init_mod
  })
  
  output$net <- renderPlot({
    plot(init_net)
    # plot(nma_net())
  }
  )
  
  output$forest <- renderPlot({
    relative_effects(nma_mod()) %>% plot() +
      ggthemes::theme_tufte(base_size = 20)
    
  })
  
  output$tau <- renderPlot({
    tau_vals <- 
      summary(nma_mod()) %>% 
      as.data.frame() %>% 
      filter(parameter == "tau")
    
    tau_quantiles <- tau_vals %>% 
      select(4:8)
    
    tau_quant_labs <- 
      tau_quantiles %>% 
      names() %>% 
      paste(collapse = ", ")
    
    tibble(
      x = rstan::extract(nma_mod()$stanfit, pars = "tau")$tau %>% as.numeric()
    ) %>% 
      ggplot() +
      geom_density(aes(x = x)) +
      geom_vline(
        xintercept = tau_quantiles %>% t(),
        linetype = "dotted",
        alpha = 0.6
      ) +
      xlim(tau_vals$mean - 3*tau_vals$sd, tau_vals$mean + 3*tau_vals$sd) +
      ggthemes::theme_tufte(
        base_size = 15
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      labs(
        title = "Between-study variation",
        subtitle = TeX("$\\tau^2$"),
        y = "",
        x = "",
        caption = glue(
          "Dotted lines indicate quantiles: 
      {tau_quant_labs}")
      )
  })
  
  
  # data table --------------------------------------------------------------
  
  obs_tab <- reactive({
    # select relevant measurements for model
    output_selections <-
      obs() %>%
      select(study, intervention,
             condition = condition_general, 
             mean, sd, r, n, scale) 
    
    output_dat <-   
               if (mod_type() == "smd") {
                 output_selections %>% 
                 select(-r)
               } else if (mod_type() == "lor") {
                 output_selections %>% 
                 select(-mean,-sd)
               }
             
    
    output_dat %>%
      gt(groupname_col = "condition") %>%
      fmt_number(sd, decimals = 2) %>% 
      hpp_tab(vertical_divider = "study")
  })
  
  output$obs_output <- render_gt(expr = obs_tab())
}
