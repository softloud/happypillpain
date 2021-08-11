fluidPage(
  # applies accross panels --------------------------------------------------
  
  
  theme = shinytheme("sandstone"),
  
  titlePanel(
    "Treating chronic pain with antidepressants"
    
  ),
  
  sidebarLayout(
    
    # sidebar -----------------------------------------------------------------
    
    sidebarPanel(
      
      h5(
        "Outcome-specific random-effects network meta-analysis"
      ),
      h6("Preliminary results"),
      
      inputPanel(
        
        align = "center",
        
        submitButton(text = str_wrap("Update analysis", 30),
                     icon = icon("calculator")
        ),
        p("Update the analysis based on selections made in Specifications panel"
          
      )),
      
 
      
      fluidRow(
        align = "left",
        
        h3("Scope of preliminary results"),
        p("- post-intervention"),
        p("- random-effects model"),
        p("- parallel design"),
        p("- arm-based analysis")
        
      ),
      
      inputPanel(
        selectInput(
          inputId = "outcome_select",
          label = "Outcome",
          choices = obs_dat %>% pull(outcome) %>% unique(),
          selected = "pain_int"
        )),
        
        inputPanel(
          selectInput(
            inputId = "condition",
            label = "Condition",
            choices = obs_dat %>% pull(condition_general) %>% unique()
          )
        ),
        
        inputPanel(
          checkboxGroupInput(
            inputId = "studies",
            label = "Studies",
            choices = obs_dat %>% pull(study) %>% unique()
          ),
          selectInput(
            "timepoint",
            label = "Timepoint",
            choices = obs_dat %>% pull(timepoint) %>% unique(),
            selected = "post_int"
          ))
          
          
        
    ),
    
    
    # main panel --------------------------------------------------------------
    
    
    mainPanel(tabsetPanel(
      
  
# summary of effects ------------------------------------------------------

tabPanel(
  "Outcome-specific findings",
  
  # ‘Summary of findings’ tables include a row for each important outcome (up to a maximum of seven). Accepted formats of ‘Summary of findings’ tables and interactive ‘Summary of findings’ tables can be produced using GRADE’s software GRADEpro GDT.
  # https://training.cochrane.org/handbook/current/chapter-14
  
  # gt_output(outputId = "sof"),
  tableOutput("sof"),
  
  NULL
),      
      
      
# network panel -----------------------------------------------------------

      
      tabPanel("network meta-analysis",
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
               column(width = 8,
                      plotOutput("net"),
                      NULL
                      ),
               column(width = 8,
                      plotOutput("forest")),
               column(width = 3, plotOutput("tau"))
               )),

      
      tabPanel("data",
               fluidRow(column(
                 width = 10,
                 gt_output("obs_output")
               )))
    )) # close main panel 
    
  )
) # close fluid page
  
  

  