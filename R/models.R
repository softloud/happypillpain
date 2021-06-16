library(targets)
library(tidyverse)
library(multinma)

tar_load(w_obs)

model_key <-
  tibble(
    outcome = c("adverse", "mood_depression", "pain"),
    model_type = case_when(outcome == "adverse" ~ "binomial",
                           TRUE ~ "smd")
  )

viable_dat <- 
  function(dat, o) {
    model <- model_key %>% 
      filter(outcome == o) %>% 
      pull(model_type)
    
    # filter to at least 2 arms per study

    model_dat <-
    if (model == "smd") {
      dat %>% 
        filter(mean > 0, se > 0, n > 0)
    } else if (model == "binomial") {
      dat %>% 
        filter(n > 0, N > 0) %>% 
        mutate(across(c(n, N), as.integer))
    } 
    
    model_dat %>% 
      group_by(study) %>% 
      filter(length(arm) > 1) %>% 
      ungroup()
    
    
  }



conditions <-
  w_obs %>% 
    select(outcome, condition_group) %>% 
    distinct() %>% 
    filter(!is.na(condition_group)) %>% 
    mutate(dat = map2(outcome, condition_group, .f = function(o, c){
      w_obs %>% 
        filter(outcome == o,
               condition_group == c | is.na(condition_group)) %>% 
        viable_dat(o)
    })) %>% 
  mutate(subgroup = "condition_group") %>% 
  rename(subgroup_value = condition_group)

classes <-
  w_obs %>% 
  select(outcome, class) %>% 
  distinct() %>% 
  filter(!is.na(class), class != "n") %>% 
  mutate(dat = map2(outcome, class, .f = function(o, c){
    w_obs %>% 
      filter(outcome == o,
             class == c | is.na(class)) %>% 
      viable_dat(o)
  })) %>% 
  mutate(
    subgroup = "class"
  ) %>% 
  rename(subgroup_value = class)

model_dat <-
model_key %>% 
  select(outcome) %>% 
  mutate(dat = map(outcome, .f = function(o){
    w_obs %>% 
      filter(outcome == o) %>% 
      viable_dat(o)
  })) %>% 
  bind_rows(conditions) %>% 
  bind_rows(classes) %>% 
  mutate(
    dat = map(dat, ungroup),
    n_studies = map_int(dat, .f = function(d){
    d %>% pull(study) %>% n_distinct()
  })) %>% 
  filter(n_studies >= 2) %>% 
  arrange(desc(n_studies)) %>% 
  left_join(model_key, by = "outcome")


net_binom <- 
  model_dat %>% 
  filter(model_type == "binomial") %>% 
  mutate(
    net = map(dat, .f = function(mdat) {
      set_agd_arm(
        data = mdat,
        study = study,
        trt = intervention,
        r = n,
        n = N,
        sample_size = N,
        trt_ref = "placebo"
      )
    })
  )

net_smd <-
model_dat %>% 
  filter(model_type == "smd") %>% 
  mutate(
    net = map(dat, .f = function(mdat) {
      set_agd_arm(
        data = mdat,
        study = study,
        trt = intervention,
        y = mean,
        se = se,
        sample_size = n,
        trt_ref = "placebo"
      )
    })
  )

safe_nma <- safely(nma, otherwise = "failed")

models <- 
bind_rows(net_binom, net_smd) %>% 
  mutate(
    model = map(net, safe_nma, trt_effects = "random")
  )

write_rds(models, "hppshiny/intervention_run.rds")

