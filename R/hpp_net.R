hpp_net <- function(dat, type) {

  n_placebo <- 
    dat %>% 
    filter(intervention == "placebo") %>% 
    nrow()
  
  ref <- if (n_placebo > 0) "placebo" else NULL

  if (type == "smd") {
    set_agd_arm(
      data = dat,
      study = study,
      trt = intervention,
      trt_ref = ref,
      y = mean,
      se = se,
      sample_size = n
    )
  } else if (type == "binomial") {
    set_agd_arm(
      data = dat,
      study = study,
      trt = intervention,
      trt_ref = ref,
      r = r,
      n = n,
      sample_size = n
    )  }
}