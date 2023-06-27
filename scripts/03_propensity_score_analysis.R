# load data
library(smdi)
library(gtsummary)
library(tidyverse)

# load pre-queried and cleaned cohort data (smdi_data)
data <- smdi_data_complete
  
# step 1: compute propensity of exposure assignment
exposure_form <- as.formula(paste("exposure ~ ", paste(colnames(data %>% dplyr::select(-exposure)), collapse = " + ")))

exposure_fit <- stats::glm(
  exposure_form,
  data = data,
  family = "binomial"
)

odds_table <- exposure_fit %>% 
  gtsummary::tbl_regression(exponentiate = T)

# step 2: compute propensity scores and visualize overlap before matching
exposure_plot <- data %>% 
  dplyr::mutate(ps = fitted(exposure_fit))

# plot density/overlap before matching
exposure_plot %>% 
  ggplot2::ggplot(ggplot2::aes(x = ps, fill = factor(exposure))) +
  ggplot2::geom_density(alpha = .5) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    x = "Pr(exposure)",
    y = "Density",
    fill = "Exposed"
    )
