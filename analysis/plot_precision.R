
# plot --------------------------------------------------------------------
library(ggplot2)

file_paths <- c(
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n25.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n50.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n100.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n150.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n200.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n300.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n400.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n500.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n25.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n50.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n100.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n150.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n200.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n300.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n400.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n500.Rdata")

results <- lapply(file_paths, function(path) {
  e <- new.env()
  load(path, envir = e)  # Load the .Rdata file into a new environment
  # Assume the object name you need to work with is known, here we use `fit` as an example
  if(is.null(e$mf_unch_b)){
    fit <- e$mb_unch_b  # Access the brmsfit object from the new environment{
    analysis="mb"    
  }
  else{
    fit <- e$mf_unch_b  # Access the brmsfit object from the new environment{
    analysis="mf"    
  }
    
  if ("previous_unique_reward1" %in% rownames(summary(fit)$fixed)) {
    lower_ci <- summary(fit)$fixed["previous_unique_reward1", "l-95% CI"]
    upper_ci <- summary(fit)$fixed["previous_unique_reward1", "u-95% CI"]
    ci_width <- abs(upper_ci - lower_ci) 
  }
  num_participants <- length(unique(fit$data$subject))
  data.frame(
    analysis=analysis,
    sample_size = num_participants,
    ci_width = ci_width)
})
results_df <- do.call(rbind, results)

ggplot(results_df, aes(x = sample_size, y = ci_width,color=analysis)) +
  geom_line() +
  geom_point() +
  theme_minimal()+
theme(legend.position = "none", 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank())
