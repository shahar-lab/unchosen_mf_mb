library(bayestestR)
library(ggplot2)
library(posterior)
# List of file paths for MB and MF models
mb_files <- c("mb_unch_n25.Rdata", "mb_unch_n50.Rdata", "mb_unch_n100.Rdata", 
              "mb_unch_n150.Rdata", "mb_unch_n200.Rdata", "mb_unch_n300.Rdata", 
              "mb_unch_n400.Rdata", "mb_unch_n500.Rdata")

mf_files <- c("mf_unch_n25.Rdata", "mf_unch_n50.Rdata", "mf_unch_n100.Rdata", 
              "mf_unch_n150.Rdata", "mf_unch_n200.Rdata", "mf_unch_n300.Rdata", 
              "mf_unch_n400.Rdata", "mf_unch_n500.Rdata")

# Placeholder to store results
sample_sizes <- c(25, 50, 100, 150, 200, 300, 400, 500)
hdi_widths_MB <- c()
hdi_widths_MF <- c()

# Function to load Rdata and extract HDI width
extract_hdi_width <- function(file_path) {
  load(file_path)  # This loads the model object, adjust based on object name
  model_name <- ls()[sapply(ls(), function(x) inherits(get(x), "brmsfit"))]
  brms_fit <- get(model_name)
  # Extract posterior draws
  posterior <- as_draws_df(brms_fit)
  
  # Extract the coefficient for 'previous_unique_reward1'
  hdi_vals <- hdi(posterior$b_previous_unique_reward1)  # Adjust to your parameter name
  
  return(abs(hdi_vals$CI_high - hdi_vals$CI_low))
}

# Loop over MB files
for (file in mb_files) {
  file=paste0("data/stanmodel_mb_mf_unchosen_ca_beta/precision/",file)
  hdi_widths_MB <- c(hdi_widths_MB, extract_hdi_width(file))
}

# Loop over MF files
for (file in mf_files) {
  file=paste0("data/stanmodel_mb_mf_unchosen_ca_beta/precision/",file)
  hdi_widths_MF <- c(hdi_widths_MF, extract_hdi_width(file))
}

# Create a dataframe for plotting
plot_data <- data.frame(
  Sample_Size = rep(sample_sizes, 2),
  HDI_Width = c(hdi_widths_MB, hdi_widths_MF),
  Analysis = rep(c("MB", "MF"), each = length(sample_sizes))
)

# Plot using ggplot2
ggplot(plot_data, aes(x = Sample_Size, y = HDI_Width, color = Analysis)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Sample size", y = "HDI95% width") +
  scale_color_manual(values = c("green", "blue"))+theme_bw()
