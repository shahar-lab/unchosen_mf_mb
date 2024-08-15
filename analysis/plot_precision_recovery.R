rm(list = ls())
source('./functions/my_starter.R')
path = set_workingmodel()


# Define the function to process each fit object
process_fit <- function(fit) {
  # Extract the q5 and q95 columns
  q5 <- fit$summary(c("population_locations[3]","population_locations[4]"))[,6]
  q95 <- fit$summary(c("population_locations[3]","population_locations[4]"))[,7]
  
  # Calculate the widths
  widths <- q95 - q5
  
  # Create a data frame with the results
  data.frame(widths = widths)
}

# Load the fit objects
fit_200 <- readRDS(paste0(path$data, '/modelfit_recovery_200', '.rds'))
fit_100 <- readRDS(paste0(path$data, '/modelfit_recovery_100', '.rds'))
fit_50 <- readRDS(paste0(path$data, '/modelfit_recovery_50', '.rds'))



# Process each fit object and combine the results
results_200 <- process_fit(fit_200)
results_100 <- process_fit(fit_100)
results_50 <- process_fit(fit_50)

# Add a column to identify the source of the data
results_200$Nsubjects <- 200
results_100$Nsubjects <- 100
results_50$Nsubjects <- 50

variables=c("c_mf_unch","c_mb_unch")
results_200$variables <- variables
results_100$variables <- variables
results_50$variables <- variables

# Combine the results into a single data frame
combined_results <- rbind(results_200, results_100, results_50)
combined_results$Nsubjects <- factor(combined_results$Nsubjects, levels = c("200", "100", "50"))
# Plot the results
p=ggplot(combined_results, aes(x = variables, y = q95, fill = Nsubjects)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs( x = "Parameter",
       y = "HDI 90% width",
       fill = "Nsubjects")

ggsave(filename = "C:/Users/Ido/My Drive/graphics/mf_mb_unchosen/precision/precision_recovery.jpg", plot = p, width = 400 / 72, height = 300 / 72)
