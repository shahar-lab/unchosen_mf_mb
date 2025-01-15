# Function to calculate the standard deviation of a Beta distribution for vectors
beta_sd_vector <- function(mus, phis) {
  # Ensure mus and phis are of the same length
  if (length(mus) != length(phis)) {
    stop("Vectors 'mus' and 'phis' must have the same length.")
  }
  
  # Vectorized formula for SD
  sqrt((mus * (1 - mus)) / (phis + 1))
}

# Example: Vectors of means and precisions
mus <- c(0.26, 0.5, 0.44)
phis <- c(7.54, 3.76, 36.91)

# Calculate vector of SDs
sds <- beta_sd_vector(mus, phis)
print(sds)
