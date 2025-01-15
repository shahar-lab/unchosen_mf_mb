library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .output-text {
        font-size: 36px;  /* Increased font size */
        font-weight: bold;
        padding: 10px;    /* Added padding for spacing */
      }
    "))
  ),
  titlePanel("Bayesian Updating with Gaussian Prior, Likelihood, and Posterior"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x_min", "X-axis Minimum:", value = -5),
      numericInput("x_max", "X-axis Maximum:", value = 5),
      textInput("sample_values", "Enter Sample Values (comma-separated):", value = "1, 2, 3, 4, 5"),
      div(verbatimTextOutput("sample_sd_output"), class = "output-text"),
      div(verbatimTextOutput("max_likelihood_output"), class = "output-text"),
      numericInput("mu_prior", "Prior Mean (μ₀):", value = 0),
      numericInput("sigma_prior", "Prior Std Dev (σ₀):", value = 1, min = 0.1)
    ),
    mainPanel(
      plotOutput("bayesianPlot", height = "700px")
    )
  )
)

server <- function(input, output) {
  output$bayesianPlot <- renderPlot({
    # Input values
    mu_prior <- input$mu_prior
    sigma_prior <- input$sigma_prior
    sample_values <- as.numeric(unlist(strsplit(input$sample_values, ",")))
    sample_size <- length(sample_values)
    
    # Calculate sample statistics
    sample_mean <- mean(sample_values, na.rm = TRUE)
    sample_sd <- sd(sample_values, na.rm = TRUE)  # Use sample standard deviation
    max_likelihood <- sample_mean  # Maximum likelihood estimate for mean
    
    # Calculate posterior parameters
    posterior_variance <- 1 / ((1 / sigma_prior^2) + (sample_size / sample_sd^2))
    posterior_mean <- posterior_variance * ((mu_prior / sigma_prior^2) + (sample_size * sample_mean / sample_sd^2))
    posterior_sd <- sqrt(posterior_variance)
    
    # x values for plotting based on user-defined range
    x <- seq(input$x_min, input$x_max, length.out = 100)
    
    # Density calculations
    prior_density <- dnorm(x, mean = mu_prior, sd = sigma_prior)
    likelihood_density <- dnorm(x, mean = sample_mean, sd = sample_sd / sqrt(sample_size))
    posterior_density <- dnorm(x, mean = posterior_mean, sd = posterior_sd)
    
    # Data frame for plotting
    df <- data.frame(
      x = rep(x, 3),
      y = c(prior_density, likelihood_density, posterior_density),
      Distribution = rep(c("Prior", "Likelihood", "Posterior"), each = 100)
    )
    
    # Plot with larger fonts and new title
    ggplot(df, aes(x = x, y = y, color = Distribution, linetype = Distribution)) +
      geom_line(size = 1) +
      labs(
        title = "Posterior ≈ Likelihood × Prior",
        subtitle = "Bayesian Updating with Gaussian Distributions",
        x = "Value",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 20),          # Base text size
        plot.title = element_text(size = 24),    # Title text size
        plot.subtitle = element_text(size = 20), # Subtitle text size
        axis.title = element_text(size = 22),    # Axis title text size
        axis.text = element_text(size = 18),     # Axis text size
        legend.title = element_text(size = 20),  # Legend title text size
        legend.text = element_text(size = 18)    # Legend text size
      ) +
      xlim(input$x_min, input$x_max)
  })
  
  output$sample_sd_output <- renderText({
    sample_values <- as.numeric(unlist(strsplit(input$sample_values, ",")))
    sample_sd <- sd(sample_values, na.rm = TRUE)
    paste("Sample Standard Deviation:", round(sample_sd, 2))
  })
  
  output$max_likelihood_output <- renderText({
    sample_values <- as.numeric(unlist(strsplit(input$sample_values, ",")))
    sample_mean <- mean(sample_values, na.rm = TRUE)
    paste("Maximum Likelihood Estimate (Mean):", round(sample_mean, 2))
  })
}

shinyApp(ui = ui, server = server)
