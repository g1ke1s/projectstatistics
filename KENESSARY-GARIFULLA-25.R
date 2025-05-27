library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(grid)

df <- read.csv("Medicaldataset.csv")

df$Result <- ifelse(df$Result == "positive" | df$Result == 1, 1, 0)
df$Diagnosis <- factor(df$Result, labels = c("Negative (0)", "Positive (1)"))

# Filter heart rate outliers
df <- df %>% filter(Heart.rate <= 1000)

vars <- c("Age", "Heart.rate", "Systolic.blood.pressure", 
          "Diastolic.blood.pressure", "Blood.sugar", "CK.MB", "Troponin")

# ECDFs among two diagnosis results ---------------------------------------------------------

ecdf_plot <- function(var) {
  ggplot(df, aes_string(x = var, color = "Diagnosis")) +
    stat_ecdf(geom = "step", size = 1) +
    labs(title = paste("Empirical CDF:", var), x = var, y = "CDF") +
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal() +
    theme(legend.position = "none")
}

# First plot with legend
ecdf_plot_with_legend <- function(var) {
  ggplot(df, aes_string(x = var, color = "Diagnosis")) +
    stat_ecdf(geom = "step", size = 1) +
    labs(title = paste("Empirical CDF:", var), x = var, y = "CDF", color = "Diagnosis") +
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
}

# Create list of plots
plots <- list()
plots[[1]] <- ecdf_plot_with_legend(vars[1])  
for (i in 2:length(vars)) {
  plots[[i]] <- ecdf_plot(vars[i])
}

# Combine and arrange plots with shared legend
final_plot <- wrap_plots(plots, ncol = 3, guides = "collect") & theme(legend.position = "right")

# Save the output
ggsave("ecdf_all_variables.png", plot = final_plot, width = 16, height = 10, dpi = 300)
print(final_plot)

# Sharipo-Fransua Test ---------------------------------------------------------------------------

library(nortest)
# List of numeric variables
vars <- c("Age", "Heart.rate", "Systolic.blood.pressure",
          "Diastolic.blood.pressure", "Blood.sugar", "CK.MB", "Troponin")

# Run SF test and collect results
results <- data.frame(Variable = character(), Statistic = numeric(), P_value = numeric())

for (var in vars) {
  test_result <- sf.test(df[[var]])
  results <- rbind(results, data.frame(
    Variable = var,
    Statistic = round(test_result$statistic, 4),
    P_value = signif(test_result$p.value, 5)
  ))
}
print(results)

# ECDF vs Normal CDF ---------------------------------------------------------------------------
# Standardize numeric variables
df[vars] <- lapply(df[vars], function(x) scale(x, center = TRUE, scale = TRUE))

# Function to generate ECDF vs Normal CDF plot
plot_ecdf_vs_normal <- function(var) {
  x <- df[[var]]
  data_plot <- data.frame(
    x = sort(x),
    empirical = ecdf(x)(sort(x)),
    theoretical = pnorm(sort(x))  # Normal CDF
  )
  
  ggplot(data_plot, aes(x = x)) +
    geom_line(aes(y = empirical, color = "Empirical CDF"), size = 1) +
    geom_line(aes(y = theoretical, color = "Normal CDF"), linetype = "dashed", size = 1) +
    labs(title = paste(var, ": ECDF vs Normal CDF"),
         x = var, y = "CDF", color = "") +
    theme_minimal() +
    scale_color_manual(values = c("Empirical CDF" = "blue", "Normal CDF" = "red")) +
    theme(legend.position = "none")
}

# First plot with legend
plot_with_legend <- function(var) {
  x <- df[[var]]
  data_plot <- data.frame(
    x = sort(x),
    empirical = ecdf(x)(sort(x)),
    theoretical = pnorm(sort(x))
  )
  
  ggplot(data_plot, aes(x = x)) +
    geom_line(aes(y = empirical, color = "Empirical CDF"), size = 1) +
    geom_line(aes(y = theoretical, color = "Normal CDF"), linetype = "dashed", size = 1) +
    labs(title = paste(var, ": ECDF vs Normal CDF"),
         x = var, y = "CDF", color = "") +
    theme_minimal() +
    scale_color_manual(values = c("Empirical CDF" = "blue", "Normal CDF" = "red")) +
    theme(legend.position = "right")
}

# Create plots
plots <- list()
plots[[1]] <- plot_with_legend(vars[1])
for (i in 2:length(vars)) {
  plots[[i]] <- plot_ecdf_vs_normal(vars[i])
}

# Combine with shared legend
final_plot <- wrap_plots(plots, ncol = 3, guides = "collect") & theme(legend.position = "right")

# Save and display
ggsave("ecdf_vs_normal_all_R.png", plot = final_plot, width = 16, height = 10, dpi = 300)
print(final_plot)

# QQ PLOTS ---------------------------------------------------------------------------

qq_plot_sf <- function(var) {
  test_result <- sf.test(df[[var]])
  w_val <- round(test_result$statistic, 4)
  p_val <- signif(test_result$p.value, 5)
  
  ggplot(df, aes(sample = get(var))) +
    stat_qq(aes(color = "Sample Quantiles"), size = 1.2) +
    stat_qq_line(aes(color = "Normal Line"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Sample Quantiles" = "blue", "Normal Line" = "red")) +
    labs(
      title = paste("Q-Q Plot:", var),
      subtitle = paste0("W = ", w_val, ", p = ", p_val),
      x = "Theoretical Quantiles", y = "Sample Quantiles", color = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# First plot with legend
qq_plot_with_legend <- function(var) {
  test_result <- sf.test(df[[var]])
  w_val <- round(test_result$statistic, 4)
  p_val <- signif(test_result$p.value, 5)
  
  ggplot(df, aes(sample = get(var))) +
    stat_qq(aes(color = "Sample Quantiles"), size = 1.2) +
    stat_qq_line(aes(color = "Normal Line"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Sample Quantiles" = "blue", "Normal Line" = "red")) +
    labs(
      title = paste("Q-Q Plot:", var),
      subtitle = paste0("W = ", w_val, ", p = ", p_val),
      x = "Theoretical Quantiles", y = "Sample Quantiles", color = NULL
    ) +
    theme_minimal()
}

# Build plots
plots <- list()
plots[[1]] <- qq_plot_with_legend(vars[1])
for (i in 2:length(vars)) {
  plots[[i]] <- qq_plot_sf(vars[i])
}
while (length(plots) < 9) {
  plots[[length(plots) + 1]] <- ggplot() + theme_void()
}

# Combine with shared legend
final_plot <- wrap_plots(plots, ncol = 3, guides = "collect") & theme(legend.position = "right")

# Save and print
ggsave("qqplots_with_sf.png", final_plot, width = 16, height = 10, dpi = 300)
print(final_plot)
