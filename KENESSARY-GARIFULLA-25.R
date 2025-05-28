
# =============================================================================
# Description : R script for ECDF visualization, normality checks (SF test),
#               ECDF vs Normal CDF, and Q-Q plots with Shapiro-Francia results.
# =============================================================================

# ----------------------------- Load Libraries ------------------------------- #
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(grid)
library(nortest)

# ----------------------------- Load & Prepare Data -------------------------- #
df <- read.csv("Medicaldataset.csv")

# Binary encode diagnosis result
df$Result <- ifelse(df$Result == "positive" | df$Result == 1, 1, 0)
df$Diagnosis <- factor(df$Result, labels = c("Negative (0)", "Positive (1)"))

# Remove implausible outliers
df <- df %>% filter(Heart.rate <= 1000)

# List of variables to analyze
vars <- c("Age", "Heart.rate", "Systolic.blood.pressure", 
          "Diastolic.blood.pressure", "Blood.sugar", "CK.MB", "Troponin")

# ----------------------------- ECDF by Diagnosis ---------------------------- #
create_ecdf_plot <- function(var, with_legend = FALSE) {
  p <- ggplot(df, aes_string(x = var, color = "Diagnosis")) +
    stat_ecdf(geom = "step", size = 1) +
    labs(title = paste("Empirical CDF:", var), x = var, y = "CDF") +
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
  if (!with_legend) p <- p + theme(legend.position = "none")
  return(p)
}

ecdf_plots <- lapply(vars, create_ecdf_plot)
ecdf_plots[[1]] <- create_ecdf_plot(vars[1], with_legend = TRUE)
ecdf_panel <- wrap_plots(ecdf_plots, ncol = 3, guides = "collect") & theme(legend.position = "right")
print(ecdf_panel)
#ggsave("ecdf_all_variables.png", ecdf_panel, width = 16, height = 10, dpi = 300)

# ----------------------------- Shapiro–Francia Test ------------------------- #
sf_results <- lapply(vars, function(var) {
  test <- sf.test(df[[var]])
  data.frame(Variable = var, Statistic = round(test$statistic, 4), P_value = signif(test$p.value, 5))
}) %>% bind_rows()
print(sf_results)

# --------------------- ECDF vs Theoretical Normal CDF ---------------------- #
df[vars] <- lapply(df[vars], scale)  # Standardize
create_ecdf_normal_plot <- function(var, with_legend = FALSE) {
  x <- df[[var]]
  plot_data <- data.frame(
    x = sort(x),
    Empirical = ecdf(x)(sort(x)),
    Normal = pnorm(sort(x))
  )
  p <- ggplot(plot_data, aes(x = x)) +
    geom_line(aes(y = Empirical, color = "Empirical CDF"), size = 1) +
    geom_line(aes(y = Normal, color = "Normal CDF"), linetype = "dashed", size = 1) +
    labs(title = paste(var, ": ECDF vs Normal CDF"), x = var, y = "CDF", color = "") +
    scale_color_manual(values = c("Empirical CDF" = "blue", "Normal CDF" = "red")) +
    theme_minimal()
  if (!with_legend) p <- p + theme(legend.position = "none")
  return(p)
}

ecdf_norm_plots <- lapply(vars, create_ecdf_normal_plot)
ecdf_norm_plots[[1]] <- create_ecdf_normal_plot(vars[1], with_legend = TRUE)
ecdf_norm_panel <- wrap_plots(ecdf_norm_plots, ncol = 3, guides = "collect") & theme(legend.position = "right")

print(ecdf_norm_panel)

#ggsave("ecdf_vs_normal_all_R.png", ecdf_norm_panel, width = 16, height = 10, dpi = 300)

# ----------------------------- Q-Q Plots with Shapiro–Francia Test ---------- #
create_qq_sf_plot <- function(var, with_legend = FALSE) {
  result <- sf.test(df[[var]])
  W <- round(result$statistic, 4)
  p_val <- signif(result$p.value, 5)
  p <- ggplot(df, aes(sample = get(var))) +
    stat_qq(aes(color = "Sample Quantiles"), size = 1.2) +
    stat_qq_line(aes(color = "Normal Line"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Sample Quantiles" = "blue", "Normal Line" = "red")) +
    labs(title = paste("Q-Q Plot:", var),
         subtitle = paste0("W = ", W, ", p = ", p_val),
         x = "Theoretical Quantiles", y = "Sample Quantiles", color = NULL) +
    theme_minimal()
  if (!with_legend) p <- p + theme(legend.position = "none")
  return(p)
}

qq_sf_plots <- lapply(vars, create_qq_sf_plot)
qq_sf_plots[[1]] <- create_qq_sf_plot(vars[1], with_legend = TRUE)
qq_sf_panel <- wrap_plots(qq_sf_plots, ncol = 3, guides = "collect") & theme(legend.position = "right")

print(qq_sf_panel)
#ggsave("qqplots_with_sf.png", qq_sf_panel, width = 16, height = 10, dpi = 300)
