# Latent Growth Curve Modeling ---------------------------------------------

library(lavaan)

# Import data ---------------------------------------------------------------

data_path <- file.path("data", "MandFMonitoring.csv")
if (!file.exists(data_path)) {
  stop("Private dataset not included. Expected file: data/MandFMonitoring.csv")
}

growth_data <- read.csv(data_path, header = TRUE)

# Model 1: unconditional linear growth -------------------------------------
# i represents the latent intercept. s represents linear change across the
# four measurement occasions with fixed time scores 0, 1, 2, and 3.

unconditional_model <- "
  i =~ 1*Momt1 + 1*Momt2 + 1*Momt3 + 1*Momt4
  s =~ 0*Momt1 + 1*Momt2 + 2*Momt3 + 3*Momt4
"

fit_unconditional <- growth(
  unconditional_model,
  data = growth_data,
  estimator = "MLM"
)
summary(fit_unconditional, fit.measures = TRUE)
modificationIndices(fit_unconditional, sort. = TRUE)

# Model 2: correlated residuals across adjacent occasions ------------------

correlated_residual_model <- "
  i =~ 1*Momt1 + 1*Momt2 + 1*Momt3 + 1*Momt4
  s =~ 0*Momt1 + 1*Momt2 + 2*Momt3 + 3*Momt4

  Momt1 ~~ Momt2
  Momt2 ~~ Momt3
  Momt3 ~~ Momt4
"

fit_correlated_residuals <- growth(
  correlated_residual_model,
  data = growth_data,
  estimator = "MLM"
)
summary(fit_correlated_residuals, fit.measures = TRUE)
modificationIndices(fit_correlated_residuals, sort. = TRUE)

# Model 3: gender as a time-invariant predictor -----------------------------
# The original coding identifies SEX = 1 as male.

conditional_model <- "
  i =~ 1*Momt1 + 1*Momt2 + 1*Momt3 + 1*Momt4
  s =~ 0*Momt1 + 1*Momt2 + 2*Momt3 + 3*Momt4

  Momt1 ~~ Momt2
  Momt2 ~~ Momt3
  Momt3 ~~ Momt4

  i ~ SEX
  s ~ SEX
"

fit_conditional <- growth(
  conditional_model,
  data = growth_data,
  estimator = "MLM"
)
summary(fit_conditional, fit.measures = TRUE)
modificationIndices(fit_conditional, sort. = TRUE)
