# Confirmatory Factor Analysis ---------------------------------------------

library(lavaan)

# Import data ---------------------------------------------------------------

data_path <- file.path("data", "cfa.csv")
if (!file.exists(data_path)) {
  stop("Private dataset not included. Expected file: data/cfa.csv")
}

cfa_data <- read.csv(data_path, header = TRUE)

# Specify competing measurement models -------------------------------------

# Five-factor model: four competition dimensions and one negative-emotion factor.
five_factor_model <- "
  f1 =~ comp1 + comp2 + comp3 + comp4
  f2 =~ comp5 + comp6 + comp7 + comp8
  f3 =~ comp9 + comp10 + comp11 + comp12
  f4 =~ comp13 + comp14 + comp15 + comp16
  f5 =~ ne1 + ne2
"

# Three-factor model: broader competition factors plus negative emotion.
three_factor_model <- "
  f1 =~ comp1 + comp2 + comp3 + comp4 + comp5 + comp6 + comp7 + comp8 +
        comp9 + comp10 + comp11 + comp12
  f2 =~ comp13 + comp14 + comp15 + comp16
  f3 =~ ne1 + ne2
"

# Estimate models -----------------------------------------------------------

fit_five_factor <- cfa(five_factor_model, data = cfa_data)
fit_three_factor <- cfa(three_factor_model, data = cfa_data)

# Review fit indices, standardized loadings, and normalized residuals -------

summary(
  fit_five_factor,
  standardized = TRUE,
  ci = TRUE,
  fit.measures = TRUE
)
inspect(fit_five_factor, what = "std")$lambda
resid(fit_five_factor, type = "normalized")$cov

summary(
  fit_three_factor,
  standardized = TRUE,
  ci = TRUE,
  fit.measures = TRUE
)
inspect(fit_three_factor, what = "std")$lambda
resid(fit_three_factor, type = "normalized")$cov
