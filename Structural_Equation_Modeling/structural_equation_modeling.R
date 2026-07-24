# Structural Equation Modeling ---------------------------------------------

library(lavaan)
library(semPlot)

# Import data ---------------------------------------------------------------

data_path <- file.path("data", "cwbper2023.csv")
if (!file.exists(data_path)) {
  stop("Private dataset not included. Expected file: data/cwbper2023.csv")
}

sem_data <- read.csv(data_path, header = TRUE)

# Proposed latent-variable model -------------------------------------------
# Comp is measured by tangible rewards, recognition, status, and
# non-tangible rewards. Organizational and interpersonal CWB are modeled as
# correlated latent variables. Job insecurity and performance are included
# in the original structural sequence.

latent_sem_model <- "
  Comp =~ TanRewards + Recognitioin + Status + NontanRewards

  JobInsecurity ~ Comp

  CWBo =~ Theft + ProductionDeviance
  CWBp =~ Abuse + Sabotage

  CWBo ~ JobInsecurity + Comp
  CWBp ~ JobInsecurity + Comp

  Performance ~ CWBo + CWBp + Comp + JobInsecurity

  CWBp ~~ CWBo
"

fit_latent_sem <- sem(
  latent_sem_model,
  data = sem_data,
  std.lv = TRUE,
  se = "boot",
  bootstrap = 5000,
  estimator = "ML"
)

summary(
  fit_latent_sem,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)
parameterEstimates(fit_latent_sem, standardized = TRUE)

semPaths(
  fit_latent_sem,
  what = "paths",
  whatLabels = "std",
  rotation = 2
)

# Original observed-outcome comparison model -------------------------------
# The five CWB dimensions are treated as observed outcomes with correlated
# residuals, preserving the second proposed specification from the source.

observed_cwb_model <- "
  Comp =~ NontanRewards + TanRewards + Recognitioin + Status

  Theft ~ Comp + JobInsecurity
  ProductionDeviance ~ Comp + JobInsecurity
  Withdrawal ~ Comp + JobInsecurity
  Abuse ~ Comp + JobInsecurity
  Sabotage ~ Comp + JobInsecurity

  Performance ~ Theft + ProductionDeviance + Withdrawal + Abuse + Sabotage +
                JobInsecurity

  Theft ~~ ProductionDeviance + Withdrawal + Abuse + Sabotage
  ProductionDeviance ~~ Withdrawal + Abuse + Sabotage
  Withdrawal ~~ Abuse + Sabotage
  Abuse ~~ Sabotage
"

fit_observed_cwb <- sem(
  observed_cwb_model,
  data = sem_data,
  std.lv = TRUE,
  estimator = "ML"
)

summary(
  fit_observed_cwb,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)
parameterEstimates(fit_observed_cwb, standardized = TRUE)

semPaths(
  fit_observed_cwb,
  what = "paths",
  whatLabels = "std",
  rotation = 2
)

# Compare the original proposed specifications when model requirements allow.
anova(fit_latent_sem, fit_observed_cwb)
