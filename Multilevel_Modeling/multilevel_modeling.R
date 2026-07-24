# Multilevel Modeling -------------------------------------------------------

library(haven)
library(lme4)

# Import data ---------------------------------------------------------------

data_path <- file.path("data", "lab9.sav")
if (!file.exists(data_path)) {
  stop("Private dataset not included. Expected file: data/lab9.sav")
}

multilevel_data <- read_sav(data_path)
multilevel_data <- subset(
  multilevel_data,
  select = c("id", "valuec", "grades", "reqc", "respsize")
)

# Model 1: random-intercept model ------------------------------------------
# Repeated grade observations are nested within participant ID.

random_intercept_model <- lmer(
  grades ~ 1 + (1 | id),
  data = multilevel_data
)
summary(random_intercept_model)

# Model 2: add the original centered value predictor -----------------------

value_model <- lmer(
  grades ~ valuec + (1 | id),
  data = multilevel_data
)
summary(value_model)
