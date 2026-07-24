# Time-Trend Regression with Newey-West Standard Errors ---------------

library(ggplot2)
library(lmtest)
library(sandwich)

# The original dataset is not included. Place it at data/JIS_copy.csv.
data_path <- file.path("data", "JIS_copy.csv")
if (!file.exists(data_path)) {
  stop("Private dataset not included. Expected file: data/JIS_copy.csv")
}

data_jis <- read.csv(data_path, header = TRUE)

# Original coding: Condition 1 = treatment; Condition 2 = control.
# Separate linear time trends are estimated for each outcome and condition.
fit_time_model <- function(outcome, condition_code) {
  formula <- reformulate("Time", response = outcome)
  model <- lm(
    formula,
    data = data_jis,
    subset = Condition == condition_code
  )

  list(
    model = model,
    conventional_results = summary(model),
    newey_west_results = coeftest(
      model,
      vcov = NeweyWest(
        model,
        lag = 1,
        prewhite = FALSE,
        adjust = TRUE
      )
    )
  )
}

# Preserve the six original model specifications while reducing repetition.
bpsys_treatment <- fit_time_model("BPSys", "1")
bpsys_control <- fit_time_model("BPSys", "2")
bpdia_treatment <- fit_time_model("BPDia", "1")
bpdia_control <- fit_time_model("BPDia", "2")
hr_treatment <- fit_time_model("HR", "1")
hr_control <- fit_time_model("HR", "2")

# Prepare mean outcome values by trial and condition for visualization.
mean_by_trial <- function(outcome) {
  aggregate(
    reformulate(c("Trial", "Condition"), response = outcome),
    data = data_jis,
    FUN = function(x) mean(as.numeric(as.character(x)), na.rm = TRUE)
  )
}

plot_outcome <- function(plot_data, outcome, title, subtitle, y_label) {
  ggplot(
    plot_data,
    aes(x = Trial, y = .data[[outcome]], colour = factor(Condition))
  ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    geom_vline(xintercept = 2, linetype = "dashed") +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Time-trend analysis with Newey-West standard errors",
      x = "Trial",
      y = y_label,
      colour = "Condition"
    ) +
    theme_bw(base_size = 12)
}

plot_bpsys <- plot_outcome(
  mean_by_trial("BPSys"), "BPSys",
  "Impact of imagined stress on systolic blood pressure",
  "Change in systolic blood pressure over time",
  "Systolic blood pressure"
)

plot_bpdia <- plot_outcome(
  mean_by_trial("BPDia"), "BPDia",
  "Impact of imagined stress on diastolic blood pressure",
  "Change in diastolic blood pressure over time",
  "Diastolic blood pressure"
)

plot_hr <- plot_outcome(
  mean_by_trial("HR"), "HR",
  "Impact of imagined stress on heart rate",
  "Change in heart rate over time",
  "Heart rate"
)

print(bpsys_treatment$newey_west_results)
print(bpsys_control$newey_west_results)
print(bpdia_treatment$newey_west_results)
print(bpdia_control$newey_west_results)
print(hr_treatment$newey_west_results)
print(hr_control$newey_west_results)
print(plot_bpsys)
print(plot_bpdia)
print(plot_hr)
