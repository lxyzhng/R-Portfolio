# Hypothesis Testing: t-tests and ANOVA -------------------------------

library(effsize)
library(ggplot2)

# Paired spider-anxiety example from the original lecture exercise.
# The values are included here so the analysis is self-contained.
spider_data <- data.frame(
  participant = factor(1:12),
  picture = c(30, 35, 45, 40, 50, 25, 38, 42, 33, 47, 36, 44),
  real_spider = c(55, 60, 72, 65, 78, 49, 63, 69, 58, 75, 61, 70)
)

spider_long <- reshape(
  spider_data,
  varying = c("picture", "real_spider"),
  v.names = "Anxiety",
  timevar = "Group",
  times = c("Picture", "Real spider"),
  direction = "long"
)

p_anxiety <- ggplot(spider_long, aes(x = Group, y = Anxiety)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(x = NULL, y = "Mean anxiety") +
  theme_minimal(base_size = 12)

paired_test <- t.test(spider_data$picture, spider_data$real_spider, paired = TRUE)
independent_test <- t.test(Anxiety ~ Group, data = spider_long, paired = FALSE)
cohens_d <- cohen.d(Anxiety ~ Group, data = spider_long)

# One-way ANOVA exercise retained from the original script.
set.seed(123)
anova_data <- data.frame(
  participant = factor(1:60),
  group = factor(rep(c("Control", "Run", "Party"), each = 20)),
  anxiety = c(
    rnorm(20, mean = 55, sd = 5),
    rnorm(20, mean = 20, sd = 5),
    rnorm(20, mean = 75, sd = 5)
  )
)

anova_model <- aov(anxiety ~ group, data = anova_data)

print(p_anxiety)
print(paired_test)
print(independent_test)
print(cohens_d)
print(summary(anova_model))
