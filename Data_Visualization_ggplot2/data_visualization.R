# Data Visualization with ggplot2 --------------------------------------

library(ggplot2)

data("mpg", package = "ggplot2")

# Highway fuel economy by manufacturer.
p_manufacturer <- ggplot(mpg, aes(x = manufacturer, y = hwy)) +
  geom_point() +
  labs(x = "Manufacturer", y = "Highway MPG") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribution of city fuel economy by vehicle class.
p_density <- ggplot(mpg, aes(x = cty, colour = class)) +
  geom_density() +
  labs(x = "City MPG", y = "Density", colour = "Vehicle class") +
  theme_minimal(base_size = 12)

# Highway fuel economy by vehicle class.
p_boxplot <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  labs(x = "Vehicle class", y = "Highway MPG") +
  theme_minimal(base_size = 12)

p_violin <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_violin() +
  labs(x = "Vehicle class", y = "Highway MPG") +
  theme_minimal(base_size = 12)

# Two-dimensional binning of fuel type and city fuel economy.
p_fuel <- ggplot(mpg, aes(x = fl, y = cty)) +
  geom_bin_2d() +
  labs(x = "Fuel type", y = "City MPG") +
  theme_minimal(base_size = 12)

print(p_manufacturer)
print(p_density)
print(p_boxplot)
print(p_violin)
print(p_fuel)
