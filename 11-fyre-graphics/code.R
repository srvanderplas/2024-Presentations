if (!"palmerpenguins" %in% installed.packages()) {
  install.packages("palmerpenguins")
}

if (!"ggplot2" %in% installed.packages()) {
  install.packages("ggplot2")
}

library(palmerpenguins)
library(ggplot2)

data(penguins)

# Differences between males and females in each species
penguin_fix <- na.omit(penguins)
ggplot(penguin_fix, aes(x = bill_length_mm, y = bill_depth_mm, color = species, 
                        shape = sex, linetype = sex)) + 
  geom_point(size = 5) + 
  scale_shape_manual(values = c("male" = "M", "female" = "F"), na.value = "?") + 
  stat_ellipse()

ggplot(penguin_fix, aes(x = bill_length_mm, y = bill_depth_mm, color = species, 
                        shape = sex, linetype = sex)) + 
  geom_point(size = 5) + 
  scale_shape_manual(values = c("male" = "M", "female" = "F"), na.value = "?") + 
  stat_smooth(method = "lm")



ggplot(penguin_fix, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_point(size = 5) + 
  facet_grid(~sex)

ggplot(penguin_fix, aes(x = bill_length_mm, fill = species)) + geom_histogram(position = "fill")
