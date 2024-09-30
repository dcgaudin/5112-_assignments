library(tidybits)
library(haven)
library(dplyr)
library(ggplot2)

new_df <-  IAHR52FL %>%
  select(hhid:shstruc) %>%
  mutate(rural = hv025 == 2)

IAHR52FL <- IAHR52FL %>%
  mutate(urban_area = factor(hv026,
    levels = c(0, 1, 2, 3, 4, 5),
    labels = c("Mega City", "Large City", "Small City", "Large Town", "Small Town", "Rural")))
  

ggplot(data = IAHR52FL, mapping = aes(x = hv009, fill = urban_area)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  xlab("Number of Household Members") +
  ylab("Count") +
  labs(title = "Distribution of Household Members by Urban Area") +
  theme_minimal()

ggplot(data = IAHR52FL, mapping = aes(x = urban_area, y = hv009)) +
  geom_boxplot() +
  xlab("Urban Area") +
  ylab("Number of Household Members") +
  labs(title = "Household Size by Urban Area") +
  coord_flip()

ggplot(data = IAHR52FL, mapping = aes(x = urban_area, y = hv009)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3) +
  xlab("Urban Area") +
  ylab("Number of Household Members") +
  labs(title = "Household Size by Urban Area with Mean") +
  coord_flip()

new_df %>%
  group_by(rural) %>%
  count = n()

summary_stats <- IAHR52FL %>%
  group_by(urban_area) %>%
  summarise(
    mean_hh_size = mean(hv009, na.rm = TRUE),
    median_hh_size = median(hv009, na.rm = TRUE),
    sample_size = n()
  )  

print(summary_stats)