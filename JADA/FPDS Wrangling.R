library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

BC <- read.csv("/Volumes/T9/RSTUDIO WORK/Directed Seminar/breast-cancer.csv")
#Each row represents one patient’s biopsy
ncol(BC)
nrow(BC)
colnames(BC)
#Topic question: Which measurement is the most telltale sign of a malignant tumor?

diagnosis_table = table(BC$diagnosis)
print(diagnosis_table)

table(BC$cyl_factor)
#Check NAs in dataset
sum(is.na(BC))

#Percent of M vs B
d_data <- BC %>%
  count(diagnosis) %>%
  mutate(
    percent = round((n / sum(n)) * 100,2)
  )


#Pie Chart: Diagnosis Distribution
ggplot(d_data, aes(x = "", y = percent, fill = diagnosis)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to pie chart
  geom_text(aes(label = paste0(percent, "%")), 
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "Diagnosis Distribution") +
  theme_void() 


#ECDF Graph

BC |> 
  filter(diagnosis %in% c("M", "B")) |>
  ggplot(aes(x = concave.points_worst, color = diagnosis)) +
  stat_ecdf(linewidth = 1) +
  theme(legend.position = "bottom")

# An ECDF (Empirical Cumulative Distribution Function) graph shows:
#   “How quickly values build up in a group”
# So instead of looking at averages, you are looking at:
#   how many values are small
# how many values are large
# Benign (B)
# ECDF rises early (left side) - Meaning: most benign tumors have low concave point values
# Malignant (M)
# ECDF is shifted right - Meaning: malignant tumors have higher concave point values

# Interpretation:
# The ECDF plot shows that malignant tumors are shifted to the right compared to 
# benign tumors, indicating that malignant tumors tend to have higher values of concave points. 
# This suggests that tumor boundary irregularity is strongly associated with malignancy.
# library(ggplot2)

# Concave worst means you have:
# mean (average)
# se (variation)
# worst (most extreme case in the sample)
# concave.points_worst captures the most abnormal cells in the tumor

#----------------------------------------------------------------------------------------------------------------------
MT <- BC |>
  filter(diagnosis == "M")


BT <- BC |>
  filter(diagnosis == "B")

#Boxplot
boxplot(
  concavity_worst ~ diagnosis,
  data   = BC,
  main   = "Concavity (Worst) by Diagnosis",
  xlab   = "Diagnosis (B = Benign, M = Malignant)",
  ylab   = "Concavity Worst",
  col    = c("lightblue", "lightpink"),
  border = "gray40"
)

#Stacked histogram: Radius_worst vs B & M
BC |> 
  filter(diagnosis %in% c("M", "B")) |>
  ggplot(aes(x = radius_worst, fill = diagnosis)) +
  geom_histogram(bins = 15)

#Linear Regression
BC |> 
  ggplot(aes(x = radius_mean, y = concavity_mean, color = diagnosis)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2) +
  ggthemes::scale_color_colorblind()