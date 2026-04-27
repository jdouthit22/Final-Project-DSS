library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

BC <- read.csv("/Volumes/T9/RSTUDIO WORK/Directed Seminar/breast-cancer.csv")
#Each row represents one patient’s biopsy

#Topic question: Which measurement is the most telltale sign of a malignant tumor?
colnames(BC)
nrow(BC)

diagnosis_table = table(BC$diagnosis)
print(diagnosis_table)

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







