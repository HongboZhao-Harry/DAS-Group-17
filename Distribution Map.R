library(tidyverse)
library(ggplot2)
library(dplyr)


animal <- read.csv("dataset17.csv", header = TRUE)
head(animal)
animal <- animal%>%
  mutate(animal_type=as.factor(animal_type),
         intake_type=as.factor(intake_type),
         outcome_type=as.factor(outcome_type),
         chip_status=factor(chip_status,levels=c("UNABLE TO SCAN","SCAN CHIP","SCAN NO CHIP")),
         month =as.factor(month),
         year=as.factor(year))

#Histogram
ggplot(animal,aes(x=time_at_shelter))+
  geom_histogram()

#scatterplot:Time_at_Shelter vs month
animal <- animal %>%
  mutate(month = as.numeric(month),
         month_ordered = ifelse(month >= 10, month - 9, month + 3))

ggplot(animal, aes(x = month_ordered, y = time_at_shelter)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", formula = y ~ x, color = "red", se = FALSE) +  # 添加平滑曲线
  labs(
    x = "month_ordered",
    y = "time_at_shelter",
    title = "Time_at_Shelter vs. Month "
  ) +
  theme_minimal()

#boxplot
ggplot(animal, aes(x = animal_type, y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "red") +  # 箱线图
  labs(x = "Animal Type", y = "Time at Shelter") +
  theme_minimal()



ggplot(animal, aes(x = intake_type, y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "red") +  # 箱线图
  labs(x = "intake_type", y = "Time at Shelter") +
  theme_minimal()

ggplot(animal, aes(x = outcome_type, y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "red") +  # 箱线图
  labs(x = "intake_type", y = "Time at Shelter") +
  theme_minimal()

ggplot(animal, aes(x = chip_status, y = time_at_shelter)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "red") +  # 箱线图
  labs(x = "intake_type", y = "Time at Shelter") +
  theme_minimal()
