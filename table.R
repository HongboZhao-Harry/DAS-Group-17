library(tidyverse)
library(gt)

animal <- read.csv("dataset17.csv", header = TRUE)
head(animal)
animal <- animal%>%
  mutate(animal_type=as.factor(animal_type),
         intake_type=as.factor(intake_type),
         outcome_type=as.factor(outcome_type),
         chip_status=factor(chip_status,levels=c("UNABLE TO SCAN","SCAN CHIP","SCAN NO CHIP")),
         month =as.factor(month),
         year=as.factor(year))

#table_animal_type
animal |> 
  summarize('Mean' = mean(time_at_shelter),
            'Median' = median(time_at_shelter),
            'St.Dev' = sd(time_at_shelter),
            'Min' = min(time_at_shelter),
            'Max' = max(time_at_shelter),
            'IQR' = quantile(time_at_shelter,0.75)-quantile(time_at_shelter,0.25),
            'Sample_size' = n(),
            .by = animal_type) |>
  gt() |>
  fmt_number(decimals=2) |>
  cols_label(
    Mean = html("Mean"),
    Median = html("Median"),
    St.Dev = html("Std. Dev"),
    Min = html("Minimum"),
    Max = html("Maximum"),
    IQR = html("Interquartile Range"),
    Sample_size = html("Sample Size")
  )

#table_year
animal |> 
summarize('Mean' = mean(time_at_shelter),
          'Median' = median(time_at_shelter),
          'St.Dev' = sd(time_at_shelter),
          'Min' = min(time_at_shelter),
          'Max' = max(time_at_shelter),
          'IQR' = quantile(time_at_shelter,0.75)-quantile(time_at_shelter,0.25),
          'Sample_size' = n(),
          .by = year) |>
  gt() |>
  fmt_number(decimals=2) |>
  cols_label(
    Mean = html("Mean"),
    Median = html("Median"),
    St.Dev = html("Std. Dev"),
    Min = html("Minimum"),
    Max = html("Maximum"),
    IQR = html("Interquartile Range"),
    Sample_size = html("Sample Size")
  )

#table_month
animal |> 
  summarize('Mean' = mean(time_at_shelter),
            'Median' = median(time_at_shelter),
            'St.Dev' = sd(time_at_shelter),
            'Min' = min(time_at_shelter),
            'Max' = max(time_at_shelter),
            'IQR' = quantile(time_at_shelter,0.75)-quantile(time_at_shelter,0.25),
            'Sample_size' = n(),
            .by = month) |>
  gt() |>
  fmt_number(decimals=2) |>
  cols_label(
    Mean = html("Mean"),
    Median = html("Median"),
    St.Dev = html("Std. Dev"),
    Min = html("Minimum"),
    Max = html("Maximum"),
    IQR = html("Interquartile Range"),
    Sample_size = html("Sample Size")
  )

#table_intake_type
animal |> 
  summarize('Mean' = mean(time_at_shelter),
            'Median' = median(time_at_shelter),
            'St.Dev' = sd(time_at_shelter),
            'Min' = min(time_at_shelter),
            'Max' = max(time_at_shelter),
            'IQR' = quantile(time_at_shelter,0.75)-quantile(time_at_shelter,0.25),
            'Sample_size' = n(),
            .by = intake_type) |>
  gt() |>
  fmt_number(decimals=2) |>
  cols_label(
    Mean = html("Mean"),
    Median = html("Median"),
    St.Dev = html("Std. Dev"),
    Min = html("Minimum"),
    Max = html("Maximum"),
    IQR = html("Interquartile Range"),
    Sample_size = html("Sample Size")
  )

#table_outcome_type
animal |> 
  summarize('Mean' = mean(time_at_shelter),
            'Median' = median(time_at_shelter),
            'St.Dev' = sd(time_at_shelter),
            'Min' = min(time_at_shelter),
            'Max' = max(time_at_shelter),
            'IQR' = quantile(time_at_shelter,0.75)-quantile(time_at_shelter,0.25),
            'Sample_size' = n(),
            .by = outcome_type) |>
  gt() |>
  fmt_number(decimals=2) |>
  cols_label(
    Mean = html("Mean"),
    Median = html("Median"),
    St.Dev = html("Std. Dev"),
    Min = html("Minimum"),
    Max = html("Maximum"),
    IQR = html("Interquartile Range"),
    Sample_size = html("Sample Size")
  )

#table_chip_status
animal |> 
  summarize('Mean' = mean(time_at_shelter),
            'Median' = median(time_at_shelter),
            'St.Dev' = sd(time_at_shelter),
            'Min' = min(time_at_shelter),
            'Max' = max(time_at_shelter),
            'IQR' = quantile(time_at_shelter,0.75)-quantile(time_at_shelter,0.25),
            'Sample_size' = n(),
            .by = chip_status) |>
  gt() |>
  fmt_number(decimals=2) |>
  cols_label(
    Mean = html("Mean"),
    Median = html("Median"),
    St.Dev = html("Std. Dev"),
    Min = html("Minimum"),
    Max = html("Maximum"),
    IQR = html("Interquartile Range"),
    Sample_size = html("Sample Size")
  )
