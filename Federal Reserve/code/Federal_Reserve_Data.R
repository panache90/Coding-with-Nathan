

install.packages("maps")
install.packages("mapproj")
install.packages("sf")
install.packages("devtools")
install.packages("fredr")

library(tidyverse)
library(maps)
library(mapproj)
library(ggplot2)
library(glue)
library(devtools)
library(lubridate)
library(fredr)
library(scales)




#devtools::install_github("jcizel/FredR")


fredr_api_key <- "98868097bf8e12d9ad9f462ed9dc9aa6"
fredr_set_key(fredr_api_key)

#========================================
#CPI Change
cpi_urban <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-01-01"),
  frequency = "m"
)

cpi_urban2 <- cpi_urban %>% mutate(value_pct = (value -lag(value, n=12))/lag(value, n =12), 
                                   chg_value =(value -lag(value, n=12)))

#graph -- need to install scales package for breaks to work
ggplot(cpi_urban2, aes(date, value_pct)) +
  geom_line(size = .3, color ="firebrick") +
  scale_x_date(date_breaks = "2 year",date_labels = "%y %m", limits = c(as.Date("2000-01-01"),NA)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title ="CPI Urban")

#========================================

#Unemployment Rate Change
unrate <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-01-01"),
  frequency = "m")

unrate2 <- unrate %>% mutate(unrate_pct = (value/100))


#graph -- need to install scales package for breaks to work
ggplot(unrate2, aes(date, unrate_pct)) +
  geom_line(size = .3, color ="firebrick") +
  scale_x_date(date_breaks = "2 year",date_labels = "%y %m", limits = c(as.Date("2000-01-01"),NA)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title ="Unemployment Rate")

#========================================


#========================================

#10yr treasury minus 2yr
T10m2 <- fredr(
  series_id = "T10Y2Y",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-03-01"),
  frequency = "m")

#T10m2_2 <- T10m2 %>% mutate(unrate_pct = (value/100))


#graph -- need to install scales package for breaks to work
ggplot(T10m2, aes(date, value)) +
  geom_line(size = .3, color ="firebrick") +
  scale_x_date(date_breaks = "2 year",date_labels = "%y %m", limits = c(as.Date("2000-01-01"),NA)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(title ="10yr Treasury minus 2yr Treasury")

#========================================

#========================================

#10yr treasury minus 2yr
M2 <- fredr(
  series_id = "WM2NS",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-03-01"),
  frequency = "m")

#T10m2_2 <- T10m2 %>% mutate(unrate_pct = (value/100))


#graph -- need to install scales package for breaks to work
ggplot(M2, aes(date, value)) +
  geom_line(size = .3, color ="firebrick") +
  scale_x_date(date_breaks = "2 year",date_labels = "%y %m", limits = c(as.Date("2000-01-01"),NA)) +
  scale_y_continuous("$ Billions", labels = scales::label_dollar()) +
  theme_bw() +
  labs(title ="M2 Money Supply")

#========================================

#========================================

#Average Hourly Wages
HrlyWages <- fredr(
  series_id = "CES0500000003",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-03-31"),
  frequency = "m")

HarlyWages2 <-HrlyWages %>% mutate(value_chg = percent((value/value[1])-1))


#graph -- need to install scales package for breaks to work
ggplot(M2, aes(date, value)) +
  geom_line(size = .3, color ="firebrick") +
  scale_x_date(date_breaks = "2 year",date_labels = "%y %m", limits = c(as.Date("2000-01-01"),NA)) +
  scale_y_continuous("$ Billions", labels = scales::label_dollar()) +
  theme_bw() +
  labs(title ="M2 Money Supply")

#========================================


