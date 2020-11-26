# Clear memory and call packages
rm(list=ls())
library(tidyverse)

# Read the raw files
my_path <- "~/Documents/CEU/Courses/2020_Fall/Mandatory/DA2/DA2_Assignment/data/"

# covid data
cv <- read_csv(paste0(my_path,'raw/covid_09_11_2020_raw.csv'))

# population data
pop <- read_csv(paste0(my_path,'raw/pop_WDI_2019.csv'))
