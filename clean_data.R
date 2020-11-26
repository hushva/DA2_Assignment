# Clear memory and call packages
rm(list=ls())
library(tidyverse)

# Read the raw files
my_path <- "~/Documents/CEU/Courses/2020_Fall/Mandatory/DA2/DA2_Assignment/data/"

# covid data
cv <- read_csv(paste0(my_path,'raw/covid_09_11_2020_raw.csv'))

# population data
pop <- read_csv(paste0(my_path,'raw/pop_WDI_2019.csv'))


#####################
# COVID DATA CLEANING
#
# Check covid data
glimpse( cv)

# Drop not needed variables
cv <- cv %>% select( -c( FIPS,Admin2,Last_Update,Lat,Long_,Combined_Key,Incidence_Rate,Case.Fatality_Ratio))

# One observation to be one country
# Check e.g. China:
cv %>% filter( Country_Region == 'China')

# Create new data table now only contains the countries
cv2 <- cv %>% 
  group_by( Country_Region ) %>% 
  summarise_if(is.numeric,lst( sum ) )

# Rename variables
cv2 <- cv2 %>% rename( country   = Country_Region ,
                       confirmed = Confirmed_sum,
                       death     = Deaths_sum,
                       recovered = Recovered_sum,
                       active    = Active_sum )

glimpse( cv2 )
