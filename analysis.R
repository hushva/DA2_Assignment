# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)

# Call the data from github
df <- read_csv( "https://raw.githubusercontent.com/hushva/DA2_Assignment/master/data/clean/covid_pop_02_11_2020_clean.csv")


# check all variables via histogram ##########

summary(df$country)
summary(df$confirmed)
summary(df$death)
summary(df$recovered)
summary(df$active)
summary(df$population)

#Scaling for the population variable by 1000
df <- df %>% mutate(df,population = population/1000)
# remove rows where death is zero since the ln of 0 is not defined
df <- filter(df,death != 0,)


#(c) - SUmmary stats and and histograms #########
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

summary( df )

# Check via kernel density and histogram
ggplot( data = df , aes( x = death ) ) +
  geom_density( aes( y = ..density.. ), color = "red" , fill = "blue", bw = 15 , alpha = 0.5 ) +
  labs(x = "Confirmed cases",y = "Deaths")


# (f)
ggplot(data = df, aes(x = confirmed))+
  geom_histogram(binwidth = 70000)

ggplot(data = df, aes(x = death))+
  geom_histogram(binwidth = 7000)

summary(df$confirmed)
summary(df$death)



#(g) - possible log transformations ##################
#Where to use log-transformation? - level-level vs level-log vs log-level vs log-log

# Model: death = alpha + beta * confirmed

# death - confirmed level-level model without scaling
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases",y = "Number of deaths")

# death - confirmed level - log model
ggplot( df , aes(x = confirmed, y = death)) +
geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases, ln scale",y = "Number of deaths") +
  scale_x_continuous( trans = log_trans(),  breaks = c(10,50,100,200,500,1000,5000,10000))

# death - confirmed log - level model
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases",y = "Number of deaths, ln scale") +
  scale_y_continuous( trans = log_trans() )


# death - confirmed log - log model
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases, ln scale",y = "Number of deaths, ln scale") +
  scale_x_continuous( trans = log_trans(), breaks = c(10,50,100,200,500,1000,5000,10000)) +
  scale_y_continuous( trans = log_trans() )


####
# Conclusions:
#   1) level-level somewhat linear but points are very dense within the square area of 
#       50000 number of deaths and ~ 200thousand number of confirmed cases - not informative
#   2) using only gdppc is possible, but need to model the non-linearity in data
#       - Substantive: Level changes is harder to interpret and our aim is not to get $ based comparison
#       - Statistical: log transformation is way better approximation make simplification!
#   3) taking log of gdppc is making the association close to linear!
#   4) taking log for life-expectancy does not matter -> use levels!
#       - Substantive: it does not give better interpretation
#       - Statistical: you can compare models with the same y, no better fit
#       - Remember: simplest the better!




#(h) - estimate models #################

# Take both logs
df <- df %>% mutate( ln_death = log( death ),
                     ln_confirmed = log( confirmed ) )

# Make some models:

#     reg1: linear:       ln_death = alpha + beta * ln_confirmed
#     reg2: exponential:  ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2
#     reg3: cubic:        ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2 + beta_3 * ln_confirmed^3
# #   reg4: weighted-ols: ln_death = alpha + beta * ln_gdppc, weights: population

df <- df %>% mutate( ln_confirmed_sq = ln_confirmed^2,
                     ln_confirmed_cb = ln_confirmed^3)

# reg1:
reg1 <- lm_robust( ln_death ~ ln_confirmed , data = df , se_type = "HC2" )
reg1
# Summary statistics
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed, y = ln_death) ) + 
  geom_point( color='green') +
  geom_smooth( method = lm , color = 'red' )

# reg2:
reg2 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='green') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# reg3:
reg3 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq + ln_confirmed_cb , data = df )
summary( reg3 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death) ) + 
  geom_point( color='green') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# reg4:
# Regression with piecewise linear spline:
# 1st define the cutoff for number of confirmed cases
cutoff <- 5000
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln<- log( cutoff )
# Use simple regression with the lspline function
?lspline
reg4 <- lm_robust(ln_death ~ lspline( ln_confirmed , cutoff_ln ), data = df )
summary( reg4 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )


# Weighted-OLS: use reg4 setup and weight with population
reg5 <- lm_robust(ln_death ~ ln_confirmed, data = df , weights = population)
summary( reg5 )

ggplot(data = df, aes(x = ln_confirmed, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = 'purple', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red') +
  #scale_size(range = c(1, 15)) +
  #coord_cartesian(ylim = c(50, 85)) +
  labs(x = "ln(Number of confirmed cases) ",y = "ln(Number of years") 
# +
  #annotate("text", x = 4, y = 80, label = "USA", size=5) # +
  #annotate("text", x = 2.7, y = 79, label = "China", size=5)+
  #annotate("text", x = 2,  y = 68, label = "India", size=5)



#(i) -  Creating model summary ################################
# with texreg

data_out <- "Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_8/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5 , reg6 , reg7),
         type = 'html',
         custom.model.names = c("GDP total - linear",
                                "GDP total - quadratic",
                                "GDP total - cubic",
                                "GDP/capita - linear",
                                "GDP/capita - quadratic",
                                "GDP/capita - PLS",
                                "GDP/capita - weighted linear"),
         caption = "Modelling life expectancy and different wealth measures of countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)







