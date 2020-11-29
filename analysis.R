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
# df <- df %>% mutate(df,population = population/1000)
# remove rows where death is zero since the ln of 0 is not defined
df <- filter(df,death != 0,)


#(c) - SUmmary stats and histograms #########
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

library(xtable)
conf_summary <- df %>%  summarise(
  mean = mean( confirmed ),
  median = median( confirmed ),
  sd = sd( confirmed  ),
  min = min( confirmed ),
  max = max( confirmed ))

death_summary <- df %>%  summarise(
  mean = mean( death ),
  median = median( death ),
  sd = sd( death ),
  min = min( death ),
  max = max( death ))

xt_conf <- xtable(conf_summary,caption = "Conf_stat")
name(xt_conf) <- c('Mean','Median', 'Std.dev.', 'Min','Max')
print(xt_conf, type = "latex", comment = getOption("xtable.comment", FALSE))

xt_death <- xtable(death_summary,caption = "Conf_stat")
name(xt_death) <- c('Mean','Median', 'Std.dev.','Min','Max')
print(xt_death, type = "latex", comment = getOption("xtable.comment", FALSE))

sstat_table <- xt_conf %>% add_row( xt_death)
sstat_table


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
  labs(x = "ln(Number of confirmed cases) ",y = "ln(Number of death)") 
# +
  #annotate("text", x = 4, y = 80, label = "USA", size=5) # +
  #annotate("text", x = 2.7, y = 79, label = "China", size=5)+
  #annotate("text", x = 2,  y = 68, label = "India", size=5)



#(i) -  Creating model summary ################################
# with texreg

data_out <- "~/Documents/CEU/Courses/2020_Fall/Mandatory/DA2/DA2_Assignment/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5),
         type = 'html',
         custom.model.names = c("confirmed - linear",
                                "confirmed - quadratic",
                                "confirmed - cubic",
                                "confirmed - PLS",
                                "confirmed - weighted linear"),
         caption = "Modelling number of deaths  and reported confirmed cases in different countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

######
# Based on model comparison our chosen model is reg5 - weighted linear
#   Substantive: - level-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well


# (k) - Residual analysis #######################

# lm_robust output is an `object` or `list` with different elements
# Check the `Value` section
?lm_robust

# Get the predicted y values from the model
df$reg5_y_pred <- reg5$fitted.values
# Calculate the errors of the model
df$reg5_res <- df$ln_death - df$reg5_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg5_res ) %>% 
  select( country , ln_death , reg5_y_pred , reg5_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg5_res ) %>% 
  select( country , ln_death , reg5_y_pred , reg5_res )


# (j) - Testing hypothesis #######################

# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg5 )

# 2) Coefficient is equal to your favorite value
library(car)
# Let test: H0: ln_confirmed = 5, HA: ln_gdppc neq 5
linearHypothesis( reg5 , "ln_gdppc = 5")


## Prediction uncertainty - #####################

# CI of predicted value/regression line is implemented in ggplot
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' , se = T )

##
# You can get them by predict function
#   interval can be any of c("none", "confidence", "prediction")
#   alpha = 0.05 (default) is the significance level
###
# CI of regression line
pred5_CI <- predict( reg5, newdata = df , interval ="confidence" , alpha = 0.05 )
## 95% CI is going to be used if alpha = 0.05
pred5_CI

# SEs for each point:
# pred5_CI <- predict( reg5, newdata = df , se.fit=T,
#                interval ="confidence" , alpha = 0.05 )
#pred5_CI
 
 
# Hand made CI for regression line
# 1) Add to datatset:
df <- df %>% mutate( CI_reg5_lower = pred5_CI$fit[,2],
                     CI_reg5_upper = pred5_CI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = ln_confirmed, y = ln_death ) , color='blue') +
  geom_line( data = df, aes( x = ln_confirmed, y = reg5_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = ln_confirmed, y = CI_reg5_lower ) , color = 'green' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = df, aes( x = ln_confirmed, y = CI_reg5_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "ln(Number of confirmed cases) ",y = "ln(Number of death)") 


## prediction intervals ########################

pred5_PI <- predict( reg5, newdata = df , interval ="prediction" , alpha = 0.05 )

# Hand made Prediction Interval for regression line
# 1) Add to datatset (You can use the SE's as well if you wish...
#                        then alpha does not have any meaning)
df <- df %>% mutate( PI_reg5_lower = pred5_PI$fit[,2],
                     PI_reg5_upper = pred5_PI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = ln_confirmed, y = ln_death ) , color='blue') +
  geom_line( data = df, aes( x = ln_confirmed, y = reg5_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = ln_confirmed, y = PI_reg5_lower ) , color = 'green' ,
             size = 1 , linetype = "dotted" ) +
  geom_line( data = df, aes( x = ln_confirmed, y = PI_reg5_upper ) , color = 'black' ,
             size = 1 , linetype = "dotted" ) +
  labs(x = "ln(Number of confirmed cases) ",y = "ln(Number of death)") 




