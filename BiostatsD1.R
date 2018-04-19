#Day 1
#Dhiren vanmari
#bioststats
#12 april 2018

# load libraries ----------------------------------------------------------
library(tidyverse)


# intergers ---------------------------------------------------------------

#hgen a  seq of intergers data

interger_r <- as.integer(seq(5, 14, by = 1))

summary(interger_r) #summary data of intergers


# continuous  -------------------------------------------------------------

#gen a seq of continuous data

numeric_r <- seq(23, 55, length.out = 10)#create 10 values of a sequence from 23 to 55 

summary(numeric_r) #summary stats off continuos data


# Dates -------------------------------------------------------------------

#performing arithmetics with dates 
as.Date("2005-12-31") - as.Date("2005-12-12") #number of days from 12-31 /12/2005
#or
dates_R <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day") 
#create a sequence of dates 

dates_R

summary(dates_R)


# Data frames -------------------------------------------------------------

#creating data frames
#variables wich share similar properties.

df_r <- data.frame(intergers = interger_r,
                   numeric = numeric_r,
                   dates = dates_R)

#tibble data

df_r <- as.tibble(df_r)
summary(df_r)



# categorical data --------------------------------------------------------------
#qualitative data
#creating vectors

#electronics
elect_r <- as.factor(c("laptops", "desktops", "cell phones"))

#people
people_r <- (c("ugly", "sexy","beutiful"))

#colours
colours_r <- (c("red","blue", "green"))



# ordinal data ------------------------------------------------------------

#this is still qualitative but with some sougt of order
#ordinal scale  or importance of a vector according to rank

colour_qual <- ordered(c("blue", "yellow","green", "orange", "red"),
                       levels = c("blue", "green", "yellow", "orange", "red")) #creating the rank of importance




# binary data -------------------------------------------------------------
#takes on one of two answers eg ttrue/false

binary_R <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_R)


# character data ----------------------------------------------------------
#words, namees


sites_r <- c("Gansbaai", "Sea Point", "koggelbay")



# missing values ----------------------------------------------------------
#numbers of chick eggs in nest
Chicks_nest <- c(3, 0, 2, 10, 4, 6, 0, NA)
summary(Chicks_nest) #summary
mean(Chicks_nest) #mean





# viewing Data ----------------------------------------------------------------

#head(name)
#tail(name,n=)
#view from environment table 


# chapter3 DESCRIPTIVE STATS ----------------------------------------------------------------

#create a data frame

chicks <- as.tibble(ChickWeight)

#count the data
chicks %>% 
  summarise(n())
#or
nrow(chicks)
# or 
#look in environment


# measures of central tendency (means) --------------------------------------------

#calc mean weight
chicks %>% 
  summarise(mean.wt = mean(weight))

#be more specific
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean.wt = mean(weight),median.wt = median(weight))

#visualising density of data

ggplot(data = filter(chicks, Time == 21), aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) 

#skewness

library(e1071)


chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean.wt = mean(weight),
            median.wt = median(weight),
            skew.wt = skewness(weight))


#kurtosis
#calc the kurtosis of tail distribution

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean.wt = mean(weight),
            median.wt = median(weight),
            skew.wt = skewness(weight))
            




# measures of variability----------------------------------------------------------------

below is a summary of many different statistical properties

wt.summary <- chicks %>% 
  filter(Time ==21) %>% 
  group_by(Diet) %>% 
  summarise(wt.mean = mean(weight),
            wt.median = median(weight),
            wt.var = var(weight),
            wt.sd = sd(weight),
            wt.min = min(weight),
            wt.quart1 = quantile(weight, 0.25),
            wt.quart2 = quantile(weight, 0.5),
            wt.quart3 = quantile(weight, 0.75))


  











