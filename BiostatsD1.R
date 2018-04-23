#Day 1
#Dhiren vanmari
#bioststats
#12 april 2018

# load libraries ----------------------------------------------------------
library(tidyverse)


# intergers ---------------------------------------------------------------

#gen a  seq of intergers data

interger_r <- as.integer(seq(5, 14, by = 1))
head(interger_r)


summary(interger_r) #summary data of intergers


# continuous  -------------------------------------------------------------

#gen a seq of continuous data

numeric_r <- seq(23, 55, length.out = 10)#create 10 values of a sequence from 23 to 55 
head(numeric_r, n = 10) #view the 10 numbers
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
#variables which share similar properties.

df_r <- data.frame(intergers = interger_r,
                   numeric = numeric_r,
                   dates = dates_R)

#tibble data to tabulate 

df_r <- as.tibble(df_r)
summary(df_r) #summarise each variable of the table



# categorical data --------------------------------------------------------------
#has no rankings or order
#qualitative data that are classed
#creating vectors

#electronics
elect_r <- as.factor(c("laptops", "desktops", "cell phones"))

#people
people_r <- (c("ugly", "sexy","beutiful"))

#colours
colours_r <- (c("red","blue", "green"))


students <- data.frame(colours_r,people_r,elect_r) #create a datframe of the categorical data we generated
 summary(students)

# ordinal data ------------------------------------------------------------
# are data that are classed according to ranks 
#eg. position 1st 2nd 3rd
#this is still qualitative but with some sougt of order
#ordinal scale  or importance of a vector according to rank

colour_qual <- ordered(c("blue", "yellow","green", "orange", "red"),
                       levels = c("blue", "green", "yellow", "orange", "red")) #creating the rank of importance

colour_qual


# binary data -------------------------------------------------------------
#takes on one of two answers eg true/false

binary_R <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_R) # best described by mode


# character data ----------------------------------------------------------
#words, names, species, companies


sites_r <- c("Gansbaai", "Sea Point", "koggelbay")



# missing values ----------------------------------------------------------
#numbers of chick eggs in nest
Chicks_nest <- c(3, 0, 2, 10, 4, 6, 0, NA)
summary(Chicks_nest) #summary
mean(Chicks_nest, na.rm = T) #mean


# viewing Data ----------------------------------------------------------------

#head(name)
#tail(name,n= )
#view from environment ----Table 


# Chapter3 DESCRIPTIVE STATS ----------------------------------------------------------------

#create a data frame

chicks <- as.tibble(ChickWeight)

#count the data
chicks %>% 
  summarise(n())
#or
nrow(chicks)
# or 
#look in environment .obs


# measures of central tendency --------------------------------------------

#means
#medians 
#Skewness


#calc mean weight
chicks %>% 
  summarise(mean.wt = mean(weight))

#be more specific and look at a specific time 
#create a new data set with variables to be considered
 chicks %>% 
  filter(Time == 21) %>% #select all time 21 
  group_by(Diet) %>%  #group data by Diets
  summarise(mean.wt = mean(weight),median.wt = median(weight))

 
#visualising density of data

ggplot(data = filter(chicks, Time == 21), aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) 
#no y variable because its a density graph which is similiar to a hiostogram

#skewness
#when graph is higher on the left = skewed to the right (+ skewed)
#when graph is higher on the right = Skewed to the left (- skewed)



library(e1071) #load this to measure skewness

#creating data for mean, median and skewness in weight for every type of Diet give to chickens
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean.wt = mean(weight),
            median.wt = median(weight),
            skew.wt = skewness(weight))
#skewness is the degree of assymetry present in the distribution when the mode, mean and median are not equal.


#kurtosis
#Kurtosis describes the tail shape of the data’s distribution. 
#A normal distribution has zero kurtosis and thus the standard tail shape (mesokurtic).
#Negative kurtosis indicates data with a thin-tailed (platykurtic) distribution.
#Positive kurtosis indicates a fat-tailed distribution (leptokurtic)

#calc the kurtosis of tail distribution

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean.wt = mean(weight),
            median.wt = median(weight),
            skew.wt = skewness(weight),
            kurt.wt = kurtosis(weight))
            




# measures of variability----------------------------------------------------------------

#below is a summary of many different statistical properties

wt.summary <- chicks %>% 
  filter(Time ==21) %>% 
  group_by(Diet) %>% 
  summarise(wt.mean = mean(weight),
            wt.median = median(weight),
            wt.var = var(weight),
            wt.sd = sd(weight),
            wt.min = min(weight),
            wt.quart1 = quantile(weight, 0.25), # lower quartile
            wt.quart2 = quantile(weight, 0.5), #mean
            wt.quart3 = quantile(weight, 0.75)) #upper quartile

#The variance and standard deviation are examples of interval estimates.
  











