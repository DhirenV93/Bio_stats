#BiostatsD3
#linear regressions, and correlations

library(tidyverse)
library(ggpubr)
# library(Rmisc) # Unfortunately this overrides many dplyr functions

# load data ---------------------------------------------------------------

snakes <- read.csv("snakes.csv")

  snakes$day = as.factor(snakes$day) #change interger day into factor day


# summarise data ----------------------------------------------------------

snakes.summary<- snakes %>% 
  group_by(day) %>% 
  summarise(snakes.mn = mean(openings),
            snakes.sd = sd(openings))


# formulate an hypothesis -------------------------------------------------
#H0: there is no difference in the number of openings from day to day
#H1: there is a difference in the number of openings from day to day



# test hypothesis ---------------------------------------------------------

#Rmisc pckge summarySE to calc the SE  and CI

snakes.summary2 <- Rmisc::summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

# visualise data ----------------------------------------------------------



ggplot(data = snakes, aes(x = day, y = openings)) +
    geom_segment(data = snakes.summary2, 
                 aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
                 size = 2.0, linetype = "solid", show.legend = F) +
    geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
    geom_jitter(width = 0.05)

#we have two factors, so we need 2 null hpotesis
    
#H0: There is no difference between snakes with respect to the number of 
  #openings at which they habituate.
#H0: There is no difference between days in terms of the number of 
  #openings at which the snakes habituate.


# run anova ---------------------------------------------------------------
  
  #test the days hypothesis only
snakes.day.aov <- aov(openings ~ day, data = snakes) # aov of openings
  summary(snakes.day.aov)
  #there is a significant difference between days in terms of the number of openings

  #test both hypothesis at once 
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)
# there is no signinificant difference........for the 1st HO


# testing assumptions afterwards ------------------------------------------

#first check normality of data
snakes.res <- residuals(snakes.all.aov)
hist(snakes.res)

#then visulaise homoscedasticity of results

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

#tukey analysis
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)


ggplot(data = snakes, aes(x = as.numeric(day), y = openings, colour = snake))+
         geom_line(size = 3)+
         geom_point(size = 4)
# exercise ----------------------------------------------------------------
 #find data from github


moths <-read.csv("moths_traps.csv") %>% 
  gather(key = "trap", value = "count", -Location)

#H0: There's no difference in the number of moths counted w.r.t. 
#the different trap types at each location.
#H1: There is a difference

moths.summary <- moths %>% 
  group_by(Location) %>% 
  summarise(moths.mn = mean(count),
            moths.sd = sd(count))

moths.aov <- aov(count ~ trap * Location, data = moths)
summary(moths.aov)

#check normality of data
moths.res <- residuals(moths.aov)
hist(moths.res)

#homoscedasticity of results

plot(fitted(moths.aov), residuals(moths.aov))

#tukey test
moths.tukey <- TukeyHSD(moths.aov, which = "Location")
plot(moths.tukey) 
  


plt1 <- ggplot(data = moths, aes(x= Location, y = count))+
         geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plt2 <- ggplot(data = moths, aes(x = trap, y = count )) +
  geom_boxplot()+
  geom_jitter(width = 0.05, shape = 21)

plt3 <- ggplot(data = moths, aes(x = Location, y = count)) +
       geom_boxplot(aes(fill = trap)) +
  geom_jitter(width = 0.05, shape = 21)

ggarrange(plt1,plt2,plt3, labels = "Auto", ncol = 2, nrow = 2)

#Are the residuals normal?
moths.res <- residuals(moths.aov)
hist(moths.res)

# regression --------------------------------------------------------------


#residual is the difference from the observed value and predicted value

#For the explanation of this statistical analysis
#We are going to use eruption data from ol' Faithful

#Look at the top of the data
head(faithful)

#plot a quick scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions))+
  geom_point()


#There is a significant relationship between the duration of the eruption and 
#the the waiting time
#H0: Waiting does not have an influence on the eruption duration
#H1: Waiting time does have an influence on the eruption duration


# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)


# Notes -------------------------------------------------------------------
#Estimate Std is the point at which the line of best fit intercepts the y-axis, 
#note the waiting time
#The P-value explains whether the slope of the line is significantly greater 
#than zero
#Adjusted R-squared explains the amount of variation known










# correlations ------------------------------------------------------------

#assumptions
  
#pairwise data
#absence of outliers
#linearity
#normality of distribution
#homoscedasticity
#level (type) of measurement
    #Continuous data (Pearson correlation)  
    #Ordinal data (Spearman correlation)    



# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("ecklonia.csv")


# formulate hypothesis ----------------------------------------------------

#H0:there is no relationship between stipe length and stipe mass for kelp E.Maxima
#H1:There is relationship between stipe length and stipe mass for E.maxima


#Test a hypothesis

cor.test(ecklonia$stipe_length, ecklonia$stipe_mass)

ggplot(data = ecklonia, aes(x = stipe_length,y = stipe_mass)) +
  geom_point()



#Run hecka test at once
ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)


ecklonia_cor <- cor(ecklonia_sub) # correlates each variable to one another
ecklonia_cor 


#Spearman rank test

#create an ordinal column
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))

#cut creates bins or segments data

#now run spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")


#kendall test

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")



#visualise all things
ecklonia_pearson <- 

corrplot(ecklonia_cor, method = "circle")


# end of day 5 ------------------------------------------------------------



