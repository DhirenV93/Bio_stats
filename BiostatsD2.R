#Day2
#dhiren vanmari
#biostats


# load libraries ----------------------------------------------------------

(library(tidyverse))


# calculation -------------------------------------------------------------
?data.frame
?rnorm #rnorm to create random numbers with specific parameters

#Gen random data
r.dat <- data.frame( dat= rnorm(n =600, mean = 372, sd = 50), sample = "A" )

#quick visualisation
ggplot(data = r.dat, aes(x = dat)) + #density graph or histogram to illustrate data distributuion
  geom_density()


# summary stats. ----------------------------------------------------------


#the mean = sum of all points / by total numm of data
#manually using  formular
r.dat %>% 
  summarise(r.sum = sum(dat),
            r.n = n(), 
            r.mean = r.sum/r.n,    #dont use static numbers rather use directly frm data( count)
            r.mean.func = mean(dat)) #as an alternative to calc mean using r func



#the median = middle value/s in ascending order 

r.dat$dat[(length(r.dat$dat)/2)]
#$ extracts data from a tibble 
#length another function for count
# this is not right becuase not arranged in ascending order


#median -using tidyverse
r.dat %>% 
  arrange(dat) %>% #from lowest to highest
  slice(n()/2) # n/2 because even number of data points


#or simply

r.dat %>% 
  summarise(r.med = median(dat))
#or
median(r.dat$dat)


#variance = sum of (each value - the mean )sqrd / count of samples -1

#manual computation
r.dat %>% 
  mutate(r.error = dat - mean(dat), #value-mean
         r.error.sqr = r.error * r.error) %>% #sqrd (value- mean)
  summarise(r.sqr.sum = sum(r.error.sqr)/n()-1) 
  
        
          
#variance
r.dat %>% 
  summarise(r.var = var(dat),
            r.sd = sqrt(r.var), #standard deviation calculated from variance
            r.sd.func = sd(dat)) #standard deviation calculated using sd function 


# exercise ----------------------------------------------------------------
#create summary stats for chicken data


summary(ChickWeight$weight)


ChickWeight %>% 
  summary(min.weight= min(weight),
          quart1 = quantile(weigth,0.25),
          med.weight = median(weight),
          mean.weight = mean(weight),
          quart3 = quantile(weight, 0,75),
          max.weight = max(weight))


# visualising data --------------------------------------------------------
#load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(viridis)


#load data
# how long is now?, now now? just now?
library(readr)
now_nownow <- read_csv("now_nownow")

satime <-now_nownow

sa.time <- satime %>% 
  mutate(human = seq(1, n(),1)) #create a column by numbering the amount of observations



#create long data
sa.long <- sa.time %>% 
  gather(key = "time.type", value = "minutes", -human)

#gather gathers multiple columns and collapses columns into rows with key values

# visualising Qualitative data


#stacked bar graphs 
sa.count <- sa.long %>%
  count(time.type) %>% #count the number of time types
  mutate(prop = n/ sum(n)) #proportion of each species in a column


#stacked bar graph - cumulative sum  
stckd.count <- ggplot(data = sa.count,aes(x = "", y = prop, fill = time.type)) +
  geom_bar(width = 1, stat= "identity") + #"identity" suppresses detecting an x value
  labs(title = "stacked bar graphs" , subtitle = "cumulative sum", x= NULL, y = "proportion") +
  theme_minimal()


#pie chart but rather make a bar graph

pie.count <- stckd.count + coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()


#bar graph
bar.coun <-ggplot(data = sa.long,aes(x =time.type, fill = time.type)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Sa time", subtitle = "How long is now? , now now, just now?", y = "Count") +
  theme_minimal()

ggarrange(stckd.count,pie.count,bar.coun,ncol = 2, nrow =2, labels = "AUTO")


# visualising Qauntitative data --------------------------------------------

#histograms (frequency or count)

ggplot(data = sa.long, aes(x =minutes)) +
  geom_histogram()


sa.clean <- sa.long %>% 
  filter(minutes < 10000) #to expand the intergers to 10000 decimal places = better represtation of data


#Faceted histogram
ggplot(data = sa.clean, aes(x = minutes)) +
  geom_histogram(aes(fill= time.type), position = "dodge")+ #position =dodge unstacks 
  facet_wrap(~time.type, ncol = 1, scales = "free_x") #facetwrap breaks graph into 3 graphs for each timetype


#Relative proportion histogram
ggplot(data = sa.clean, aes(x = minutes))+
  geom_histogram(aes(y = ..density.. ,fill = time.type),
                 position = "dodge", binwidth = 1) + 
  facet_wrap(~time.type, ncol = 1, scales = "free_x")


#BOXPLOT
#when ever we create a figure we always start with inseting a function
#boxplots have both x and y axis
#boxplot is a mixture of both qualitative and quantitative data
ggplot(data = sa.clean, aes(x = time.type, y = minutes)) + 
  geom_boxplot(aes(fill = time.type))


#the black line in the middle of the the histograme is the median value
#the dot is the max value, but why is it a dot? the lines are the tails (whiskers). 
#in between the q1 and the g3 is the interquartile range
#multiply the interq range by 1.5 to find the tail
ggplot(data = sa.clean, aes(x = time.type, y = minutes)) + 
  geom_boxplot(aes(fill = time.type), notch = TRUE)

#there is no statistical difference due to the notch overlaping 

#CALCULATING SUMMARY STATS FOR PLOTTING OVER THE BOXPLOTS
sa_summary_stats <- sa.clean %>% 
  group_by(time.type) %>% 
  summarise(time_type_mean = mean(minutes))

#plot these means over the boxplots
ggplot(data = sa.clean, aes(x = time.type, y = minutes)) + 
  geom_boxplot(aes(fill = time.type), notch = TRUE) + 
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")


# Relationships -----------------------------------------------------------

sa_time_clean <- sa.time %>% 
  filter(just_now)

#A basic scatterplot
ggplot(data = sa.time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = "Geo")) +
  coord_equal(xlim = c(0, 60), ylim = c(0,60))

#Adding trend lines
ggplot(data = sa.time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = "geo")) +
  geom_smooth(aes(colour = "geo"), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))






