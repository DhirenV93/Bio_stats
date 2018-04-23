#biostatsD3
#17 april 2018
#basics on how to run a t-test and how data should look like before running a t-test


#generate a cullen and fray graph- determine the type of distribution of data


library(fitdistrplus) 
library(logspline) 


# ASSUMPTIONS -------------------------------------------------------------

#To conduct a t-test make sure that the following applies to your data:
#the dependent variable must be continuous (i.e. it is measured at the interval or ratio level),
#the observations in the groups being compared are independent of each other,
#the data are NORMALLY DISTRIBUTED, and
#that the data are HOMOSCEDASTIC, and in particular, that there are no outliers.




#generate normal data 
r.norm <- rnorm(n= 1000, mean = 13, sd = 1)

#graphs- what this data looks like
hist(r.norm)


#check what kind of data you have, using descdist function
# requires library(fitdistrplus)
descdist(r.norm, discrete = FALSE, boot = 100)
#we have normal dist data as indicated by blue dot

#uniform data

y <- runif(100) 
par(mfrow = c(2, 2)) 
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)
y



# t-test ------------------------------------------------------------------
#t-test used when testing two variables
#anova for multiple variables


library(tidyverse)
#create normal data
#random normal
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3), #combine a dat frame
                            rnorm(n = 1000 , mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000))) 

#1st check NORMALITY----use shapiro-wilk test

#tests all data together
shapiro.test(r_dat$dat) #but thios wrong as its not fpr the groups A and B

r_dat %>%
  group_by(sample) %>% 
  summarise(r.norm.dist = as.numeric(shapiro.test(dat)[2])) #use [2] to obtain p-value

#data is normal when p >0.05 (above) 
# not normal when p <=0.05

# 2d check for HOMOSCEDASTICITY
#many ways to check for homoscedasticity
#homoscedascity = the similiarity of variance between  sample sets
#for now we will simply say that this assumption is met when the variance of the
# sammples are not more than 2-4 times greater than one anoth


#check everything at once 
var(r_dat$dat) #but this is wrong because this checks all the data not groups A and B
 
#therefore
 #r_norm_dist  p valu
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

#one sided one sample t-test --------------------------------------------------------
#1 sample set of data tesdted against a known population mean


#create a single sample of random normal data

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")


#run test 
t.test(r_one$dat, mu = 20) #mu population mean



# Two sided one sample t-test -------------------------------------------------------------

# are these data smaller/less than the population mean

t.test(r_one$dat, mu = 20, alternative = "less")
#or greater
t.test(r_one$dat, mu = 20, alternative = "greater")


#for larger population means 
#are the samples less than the population of 30
t.test(r_one$dat, mu = 30, alternative = "less")
#or greater
t.test(r_one$dat, mu = 30, alternative = "greater")
  #no because pval = 1



# two sample t-tests ------------------------------------------------------

#create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD 

t.test(dat ~ sample, data = r_two, var.equal = TRUE)



t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")


t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")


#6.6 continue

# r workflow --------------------------------------------------------------

library(readr)
library(ggplot2)
ecklonia <- read_csv("ecklonia.csv") %>% 
gather(key = "variable", value = "value", -species, -site, -ID)


#lets visualise this
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()
#stipe_mass look similar 


# come up with a hypothesis -----------------------------------------------

#lets get a picture the stipe mass better to formulate a hypothesis

# filter stipe_mass data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

#boxplot
ggplot(ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() + #flips the cor-ordinates
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#we hypothesise that
#H0: Stipe mass at Batsata Rock is not greater than at Boulders Beach.
#H1: Stipe mass at Batsata Rock is greater than at Boulders Beach.


ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = var(value),
            stipe_mass_norm = as.numeric(shapiro.test(value)[2]))

#now run the analysis
t.test(value ~ site, data = ecklonia_sub,
       var.equal = TRUE, alternative = "greater")

compare_means(value ~ site, data = ecklonia_sub,
              method = "t.test",
              var.equal = TRUE,
              alternative = "greater")
#conclusion
#The stipe mass (kg) of the kelp Ecklonia maxima was found to be significantly 
#greater at Batsata Rock than at Boulders Beach (p = 0.03, t = 1.87, df = 24).



