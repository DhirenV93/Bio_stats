#BiostatsD4
#doing an ANOVA
#19 April 2018

library(tidyverse)


# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)


#t-test
t.test(weight ~ Diet, data = chicks_sub)

#we do not reject the null hypothesis


# 1way ANOVA --------------------------------------------------------------

#research question: is there a difference in chicken mass attained after 21
#days after the chickens having been fed four different diets

#Null hypothesis: THer is no difference in chicken mass at 21 days after having 
#been fed one of four diets

Chicks21 <- chicks %>% 
  filter(Time == 21)

chicks.aov <- aov(weight ~ Diet, data = Chicks21)
summary(chicks.aov)

#look at Pr which is <0.05
#we don not accept the null hypothesis, we accept the alternative hypothesis
#where diets significantly influence chicken mass after day 21

ggplot(Chicks21, aes(x = Diet, y = weight))+
  geom_boxplot(aes(fill= Diet), notch = TRUE)
#notchs are indentation in box plots where imaginary band notch cease to overlap they are significant




# TukeyHSD ----------------------------------------------------------------

TukeyHSD(chicks.aov) #compares diets to one another

#boxplot
ggplot(Chicks21, aes(x = Diet, y = weight, fill = Diet))+
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(X = Diet, xend = , y = weight, yend = weight/2))

#segments showing confidence intervals 
#Data frame of segments

chicks.tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = Chicks21))$Diet)
chicks.tukey$pairs <- as.factor(row.names(chicks.tukey))


ggplot(chicks.tukey, aes(x = pairs, y = diff)) +
geom_segment(aes(x = lwr, xend =upr, y= pairs, yend = pairs)) +
  geom_vline(xintercept = 0, linetype = "dotted")) +

 

#or
plot(TukeyHSD(aov(weight ~ Diet, data = Chicks21)))


#t-test tests 2 means


# multiple factors --------------------------------------------------------
#looking at time 

#H0: there is no change in chicken mass from day 1 to day 21

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0,21))))


chicks0.21 <- ChickWeight %>% 
  filter(Time %in% c(0,21))

visualise the data
ggplot(data = chicks0.21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = TRUE, aes(fill = as.factor(Time)))

#Run the ANOVA
 
summary(aov(weight ~ as.factor(Time), data = chicks0.21))


#perform a tukey post hoc test
TukeyHSD((aov(weight ~as.factor(Time), data = chicks0.21)))
 
#look at CI
plot(TukeyHSD((aov(weight ~as.factor(Time), data = chicks0.21))))

#looking only at day 0 and 21 fro both time and Diet
summary(aov(weight ~ as.factor(Time), data = filter(ChickWeight, Time %in%c(0,21))))

#look at all of the time....not the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
#note increase in df for time factor

 
#for every time at every diet
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21)))))

#create a line graph to explain this concept
#first create mean values
chick.means <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight.mean = mean(weight, na.rm = T))

ggplot(data = chick.means, aes(x = Time, y = weight.mean, colour = Diet))+
  geom_line() +
  geom_point()





# non parametric test -----------------------------------------------------

#but what if we have normal data?
#for a t-test we rather use Wilcox rank sum test
wilcox.test()# and then one fills this the same as  for a t-test


#and no for the kruskall- wallis
kruskal.test(weight ~ Diet, data = chicks0.21)

#load  this for a non-parametric post-hoc test
library(pgirmess)
Kruskalmc(weight ~ Diet, data = chicks0.21)
