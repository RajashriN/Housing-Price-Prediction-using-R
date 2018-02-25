##housing price visualisation
head(datagp1)

library(ggplot2)

ggplot(datagp1,aes(age))+geom_density(kernel = "gaussian")
##density plot shows that the data has more people above age 75

ggplot(datagp1) + geom_point(aes(x = crim, y = chas)) 

##chas level 0 has more crim populated than chas level 1


ggplot(datagp1) + geom_density(aes(x = crim, color = rm)) + facet_wrap(~chas)
##density of crim per chas level,density 0 shows huge spike under 25%.

ggplot(datagp1,aes(x = chas))+geom_bar()
##chas level 0 is high
ggplot(datagp1,aes(x = rad))+geom_bar()
##rad has most populations in the interval 0 to 10 and there after populations closer to 25.

library(dplyr)
datagp1 %>% select(rad, tax) %>% ggplot(aes(factor(rad), tax)) + geom_boxplot(range = 0) + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('rad vs tax')
##plots show rad levels 1,2,3 have lesser tax compared to other levels
##There are few rad levels with high taxes especially 4 & 24

datagp1 %>% select(lstat) %>% ggplot(aes(lstat)) + geom_density() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('lstat')

##plot shows that the population is skewed left ,lstat mean is closer to 10

ggplot(datagp1, aes(age)) + geom_histogram(binwidth = 5, fill = "red", size = 0.5, linetype = 1, colour = "black") + xlab("Age") + ylab("Frequency")

ggplot(datagp1, aes(x = black, color = lstat)) +
  geom_histogram(bins = 25) 

sp3<-ggplot(datagp1, aes(x=crim, y=rm, color=rm)) + geom_point()
sp3
sp3+scale_color_gradientn(colours = rainbow(5))
##rm is between 5 & 7 for crim between  0 to 25 .

##Checking for counts of black
ggplot(datagp1, aes(black)) +geom_histogram(binwidth = 30)



library(car)
scatterplotMatrix(~age+ptratio+rm+crim, data=datagp1, id.n=2)

plot(model)
##The car package has many more functions for plotting linear model objects
##Among these, added variable plots show the partial relations of y to each x, holding all
##other predictors constant.
##avPlots(model, id.n=2,ellipse=TRUE) 


ggplot(datagp1, aes(zn)) + geom_histogram(color = "black",bins = 30)+
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "zn", y = " Count")
##zn has higher population below 5


ggplot(datagp1, aes( x = lstat, y = medv)) + geom_point() +
  geom_smooth(method = "lm")
##lstat and medv are negatively related

ggplot(datagp1, aes( x = medv, y = zn)) + geom_point() +
  geom_smooth(method = "lm")
##medv and zn are related positively
ggplot(datagp1, aes( x = medv, y = rm)) + geom_point() +
  geom_smooth(method = "lm")
##medv and rm are related positively
ggplot(datagp1, aes( x = medv, y = dis)) + geom_point() +
  geom_smooth(method = "lm")
##medv and dis are related positively

