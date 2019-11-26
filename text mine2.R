# load the packages of "linear regression"
library(tidyverse)
library(MASS)
data("women")
str(women)

summary(women)

#using scatter plot to see if there is any relation between height and weight
ggplot(data = women, aes(x = weight, y = height)) +
  geom_point()

#"cor.test()" to calculate the p-value and the confidence interval 
cor.test(women$weight, women$height)

#create linear regression model using "lm()"
womenModel <- lm(formula = height~weight, data  = women)

#ggbline to draw a plot to define a straight line
coefficients <- ggplot(data = women, aes(x = weight, y = height)) +
  geom_point() +
  geom_abline(mapping = aes(slope = womenModel$coefficients[2],
                            intercept = womenModel$coefficients[1],
                            colour = "red"))
#????  using par() to figure out this problem
layout(matrix(1:6, ncol = 2, byrow = TRUE))
plot(womenModel, 1:6)
par(womenModel = c(1, 1))

#1.prdict value
newWomen <- data.frame(weight = c(130, 170))
predict(womenModel, newdata = newWomen)
#2.predict value using CI
newWomen <- data.frame(weight = c(130, 170))
predict(womenModel, newdata = newWomen, interval = "confidence")
#3. would the values on average
predict(womenModel, newdata = newWomen, interval = "prediction")

#shapiro.test(), if the p-value > 0.05, 
#then the population follows normal distribution

shapiro.test(women$height)
#W = 0.96359, p-value = 0.7545, follow normal distribition due to p>0.05
shapiro.test(women$weight)
#W = 0.96036, p-value = 0.6986, follow normal distribition due to p>0.05

#if we want to know the prediction of (hwy) and (displ)
?mpg
View(mpg)
ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = displ), fill = "red") +
  geom_histogram(mapping = aes(x = hwy), fill = "green")                 

#cowplot() to arrange the plots as a grid
install.packages("cowplot")
library(cowplot)
plot1 <- ggplot(mpg) +
  geom_histogram(aes(displ), fill = "red") 
plot2 <- ggplot(mpg) +
  geom_histogram(aes(displ), fill = "green")

cowplot::plot_grid(plot1, plot2)

#par() and hist()
par(mfrow = c(1, 2)) #set the output window to be 1 row and 2 columns
hist(x = mpg$displ, breaks = 8, main = "Displacement", xlab = "displ")
hist(x = mpg$hwy, breaks = 8, main = "Highway efficiency", xlab = "hwy")
par(mfrow = c(1, 1)) #reste the output window

#inspect the relationship by x-y scaaterplot
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = class))

#cor.test to check the relationship
cor.test(mpg$displ, mpg$hwy)

#linear regression
hwyDisplModel <- lm(formula = hwy~displ, data = mpg)
summary(hwyDisplModel)
summary(mpg$hwy) #this can get the mean

#draw a mpg linear regression plot 
 ggplot(data = mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(colour = class)) +
    geom_abline(aes(slope = hwyDisplModel$coefficients[2],
                 intercept = hwyDisplModel$coefficients[1])
                 , colour = "red")

#create new points to predict
newCarData <- data.frame(displ = c(2.2, 4, 6.3, 12))
predict(hwyDisplModel, newdata = newCarData, interval = "confidence") 
#access the value(predicted) from the regression line by "fitted.values"
hwyPred <- hwyDisplModel$fitted.values

#creat a dataframe to compare actual and pridicted value
mpgSubset <- data.frame(displ = mpg$displ, hwy = mpg$hwy, pred = hwyPred)
View(mpgSubset) 

#multiple linear regression
mpgModel <- lm(formula = mpg~cyl +disp +hp + drat + wt + 
              qsec + vs + am + gear +carb, data = mtcars)

summary(mpgModel)

#create the predicted values by "fitted.values"
mpgPredictions <- data.frame(mpg = mtcars$mpg, pred = mpgModel$fitted.values)
#plot the scatterplot and line plot to indicate where the error.

