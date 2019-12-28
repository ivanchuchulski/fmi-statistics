library(UsingR)
# www.kaggle.com/c/titanic/data
titanic = read.csv("train.csv")
View(titanic)

## Bivariate Data
# The relationship bertween 2 variables

# Bivariate categorical data
# Example - students who smoke study less
# amount (1  - less than 5 hours studing, 
#         2  - 5 - 10 hours studying,
#         3  - more than 10 hours studying)
smokes = c("Yes","No","No","Yes","No","Yes","Yes","Yes","No","Yes")
amount = c(1,2,2,3,3,1,2,1,3,2)
table(smokes,amount)
## proportions (Пропорции)
options(digits=3) # only print 3 decimal places
# row proportions - rows sum to 1
prop.table(table(smokes,amount),1)
# column proportions - columns sum to 1
prop.table(table(smokes,amount),2)
# proportion - all the numbers sum to 1
prop.table(table(smokes,amount))
# marginal totals (Маргинални вероятности)

#####
hair = c("blond", "blond", "black", "blond", "brown", "brown", 
         "brown", "brown", "black", "brown", "black", "brown",
         "black", "black", "black", "brown", "brown", "brown",
         "brown", "brown", "black", "brown", "black", "brown",
         "blond", "blond", "black", "blond", "brown", "brown", 
         "brown", "brown", "black", "brown", "black", "brown",
         "brown", "brown", "black", "brown", "black", "brown",
         "blond", "blond", "black", "blond", "brown", "brown")
eyes = c("blue", "green", "brown",  "blue", "green", "brown",
         "brown", "black", "black", "green", "brown", "brown",
         "green", "black", "black", "brown", "brown", "black",
         "green", "black", "black", "brown", "brown", "black",
         "brown",  "blue", "green", "brown", "brown", "black",
         "black", "green", "brown",  "blue", "green", "brown",
         "brown", "black", "brown",  "blue", "green", "brown",
         "blue", "green", "brown",  "blue", "green", "brown")
table(hair, eyes)
prop.table(table(hair, eyes),1)
prop.table(table(hair, eyes),2)
prop.table(table(hair, eyes))
#####

# Барплот (Barplot)
barplot(table(smokes,amount))
barplot(table(amount,smokes))
?barplot
barplot(table(smokes,amount),
        names.arg = c("<5 hours", "5-10 hours", ">10 hours"),
        beside=TRUE,
        col = c("Green", "Grey"),
        xlab = "study hours")
barplot(table(smokes,amount),
        names.arg = c("<5 hours", "5-10 hours", ">10 hours"),
        legend.text=TRUE,
        beside=TRUE,
        col = c("Green", "Grey"),
        xlab = "study hours")
barplot(table(amount,smokes),
        legend.text=c("<5 hours studying","5-10 hours studying",">10 hours studying"),
        beside=TRUE,
        col = c("Yellow", "Orange", "Red"))
# See at home: apply, tapply, mapply, sapply
# You can also see ggplot2 package - for more fancy plots

# Categorical vs Numerical data
# Example - Drug test - experimental group and control group
experimentalGroup = c(5, 5, 5, 13, 7, 11, 11, 9, 8, 9)
controlGroup = c(11, 8, 4, 5, 9, 5, 10, 5, 4, 10)
boxplot(experimentalGroup,controlGroup)
# The same example
# response ~ predictor
amount = c(5, 5, 5, 13, 7, 11, 11, 9, 8, 9, 11, 8, 4, 5, 9, 5, 10, 5, 4, 10)
category = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
boxplot(amount ~ category)
# breaking up the values in amount, by the categories in category
# amount by category

# Numerical vs numerical data
# independent variables -> compare the distributions
# Example - home data
library("UsingR")
head(home)
names(home)
home$old
home$new
boxplot(home$new)
boxplot(home$old)
boxplot(home$new, home$old)
home$old
mean(home$old)
sd(home$old)
scale(home$old)
scale(home$new)
boxplot(scale(home$old),scale(home$new))
# Dot plots (Точкова диаграма)
stripchart(home$old)
stripchart(home$new)
stripchart(scale(home$old))
# Violin Plot instead of boxplots
violinplot(scale(home$old), scale(home$new))
simple.violinplot(scale(home$old),scale(home$new))

# independent -> compare relationships
# Investigate one numerical variable against another
# Scatter plot
home
plot(home$old, home$new)
min(home$old)
min(home$new)
# entire data set
homedata
plot(homedata$y1970, homedata$y2000)
# strong linear trend

# Pearson correlation coefficients [-1 < Corr < 1]
# - scaled Covariance between X and Y
# - How one variable varies as the other does
# 1 - strong linear relationship
# 0 - weak one (no linear realtionship)
# The Pearson correlation coefficient indicates the strength of 
# a linear relationship between two variables, 
# but its value generally does not completely characterize their relationship.
# See the two pictures here:
# https://en.wikipedia.org/wiki/Correlation_and_dependence
cor(home$old, home$new)
cor(home$old, home$new)^2
# Funny online games to predict correlations
# http://guessthecorrelation.com/
# http://istics.net/Correlations/

homedata
plot(homedata$y1970, homedata$y2000)
plot(homedata$y2000 ~ homedata$y1970)
cor(homedata$y1970, homedata$y2000)
plot(homedata)

# Linear regression
homedata
plot(homedata$y1970, homedata$y2000)
abline(lm(homedata$y2000 ~ homedata$y1970))
simple.lm(homedata$y1970, homedata$y2000)
lm.res = simple.lm(homedata$y1970, homedata$y2000)
coef(lm.res)
coef(lm.res)[1]
coef(lm.res)[2]
simple.lm(homedata$y1970, homedata$y2000, show.residuals = TRUE)
par(mfrow = c(1,1))

summary(lm(homedata$y2000 ~ homedata$y1970))

head(emissions)
plot(emissions)
plot(emissions$CO2, emissions$GDP)

florida
plot(florida)
names(florida)
cor(florida$BUSH, florida$BUCHANAN)
plot(florida$BUSH, florida$BUCHANAN)
simple.lm(florida$BUSH, florida$BUCHANAN)
simple.lm(florida$BUSH, florida$BUCHANAN, show.residuals = TRUE)
#Strong linear relationship, except for two outliers
par(mfrow = c(1,1))
simple.lm(florida$BUSH, florida$BUCHANAN)
identify(florida$BUSH, florida$BUCHANAN)
# Click on the two outliers
# Esc to finish and print the identified cordinates
florida[50,]
florida.new = florida[-50, ]
lm.res = simple.lm(florida.new$BUSH, florida.new$BUCHANAN)
coef(lm.res)[1]
coef(lm.res)[2]
x = 152846
y = coef(lm.res)[1]+coef(lm.res)[2]*x;y
# We expect Buchanan to have received 598 votes, not 3407 as actually received.

simple.lm(florida$BUSH, florida$BUCHANAN)
abline(65.6, 0.00348)

# Multivariate Data
weight = c(150, 135, 210, 140)
height = c(65, 61, 70, 65)
gender = c("Fe","Fe","M","Fe")
study = data.frame(weight,height,gender)
study

PlantGrowth
unstack(PlantGrowth)
boxplot(unstack(PlantGrowth))

hair = c("blond", "blond", "black", "blond", "brown", "brown", 
         "brown", "brown", "black", "brown", "black", "brown",
         "black", "black", "black", "brown", "brown", "brown",
         "brown", "brown", "black", "brown", "black", "brown",
         "blond", "blond", "black", "blond", "brown", "brown", 
         "brown", "brown", "black", "brown", "black", "brown",
         "brown", "brown", "black", "brown", "black", "brown",
         "blond", "blond", "black", "blond", "brown", "brown")
eyes = c("blue", "green", "brown",  "blue", "green", "brown",
         "brown", "black", "black", "green", "brown", "brown",
         "green", "black", "black", "brown", "brown", "black",
         "green", "black", "black", "brown", "brown", "black",
         "brown",  "blue", "green", "brown", "brown", "black",
         "black", "green", "brown",  "blue", "green", "brown",
         "brown", "black", "brown",  "blue", "green", "brown",
         "blue", "green", "brown",  "blue", "green", "brown")
sex = c("female", "male", "female", "female", "female", "male",
        "male", "male", "female", "female", "male", "male",
        "male", "male", "female", "male", "male", "female",
        "female", "male", "female", "female", "female", "male",
        "male", "male", "female", "female", "male", "male",
        "male", "male", "female", "male", "male", "female",
        "female", "male", "female", "female", "female", "male",
        "male", "male", "female", "female", "male", "male")
student = c("yes", "no", "no", "no", "no", "yes",
            "yes", "no", "yes", "no", "yes", "no",
            "no", "yes", "yes", "yes", "no", "yes",
            "yes", "no", "no", "no", "no", "yes",
            "yes", "no", "yes", "no", "yes", "no",
            "no", "yes", "yes", "yes", "no", "yes",
            "yes", "no", "no", "no", "no", "yes",
            "yes", "no", "yes", "no", "yes", "no")
table(hair, eyes, sex, student)
prop.table(table(hair, eyes, sex, student))
ftable(hair, eyes, sex, student)

x = rnorm(100)
y = factor(rep(1:10,10))
stripchart(x ~ y)

InsectSprays
boxplot(InsectSprays$count ~ InsectSprays$spray, col = "lightgray")
simple.violinplot(InsectSprays$count ~ InsectSprays$spray, col = "lightgray")
simple.densityplot(InsectSprays$count ~ InsectSprays$spray)

emissions
simple.scatterplot(emissions$perCapita,emissions$CO2)

pairs(emissions)

install.packages("lattice")
library(lattice)
library(UsingR)
histogram( ~ Max.Price | Cylinders , data = Cars93)

bwplot( ~ Max.Price | Cylinders , data = Cars93)

xyplot(MPG.highway ~ Fuel.tank.capacity | Type, data = Cars93)
