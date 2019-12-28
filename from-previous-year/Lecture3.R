library(UsingR)
# www.kaggle.com/c/titanic/data
titanic = read.csv("train.csv")
View(titanic)

## Univariate Data

# Categorical data (Категорни данни)
titanic$Survived
factor(titanic$Survived)
# Frequency table (Честотна таблица)
table(titanic$Survived)
# Barplot (Барплот)
barplot(table(titanic$Survived))
barplot(table(titanic$Survived), 
        names.arg = c("Dead", "Survived"),
        col = c("red", "blue"),
        main = "People on Titanic",
        ylab = "Frequency")
table(titanic$Survived)/length(titanic$Survived)
barplot(table(titanic$Survived)/length(titanic$Survived),
        names.arg = c("Dead", "Survived"),
        col = c("red", "blue"),
        main = "People on Titanic",
        ylab = "Probability")
#Pie Chart (Кръгова диаграма)
pie(table(titanic$Survived),
    labels = c("Dead", "Alive"),
    col = c("Red", "Blue"),
    main = "People on Titanic")

# Numerical data (Количествени данни)
titanic$Fare
# Вариационен ред
sort(titanic$Fare)
# Минимална стойност за цена на билет
min(titanic$Fare)
# Максимална стойност за цена на билет
max(titanic$Fare)
# Средно на цените на билетите
mean(titanic$Fare)
# Медиана на цените на билетите
median(titanic$Fare)
# Дисперсия на цените на билетите
var(titanic$Fare)
# Стандартно отклонение на цените на билетите
sd(titanic$Fare)
# Първи квартил
quantile((titanic$Fare), 0.25)
# Трети квартил
quantile((titanic$Fare), 0.75)
# range
# R = max - min..
# IQR (Интерквартилен размах)
IQR(titanic$Fare)
# Min H_lower Median H_upper Max
fivenum(titanic$Fare)
# Min 1st_Q Median Mean 3rd_Q Max
summary(titanic$Fare)
# Stem-and-leaf Chart
stem(titanic$Fare, scale = 1)
# Хистограма
hist(titanic$Fare,
     include.lowest = TRUE,
     right = TRUE,
     main = "Ticket price")
rug(jitter(titanic$Fare))
# Кутия с мустачки
boxplot(titanic$Fare,
        horizontal = TRUE)
# Хистограма и курия с мустачки
simple.hist.and.boxplot(titanic$Fare)
# skewed
# Полигон
h = hist(titanic$Fare)
h$breaks
h$counts
h$density
h$mids
h$xname
lines(c(min(h$breaks), h$mids, max(h$breaks)),
      c(0, h$counts, 0),
      type = "l")
simple.freqpoly(titanic$Fare)
# Density (Плътност)
hist(titanic$Fare,
     probability = TRUE)
lines(density(titanic$Fare, bw = 10), col = "blue")