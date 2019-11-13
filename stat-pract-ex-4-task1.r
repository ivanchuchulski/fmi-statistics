IP <- installed.packages()  # разглеждаме кои библиотеки имаме инсталирани
IP <- IP[, "Package"] # Взимаме колоната с имената на пакетите

li <- setdiff(c("quantmod", "fBasic"), IP)  # Библиотеки, които са се инсталират
# setdiff - Връща елементите на първото множество, които не са елементи от второт множество

if(length(li) > 0) {for(i in li) {install.packages(i)}}


# зареждане на библиотеките
library(quantmod)
library(fBasics)










#   Задача 1
#   Оценете активите - възвръщаемост, риск, Sharpe ratio, Value-at-risk (VaR) за 5%. 
# Сравнете два произволни актива. Постройте портфейл от активи и го оценете.
#   Използвайте данните от библиотеката "fBasics" "DowJones30"

#   Анализът да се извърши върху възвръщаемостите на цените на активите
#   Да се изследва графично разпределенията на активите с помощта на Q-Q plot и хистограма





#   Решение
data("DowJones30")  # Зареждане на данните

head(DowJones30)  # разглеждане на данните - цолона с дати и цени на 30 компании от DJI30



#     Функции
sharpe_ratio_F <- function(x) {mean(x) / sd(x)}


#   portfolio_F - функция, която изчислява Sharpe ratio на портфейл
portfolio_F <- function(w, returns_DF, type = c("sharpe", "var")) {
  portfolio_returns <- as.vector(matrix(w, nrow = 1) %*% t(returns_DF))
  
  if(type == "sharpe") {return(-sharpe_ratio_F(portfolio_returns))} 
  if(type == "var") {return(-quantile(portfolio_returns, prob = 0.05))} 
  
  return(0)
}

#   изчисляваме възвръщаемостите
return_F <- function(x, type = "return") {
  r <- array(dim = length(x))#  създаваме масив с дължина дължината на X и стойности NA
  
  if(type == "log") {r <- c(0, diff(log(x)))}
  if(type == "return") {r <- c(0, x[-1]/x[-length(x)] - 1)}
  
  r
}



#   Анализите се правят върху възвръщаемостите на активите, следователно трябва да ги изчислим.
# За целта ще използваме log представяне на възвръщаемостта.

#   Защо използваме логаритмично представяне на възвръщаемостите вместо стандартната формула?
#   Защото с помощта на log представянето имаме заглжадане на възвръщаемостите, при което заглаждане
# положителните шокове не изместват оценката и реалните резултати са по-близки до абослютната стойност.
# За малки изменения, възвръщаемостите по двата метода почти съвпадат.


companies <- c(#"XOM", "GE", 
  "DIS", "WMT")

prices_DF <- DowJones30[, companies]  # Създаваме data frame с цените на инструментите, които искаме да изследваме

prices_return_DF <- prices_DF
for(col_index in 1:ncol(prices_return_DF)) {
  prices_return_DF[, col_index] <- return_F(prices_return_DF[, col_index], type = "log")
}





#   Разглеждаме двете компании
x <- prices_return_DF[, companies[1]]
y <- prices_return_DF[, companies[2]]



par(mfrow = c(2, 2))
  hist(x, main = paste("Histogram:", companies[1]), col = "red", xlab = "Returns", breaks = 20)
  qqnorm(x, main = paste("Normal Q-Q plot:", companies[1]));  qqline(x)

  hist(y, main = paste("Histogram:", companies[2]), col = "red", xlab = "Returns", breaks = 20)
  qqnorm(y, main = paste("Normal Q-Q plot:", companies[2]));  qqline(y)
par(mfrow = c(1, 1))


#   Разпределенията на възвръщаемостите и на двете компании НЕ са нормални, защото на графиката 
# "Normal Q-Q Plot" се забелязват тежки опашки (крайните наблюдения не са по линията).
#   Тежките опашки показват, че вероятността за сбъдване на екстремална стойност е по-голяма
# отколкото при теоретично разпределение.


#   Очаквана възвръщаемост
summary(x)
mean(x, trim = 0.05)

summary(y)
mean(y, trim = 0.05)


#   Риск
mad(x)
sd(x)
quantile(x, prob = 0.05)  # Value-at-risk 

mad(y)
sd(y)
quantile(y, prob = 0.05)  # Value-at-risk 
#   Value-at-risk показва каква сума бихме загубили, ако нещата тръгнат на зле при конкретна 
# вероятност. В нашия случай тази вероятност е 5%.


#   Sharpe ratio
sharpe_ratio_F(x)
sharpe_ratio_F(y)
#   Показва ни за всяка единица риск, колко единици възвръщаемост получаваме. Стремим се 
# към по-големи стойности на този коефициент. 
#   Можем да използваме този коефициент при сравняването на активи.
#   Работи добре при условие, че възвръщаемостите са нормално разпределени.


#   Сравняване на двата актива
x_cumsum <- cumsum(x)
y_cumsum <- cumsum(y)
# Кумулативна сума на целия вектор. Тоест a[1] = x[1], a[2] = x[1] + x[2], ... a[n] = x[1] + ... + x[n]
r <- range(c(x_cumsum, y_cumsum))# връща максималната и минималната стойност на вектор

plot(x_cumsum, ylim = r, type = "l", col = "blue", ylab = "returns")
lines(y_cumsum, col = "red")
legend("topleft", fill = c("blue", "red"), legend = companies)

#   В кой актив е по-добре да се инвестира?



#       Допълнение, което не е разглеждано в час
#   Създаване на портфейл

#   Създаваме data frame, който да съдържа теглата на всеки актив. Тоест, кой актив какъв процент представлява
# в портфейла
A <- seq(0, 1, 0.01) # тегла (измерват се в проценти)
B <- 1 - A  # Защо процентите на B са 1 - A?
W_DF <- cbind(A, B)


#   Създаваме матрица с процентите на двата актива
AB_returns <- cbind(x, y)
head(AB_returns)



sharpe_ratio_values <- apply(W_DF, 1, portfolio_F, returns_DF = AB_returns, type = "var")
#   С помощта на функцията "apply", за всеки един ред от матрицата W_DF, получаваме стойност за 
# функцията "portfolio_F". Последните два параметъра "returns_DF" и "type" принадлежат на 
# portfolio_F.
#   Така получаваме вектор с числови стойности с размер броя на редовете на W_DF.
head(sharpe_ratio_values, 10)

#   Остава да изберем оптималната стойност. 
min(sharpe_ratio_values)  # Връща минималната стойност
which.min(sharpe_ratio_values)  # Показва индекса на първата минимална стойност
(optimal_portfolio_W <- W_DF[which.min(sharpe_ratio_values), ])

#   Получаваме възвръщаемостите на портфейла за всеки ден. Как?
# Събираме произведението от процента на всеки актив (участващ в портфейла) и процентът на възвръщаемостта
# r(P) = r(A1)*w(A1) + r(A2)*w(A2) + ... + r(An)*w(An),
# където r(.) - възвръщаемост на актива, w(.) - процентното участие на актива в портфейла

portfolio_returns <- matrix(optimal_portfolio_W, nrow = 1) %*% t(AB_returns)
portfolio_returns <- as.vector(portfolio_returns)


p_cumsum <- cumsum(portfolio_returns)
r <- range(c(x_cumsum, y_cumsum, p_cumsum))

plot(x_cumsum, ylim = r, type = "l", col = "blue", ylab = "returns")
lines(y_cumsum, col = "red")
lines(p_cumsum, col = "green")

legend("topleft", fill = c("blue", "red", "green"), legend = c(companies, "portfolio"))


round(summary(portfolio_returns), 4)
round(summary(x), 4)
round(summary(y), 4)


sd(portfolio_returns)
sd(x)
sd(y)


quantile(portfolio_returns, prob = 0.05)
quantile(x, prob = 0.05)
quantile(y, prob = 0.05)


sharpe_ratio_F(portfolio_returns)
sharpe_ratio_F(x)
sharpe_ratio_F(y)
