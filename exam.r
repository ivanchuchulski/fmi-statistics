#1. Linear regression
NN <- 500
set.seed(73391)
x1 <- round(runif(NN, 0, 5), 1)
y <- 3 + (x1^2) + x1 + rnorm(NN)
df <- data.frame(x1, y)

model1 <- lm(y ~ I(x1^2) + x1, df)

summary(model1)
    # estimate : their coefficients in the regression equation
    # parameters are good if
        # p-value < 0.05
        # abs(t-value) >= 2

    # max(t-value) is the most valueble param

    # model is good if adjusted Rsq -> 1
    # model has unnecessary param if adj Rsq is < 0 

# residuals
resid1 <- residuals(model1)

# predictions
pred1 <- predict(model1, df)

# to see predictions vs real values
plot(pred1, df$y)


# conditions for good model
    # 1) homoscedasticity of the residuals
        # with test
        lmtest::bptest(model1)      # H0 : the is heteroscedasticity; p-value

        # or with plot
        plot(pred1, resid1)         # the graphic should be scattered across the plot

    # 2) no autocorrelation of the residuals
        lmtest::dwtest(model1)      # H0 : the is no autocorrelation; p-value

    # 3) residuals are normally distributed
        # with test
        shapiro.test(resid1)       # H0 : the data is normally distributed

        # or with graphic 
        qqnorm(resid1)
        qqline(resid1)

# 2. Location tests
    # 1 variable
        # if normally distributed 
        t.test(vec, mu = H0_value, alternative = "")
            # alternative in {less, greater, two.sided}
            # if mu in conf interval we accept H0, else we reject it
            
        # if not
        wilcox.test(vec, mu = H0_value, alternative = "", conf.int = TRUE)
            # alternative in {less, greater}

    # 2 variables
    # H0 : E[X] - E[Y] = 0
    # H1 : E[X] - E[Y] != 0
        # if all have normal distirbution :
            # check for equal variances
            var.test(x, y)      # H0 : equal variances
            t.test(x, y, var.equal = TRUE) # if not equal variances var.equal = FALSE  

        #  if one doesn't have :
        wilcox.test(y ~ x, data = df, conf.int = TRUE, exact = FALSE)

    # more the 2 variables
    # Н0: E[X1] = E[X2] = ... = E[Xn]  for all 1, ..., n 
    # H1: E[Xi] != E[Xj]               for at least one i != j.
        # if all have normal distirbution :
        # check for homoscedasticity
            barlett.test(y ~ x, data = df)      # H0 : equal variances
            result <- aov(y ~ x, data = df)
            summary(result)

        #  if one doesn't have :
        kruskal.test(y ~ x, data = df) 


        

