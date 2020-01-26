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





