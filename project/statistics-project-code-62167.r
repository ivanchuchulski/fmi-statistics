

# инсталиране на пакета от данни
install.packages("MASS")

# зареждане на пакета от данни
library("MASS") # or require("MASS")

# извличане на данните само от желаните колони
columns_to_keep <- c("Sex", "Wr.Hnd", "Height")

mydata <- survey[columns_to_keep]

mydata <- subset(mydata, !is.na(mydata$Sex) & 
                        !is.na(mydata$Wr.Hnd) & 
                        !is.na(mydata$Height))

# преименуваме колоната за дължината на дланта
colnames(mydata)[2] <- "Handspan"

# записваме данните в csv файл
write.csv(mydata, 'D:\\survey_data.csv', row.names = FALSE)

# 2. Изследване на променливите поотделно
# 2.1. пол, категорийна номинална
    summary(mydata$Sex)

    par(mfrow = c(1, 2))
        table_sex <- table(mydata$Sex)
        percents <- round(100*table_sex / sum(table_sex), 1)
        colors <- c("coral1", "cyan1")

        barplot(table_sex, names.arg = c("жени", "мъже"), col = "darkgoldenrod1")

        pie(x = table_sex, main = "Полове", labels = percents, col = colors)
        legend(x = "topleft", legend = c("жени", "мъже"), cex = 1, fill = colors)
    par(mfrow = c(1, 1))

# 2.2. ръст, числова непрекъсната
    par(mfrow = c(1, 2))
        # хистограма
        hist(mydata$Height, main = "вероятностно разпределение", 
            xlab = "ръст в см", ylab = "честота",
            col = "chartreuse1", prob = TRUE)

        # boxplot
        boxplot(mydata$Height, main = "ръст", ylab = "cm", col = "lightskyblue")
    par(mfrow = c(1, 1))

    # qqplot
    set.seed(9504)
    height_normal_distrib <- rnorm(n = 1000, mean = mean(mydata$Height), sd = sd(mydata$Height))

    qqplot(mydata$Height, height_normal_distrib, xlab = "ръст", ylab = "теоретично нормално разпределение")
    abline(a = 0, b = 1)

    # ниво на съгласие
    alpha <- 0.05

    # тест за нормално разпределение
    shapiro.test(mydata$Height) 
        # p-value = 0.08102 > 0.05 = alpha

    # имаме нормално разпределение
    # локация
    mean(mydata$Height)
    # дисперсия
    sd(mydata$Height)

# 2.3. педя, числова непрекъсната
    par(mfrow = c(1, 2))
        # хистограма
        hist(mydata$Handspan, main = "вероятностно разпределение", xlab = "педя в см", ylab = "честота",
        col = "salmon2", prob = TRUE)

        # boxplot
        boxplot(mydata$Handspan, main = "педя", ylab = "cm", col = "slateblue1")
    par(mfrow = c(1, 1))

    # qqplot
    set.seed(734)
    handspan_normal_distrib <- rnorm(n = 1000, mean = mean(mydata$Handspan), sd = sd(mydata$Handspan))

    qqplot(mydata$Handspan, handspan_normal_distrib, main = "педя", ylab = "теоретичното нормално разпределение")
    abline(a = 0, b = 1)

    # ниво на съгласие
    alpha <- 0.05

    shapiro.test(mydata$Handspan) 
    # p-value = 0.003831 < 0.05 = alpha

    # нямаме нормално разпределение
    median(mydata$Handspan)
    # дисперсия
    mad(mydata$Handspan)

# 3. Изследване на взаимодействия между променливите
# 3.1. категорийни обясняващи и числови зависими
    # 3.1.1. пол и ръст
        boxplot(mydata$Height ~ mydata$Sex)

        fem_heights <- mydata$Height[which(mydata$Sex == 'Female')]
        male_heights <- mydata$Height[which(mydata$Sex == 'Male')]

        par(mfrow = c(1, 2))
            hist(fem_heights, main = "", xlab = "ръст на жени в см", ylab = "честота",
                col = "springgreen1", prob = TRUE)

            hist(male_heights, main = "", xlab = "ръст на мъже в см", ylab = "честота",
                col = "springgreen1", prob = TRUE)
        par(mfrow = c(1, 1))

        par(mfrow = c(1, 2))
            boxplot(fem_heights, main = "ръст на жени", ylab = "cm", col = "turquoise1")
            boxplot(male_heights, main = "ръст на мъже", ylab = "cm", col = "turquoise1")
        par(mfrow = c(1, 1))

        shapiro.test(fem_heights)  
            # p-value = 0.1313 > 0.05 = alpha

        shapiro.test(male_heights) 
            # p-value = 0.7162 > 0.05 = alpha

        # женския и мъжкия ръст са нормално разпределени

    # 3.1.2. пол и педя
        boxplot(mydata$Handspan ~ mydata$Sex)

        fem_handspan <- mydata$Handspan[which(mydata$Sex == 'Female')]
        male_handspan <- mydata$Handspan[which(mydata$Sex == 'Male')]

        par(mfrow = c(1, 2))
            hist(fem_handspan, main = "", xlab = "педя на жени в см", ylab = "честота",
                col = "olivedrab2", prob = TRUE)

            hist(male_handspan, main = "", xlab = "педя на мъже в см", ylab = "честота",
                col = "olivedrab2", prob = TRUE)
        par(mfrow = c(1, 1))

        par(mfrow = c(1, 2))
            boxplot(fem_handspan, main = "педя на жени", ylab = "cm", col = "orchid1")
            boxplot(male_handspan, main = "педя на мъже", ylab = "cm", col = "orchid1")
        par(mfrow = c(1, 1))


        shapiro.test(fem_handspan) 
            # p-value = 0.002367 < 0.05 = alpha

        shapiro.test(male_handspan) 
            # p-value = 0.06273 > 0.05 = alpha

        # педята на жените не е нормално разпределена, а мъжката е 

# 3.2. числови обясняващи и числови зависими

    # correlation
        # pearson -> in normal distribution
        # spearman
        # kendall

    plot(mydata$Handspan, mydata$Height)

    rho <- round(cor(mydata$Height, mydata$Handspan, method = "pearson"), 3)

    plot(fem_handspan, fem_heights)
    plot(male_handspan, male_heights)

    rho_females <- round(cor(fem_handspan, fem_heights, method = "spearman"), digits = 3)
    rho_males <- round(cor(male_handspan, male_heights, method = "pearson"), digits = 3)

    abs(rho) # 0.602
    abs(rho_females) # 0.341
    abs(rho_males) # 0.385







