# TODO 
# 
# 


# all columns in dataframe survey
# Sex, Wr.Hnd, NW.Hnd, W.Hnd, Fold, Pulse, Clap, Exer, Smoke, Height, M.I, Age

# wanted columns
# Sex(cat), Wr.Hnd(num cont), Pulse(num cont), Exer(cat), Smoke(cat), Height(num cont), Age(num cont)
 

# from the package MASS we are using the dataset survey
install.packages("MASS")

library("MASS") # or require("MASS")

# getting the wanted columns and removing NA values
columns_to_keep <- c("Sex", "Wr.Hnd", "Height")
mydata <- survey[columns_to_keep]

mydata <- subset(mydata, !is.na(mydata$Sex) & 
                        !is.na(mydata$Wr.Hnd) & 
                        !is.na(mydata$Height))

# rename column
colnames(mydata)[2] <- "Handspan"

# to write to csv file
write.csv(mydata, 'D:\\survey_data.csv', row.names = FALSE)

survey_observations <- dim(survey)[1]
survey_observations_ <- dim(mydata)[1]

# 0. Анализ на една променлива
    # Sex (категорийна номинална)
        # summary
        summary(mydata$Sex)

        par(mfrow = c(1, 2))

            table_sex <- table(mydata$Sex)
            percents <- round(100*table_sex / sum(table_sex), 1)
            colors <- c("coral1", "cyan1")

            barplot(table_sex, names.arg = c("жени", "мъже"), col = "darkgoldenrod1")

            pie(x = table_sex, main = "Полове", labels = percents, col = colors)
            legend(x = "topleft", legend = c("жени", "мъже"), cex = 1, fill = colors)

        par(mfrow = c(1, 1))
     
        # резултатите показват, че разпределението е поравно между момичетата и момчетата

    # Height (числова непрекъсната)
        # summary
        summary(mydata$Height)

        # hist
        hist(mydata$Height, main = "вероятностно разпределение", 
            xlab = "ръст в см", ylab = "честота",
            col = "chartreuse1", prob = TRUE)

        # boxplot
        boxplot(mydata$Height, main = "ръст", ylab = "cm", col = "lightskyblue")

        # qqplot
        set.seed(9504)
        height_normal_distrib <- rnorm(n = 1000, mean = mean(mydata$Height), sd = sd(mydata$Height))
        qqplot(mydata$Height, height_normal_distrib, xlab = "ръст", ylab = "теоретично нормално разпределение")
        abline(a = 0, b = 1)

        # tests
        alpha <- 0.05

        # for normal distribution
        shapiro.test(mydata$Height) # 0.08102

        fem_heights <- mydata$Height[which(mydata$Sex == 'Female')]
        male_heights <- mydata$Height[which(mydata$Sex == 'Male')]

        shapiro.test(fem_heights)  # 0.1313

        shapiro.test(male_heights) # 0.7162

        t.test(fem_heights, male_heights)

        par(mfrow = c(1, 2))
        hist(fem_heights, main = "вероятностно разпределение", xlab = "ръст на жени в см", ylab = "честота",
            col = "chartreuse1", prob = TRUE)

        hist(male_heights, main = "вероятностно разпределение", xlab = "ръст на мъже в см", ylab = "честота",
            col = "chartreuse1", prob = TRUE)
        par(mfrow = c(1, 1))

        # имаме нормално разпределение
        # локация
        mean(mydata$Height)
        # дисперсия
        sd(mydata$Height)


    # Handspan (числова непрекъсната)
        # summary
        summary(mydata$Handspan)

        # histogram
        hist(mydata$Handspan, main = "вероятностно разпределение", xlab = "педя в см", ylab = "честота",
            col = "salmon2", prob = TRUE)

        # boxplot
        boxplot(mydata$Handspan, main = "педя", ylab = "cm", col = "slateblue1")

        # qqplot
        set.seed(734)
        handspan_normal_distrib <- rnorm(n = 500, mean = mean(mydata$Handspan), sd = sd(mydata$Handspan))
        qqplot(mydata$Handspan, handspan_normal_distrib, main = "педя", ylab = "теоретичното нормално разпределение")
        abline(a = 0, b = 1)

        # tests
        alpha <- 0.05

        # for normal distribution
        shapiro.test(mydata$Handspan) # 0.003831

        fem_handspan <- mydata$Handspan[which(mydata$Sex == 'Female')]
        male_handspan <- mydata$Handspan[which(mydata$Sex == 'Male')]

        shapiro.test(fem_handspan) # 0.002367

        shapiro.test(male_handspan) # 0.06273

        t.test(fem_handspan, male_handspan)

        hist(fem_handspan, main = "вероятностно разпределение", 
            xlab = "педя на жени в см", ylab = "честота",
            col = "chartreuse1", prob = TRUE)

        hist(male_handspan, main = "вероятностно разпределение", 
            xlab = "педя на мъже в см", ylab = "честота",
            col = "chartreuse1", prob = TRUE)

        # нямаме нормално разпределение
        median(mydata$Handspan)
        # дисперсия
        mad(mydata$Handspan)

# 2. Категорийни (обясняващи) VS числови (зависими)
        
    boxplot(mydata$Height ~ mydata$Sex)
    boxplot(mydata$Handspan ~ mydata$Sex)

# 4. Числови (обясняващи) VS числови (зависими)

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


##################

# 1. Категорийни (обясняващи) VS категорийни (зависими)
barplot(prop.table(x = table(mydata$Exer, mydata$Sex), margin = 2), legend.text = T)

barplot(prop.table(x = table(survey$Exer, survey$Sex), margin = 2), legend.text = T)
barplot(prop.table(x = table(survey$Sex, survey$Exer), margin = 2), legend.text = T)


# old ideas #######################
# have to remove na
sexes <- survey$Sex
heights <- survey$Height
handWidth <- survey$Wr.Hnd

hist(heights)
mean(heights, na.rm = T)
sd(heights, na.rm = T)

hist(handWidth)
mean(handWidth, na.rm = T)
sd(handWidth, na.rm = T)

students <- data.frame(sexes, heights, handWidth)

t1 <- boxplot(students$heights ~ students$sexes)
t2 <- boxplot(students$handWidth ~ students$sexes)






