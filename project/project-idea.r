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
write.csv(mydata, 'D:\\surveyData.csv', row.names = FALSE)

# exercise, age, pulse
survey_observations <- dim(survey)[1]
survey_observations_ <- dim(mydata)[1]


# 0. Анализ на една променлива
    # Sex (категорийна)
        # summary
        summary(mydata$Sex)

        # barplot
        table_sex <- table(mydata$Sex)
        barplot(height = prop.table(table(mydata$Sex)), col = "cadetblue1")

        # piechart
        percents <- round(100*table_sex / sum(table_sex), 1)
        colors <- c("coral1", "cyan1")

        pie(x = table_sex, main = "Полове", labels = percents, col = colors)
        legend(x = "topleft", legend = c("мъже", "жени"), cex = 1, fill = colors)

        # резултатите показват, че разпределението е поравно между момичетата и момчетата

    # Height (числова непрекъсната)
        # summary
        summary(mydata$Height)

        # hist
        hist(mydata$Height, main = "честотно разпределение", xlab = "ръст в см", ylab = "брой", col = "chartreuse1")
        hist(mydata$Height, main = "вероятностно разпределение", xlab = "ръст в см", ylab = "честота",
            col = "chartreuse1", prob = T)

        # boxplot
        boxplot(mydata$Height, main = "ръст", ylab = "cm", col = "lightskyblue")

        # qqplot
        set.seed(9504)
        height_normal_distrib <- rnorm(n = 1000, mean = mean(mydata$Height), sd = sd(mydata$Height))
        qqplot(mydata$Height, height_normal_distrib, main = "ръст", ylab = "теоретично нормално разпределение")
        abline(a = 0, b = 1)

        # tests
        alpha <- 0.05

        # for normal distribution
        shapiro.test(mydata$Height)

        fem_heights <- mydata$Height[which(mydata$Sex == 'Female')]
        male_heights <- mydata$Height[which(mydata$Sex == 'Male')]

        shapiro.test(fem_heights)
        shapiro.test(male_heights)

        t.test(fem_heights, male_heights)
        hist(fem_heights)
        hist(male_heights)

    # Handspan (числова непрекъсната)
        # summary
        summary(mydata$Handspan)
        hist(mydata$Handspan, main = "честотно разпределение", xlab = "педя в см", ylab = "брой", col = "chartreuse1")
        hist(mydata$Handspan, main = "вероятностно разпределение", xlab = "педя в см", ylab = "честота",
            col = "chartreuse1", prob = T)

        # boxplot
        boxplot(mydata$Handspan, main = "педя", ylab = "cm", col = "lightskyblue")

        # qqplot
        set.seed(9504)
        handspan_normal_distrib <- rnorm(n = 1000, mean = mean(mydata$Handspan), sd = sd(mydata$Handspan))
        qqplot(mydata$Handspan, handspan_normal_distrib, main = "педя", ylab = "теоретично нормално разпределение")
        abline(a = 0, b = 1)

        # tests
        alpha <- 0.05

        # for normal distribution
        shapiro.test(mydata$Handspan)

        fem_handspan <- mydata$Handspan[which(mydata$Sex == 'Female')]
        male_handspan <- mydata$Handspan[which(mydata$Sex == 'Male')]

        shapiro.test(fem_handspan)
        shapiro.test(male_handspan)

        t.test(fem_handspan, male_handspan)
        hist(fem_handspan)
        hist(male_handspan)

# 2. Категорийни (обясняващи) VS числови (зависими)

boxplot(mydata$Height ~ mydata$Sex)
boxplot(mydata$Handspan ~ mydata$Sex)

boxplot(mydata$Pulse ~ mydata$Exer)
boxplot(mydata$Pulse ~ mydata$Smoke)


# 4. Числови (обясняващи) VS числови (зависими)

plot(mydata$Height, mydata$Handspan)

plot(mydata$Height, mydata$Pulse)
plot(mydata$Age, mydata$Pulse)

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






