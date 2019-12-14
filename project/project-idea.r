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
        normal_distrib <- rnorm(n = 10^2, mean = mean(mydata$Height), sd = sd(mydata$Height))
        qqplot(mydata$Height, normal_distrib, main = "ръст", ylab = "теоретично разпределение")
        abline(a = 0, b = 1)

        # tests
        alpha <- 0.05

        # for normal distribution
        shapiro.test(mydata$Height)

        females <- mydata$Height[which(mydata$Sex == 'Female')]
        males <- mydata$Height[which(mydata$Sex == 'Male')]

        shapiro.test(females)
        shapiro.test(males)

        t.test(females, males)
        hist(females)
        hist(males)

    # Wr.Hnd (числова непрекъсната)
        # summary
        summary(mydata$Wr.Hnd)
        # hist
        # boxplot
        # qqplot

# 2. Категорийни (обясняващи) VS числови (зависими)

boxplot(mydata$Height ~ mydata$Sex)
boxplot(mydata$Wr.Hnd ~ mydata$Sex)

boxplot(mydata$Pulse ~ mydata$Exer)
boxplot(mydata$Pulse ~ mydata$Smoke)


# 4. Числови (обясняващи) VS числови (зависими)

plot(mydata$Height, mydata$Wr.Hnd)

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

# ################
# numbered variables
    # pointsPerGame
    # reboundsPerGame
# category variables
    # "guard", "forward", "center"

playerNames <- c("Klay Thompson", "Kyrie Irving", "James Harden", "Kemba Walker",
            "Khris Middleton", "Jimmy Butler", "Gordon Hayward", "Jayson Tatum",
             "Dwight Howard", "Nicola Jokic", "Nikola Vucevic", "Andre Drummond")

pointsPerGame <- c(19.5, 22.4, 24.6, 21.0,
                    15.7, 16.7, 15.2, 15.2,
                    17.2, 16.3, 15.7, 14.3)

reboundsPerGame <- c(3.5, 3.7, 5.2, 3.9, 
                       4.5, 4.9, 4.3, 5.6,
                        12.6, 9.5, 10.2, 13.8)

fieldPositions <- c("guard", "guard", "guard", "guard", 
                    "forward", "forward", "forward", "forward", 
                    "center", "center", "center", "center")

basketball <- data.frame(playerNames, pointsPerGame, reboundsPerGame, fieldPositions)

boxplot(basketball$pointsPerGame[fieldPositions ==  "guard"], 
        basketball$pointsPerGame[fieldPositions == "forward"], 
        basketball$pointsPerGame[fieldPositions == "center"], 
        col = rainbow(3), 
        names = c("guard pts", "forward pts", "forward pts"))

boxplot(basketball$reboundsPerGame[fieldPositions ==  "guard"], 
        basketball$reboundsPerGame[fieldPositions == "forward"], 
        basketball$reboundsPerGame[fieldPositions == "center"], 
        col = rainbow(3), 
        names = c("guard rebs", "forward rebs", "forward rebs"))

hist(x = basketball$pointsPerGame, col = "green", xlab = "ppg")






