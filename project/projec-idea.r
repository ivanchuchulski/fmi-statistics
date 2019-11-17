# TODO 
# 
# 

# from the package MASS we are using the dataset survey
install.packages("MASS")
library("MASS") # or require("MASS")


# 0. Анализ на една променлива


# exercise, age, pulse
# !!
observationCount <- dim(survey)[1]
no_na_survey <- subset(survey, !is.na(survey$Sex) & !is.na(survey$Wr.Hnd) & !is.na(survey$Pulse) & 
                                !is.na(survey$Exer) & !is.na(survey$Smoke) & !is.na(survey$Height)) 
 
# 1. Категорийни (обясняващи) VS категорийни (зависими)
barplot(prop.table(x = table(no_na_survey$Exer, no_na_survey$Sex), margin = 2), legend.text = T)
barplot(prop.table(x = table(survey$Sex, survey$Exer), margin = 2), legend.text = T)

barplot(prop.table(x = table(survey$Exer, survey$Sex), margin = 2), legend.text = T)
barplot(prop.table(x = table(survey$Sex, survey$Exer), margin = 2), legend.text = T)


# 2. Категорийни (обясняващи) VS числови (зависими)
#       sex         height or Wr.Hnd
boxplot(no_na_survey$Height ~ no_na_survey$Sex)
boxplot(no_na_survey$Wr.Hnd ~ no_na_survey$Sex)

boxplot(survey$Height ~ survey$Sex)
boxplot(survey$Wr.Hnd ~ survey$Sex)


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






