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
columns_to_keep <- c("Sex", "Wr.Hnd", "Pulse", "Exer", "Smoke", "Height", "Age")

modified_data <- survey[columns_to_keep]

modified_data <- subset(modified_data, !is.na(modified_data$Sex) & 
                                    !is.na(modified_data$Wr.Hnd) & 
                                    !is.na(modified_data$Pulse) & 
                                    !is.na(modified_data$Exer) & 
                                    !is.na(modified_data$Smoke) & 
                                    !is.na(modified_data$Height) & 
                                    !is.na(modified_data$Age)) 
# maybe round Age?

# to write to csv file
write.csv(modified_data, 'D:\\surveyData.csv', row.names = FALSE)

# exercise, age, pulse
survey_observations <- dim(survey)[1]
survey_observations_ <- dim(modified_data)[1]


# 0. Анализ на една променлива
    # Sex (категорийна)
        # summary
        summary(modified_data$Sex)

        # barplot
        table_sex <- table(modified_data$Sex)
        barplot(height = prop.table(table(modified_data$Sex)), col = "cadetblue1")

        # piechart
        percents <- round(100*table_sex/sum(table_sex), 1)
        colors <- rainbow(n = length(table_sex)) 
        # fix colors, provide custom ones

        pie(x = table_sex, main = "Полове", labels = percents, col = colors)
        legend(x = "topleft", legend = c("мъже", "жени"), cex = 1, fill = colors)

        # резултатите показват, че разпределението е поравно между момичетата и момчетата

    # Height (числова непрекъсната)
        # summary
        summary(modified_data$Height)

        # hist
        hist(modified_data$Height, main = "честотно разпределение", xlab = "височина", ylab = "брой", col = "chartreuse1")
        hist(modified_data$Height, main = "вероятностно разпределение", xlab = "височина", ylab = "честота",
            col = "chartreuse1", prob = T)

        # boxplot
        

        # qqplot

    # Wr.Hnd (числова непрекъсната)
        # summary
        summary(modified_data$Wr.Hnd)
        # hist
        # boxplot
        # qqplot

# 2. Категорийни (обясняващи) VS числови (зависими)

boxplot(modified_data$Height ~ modified_data$Sex)
boxplot(modified_data$Wr.Hnd ~ modified_data$Sex)

boxplot(modified_data$Pulse ~ modified_data$Exer)
boxplot(modified_data$Pulse ~ modified_data$Smoke)


# 4. Числови (обясняващи) VS числови (зависими)
plot(modified_data$Height, modified_data$Pulse)

plot(modified_data$Age, modified_data$Pulse)

##################

# 1. Категорийни (обясняващи) VS категорийни (зависими)
barplot(prop.table(x = table(modified_data$Exer, modified_data$Sex), margin = 2), legend.text = T)

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






