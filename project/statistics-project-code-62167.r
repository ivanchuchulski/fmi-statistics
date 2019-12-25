

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
    # 2.1. пол на анкетираните, категорийна номинална
    summary(mydata$Sex)
    
    par(mfrow = c(1, 2))

        table_sex <- table(mydata$Sex)
        percents <- round(100*table_sex / sum(table_sex), 1)
        colors <- c("coral1", "cyan1")

        barplot(table_sex, names.arg = c("жени", "мъже"), col = "darkgoldenrod1")

        pie(x = table_sex, main = "Полове", labels = percents, col = colors)
        legend(x = "topleft", legend = c("жени", "мъже"), cex = 1, fill = colors)

    par(mfrow = c(1, 1))











