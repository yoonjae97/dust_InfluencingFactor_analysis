MonthTemp <- read.csv(file="19812019TempMonth.csv", encoding="CP949")
str(MonthTemp)

MonthTemp.lm <- lm(MonthAvgTemp ~ 癤풷ear, data=MonthTemp)

class(MonthTemp.lm)
MonthTemp.lm

plot(MonthTemp$MonthAvgTemp ~ MonthTemp$癤풷ear,
     col='cornflowerblue', pch=19,
     xlab='year', ylab='Temperature')
abline(MonthTemp.lm, col="salmon", lwd=2)

summary(MonthTemp.lm)

anova(MonthTemp.lm)



















