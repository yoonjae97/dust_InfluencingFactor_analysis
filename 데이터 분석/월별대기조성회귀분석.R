Elements <- read.csv(file="20082019월평균대기조성자료.csv", encoding="CP949")
str(Elements)

Elements <- Elements[2:5]
library(car)
scatterplotMatrix(Elements, pch=19, col='royalblue', cex=1.2,
                  regLine=list(method=lm, lty=1, col='salmon'),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth='forestgreen'))

Elements.lm <- lm(Temp ~ CO2 + CH4 + UVA, data=Elements)
summary(Elements.lm)

install.packages("stargazer")
library(stargazer)
stargazer(Elements.lm, type='text', no.space=TRUE)

Elements.lm <- lm(scale(Temp) ~ scale(CO2) + scale(CH4) + scale(UVA),
                  data=Elements)
summary(Elements.lm)

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(Elements.lm)

Elements.lm <- lm(Temp ~ CO2 + CH4 + UVA, data=Elements)
plot(Elements.lm)

install.packages("car")
library(car)
vif(Elements.lm)
vif(Elements.lm) > 4
vif(Elements.lm) > 10

#error
summary(powerTransform(Elements$Temp))
Elements.lm1 <- lm(Temp ~ CO2 + CH4, data=Elements)
Elements.lm2 <- lm(Temp ~ CO2 + CH4 + UVA, data=Elements)

str(Elements.lm1)
str(Elements.lm2)

anova(Elements.lm1, Elements.lm2)
AIC(Elements.lm1, Elements.lm2)

Elements.lm <- lm(Temp ~ CO2 + CH4 + UVA , data=Elements)
step(Elements.lm, direction="backward")

install.packages("leaps")
library(leaps)
Elements.regsubsets <- regsubsets(x=Temp ~ CO2 + CH4 + UVA, data=Elements, nbest=4)

install.packages("RColorBrewer")
library(RColorBrewer)
plot(Elements.regsubsets, scale="adjr2", col=brewer.pal(9, "Pastel1"))

names(summary(Elements.regsubsets))

summary(Elements.regsubsets)$adjr2
which.max(summary(Elements.regsubsets)$adjr2)
coef(Elements.regsubsets,7)

install.packages("caret")
library(caret)
set.seed(123)
train <- createDataPartition(y=Elements$Temp, p=0.7, list=FALSE)
Elements.train <- Elements[train,]
Elements.test <- Elements[-train,]

install.packages("glmnet")
library(glmnet)

x <- model.matrix(Temp ~ ., Elements.train)[,-1]
y <- Elements.train$Temp

Elements.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0)
plot(Elements.cv)
Elements.cv$lambda.min
log(Elements.cv$lambda.min)

Elements.gnet <- glmnet(x,y, family="gaussian", alpha=0, lambda = Elements.cv$lambda.min)
coef(Elements.gnet)

Elements.test.x <- model.matrix(Temp ~ ., Elements.test)[,-1]
Elements.pred <- predict(Elements.gnet, newx=Elements.test.x)

postResample(pred=Elements.pred, obs=Elements.test$Temp)



