Element <- read.csv(file="20082019연도별대기조성자료.csv", encoding="CP949")
str(Element)

Element <- Element[2:9]
library(car)
scatterplotMatrix(Element, pch=19, col='royalblue', cex=1.2,
                  regLine=list(method=lm, lty=1, col='salmon'),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth='forestgreen'))

Element.lm <- lm(Temp ~ CO2 + CH4 + PM10 + SF6 + N2O, data=Element)
summary(Element.lm)

install.packages("stargazer")
library(stargazer)
stargazer(Element.lm, type='text', no.space=TRUE)

Element.lm <- lm(scale(Temp) ~ scale(CO2) + scale(CH4) + scale(PM10) + scale(SF6) + scale(N2O),
                  data=Element)
summary(Element.lm)

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(Element.lm)

Element.lm <- lm(Temp ~ CO2 + CH4 + PM10 + SF6 + N2O, data=Element)
plot(Element.lm)

install.packages("car")
library(car)
vif(Element.lm)
vif(Element.lm) > 4
vif(Element.lm) > 10


summary(powerTransform(Element$Temp))
Element.lm1 <- lm(Temp ~ CO2 + CH4, data=Element)
Element.lm2 <- lm(Temp ~ CO2 + CH4 + PM10 + SF6 + N2O, data=Element)

str(Element.lm1)
str(Element.lm2)

anova(Element.lm1, Element.lm2)
AIC(Element.lm1, Element.lm2)

Element.lm <- lm(Temp ~ CO2 + CH4 + PM10 + SF6 + N2O , data=Element)
step(Element.lm, direction="backward")

install.packages("leaps")
library(leaps)
Element.regsubsets <- regsubsets(x=Temp ~ CO2 + CH4 + PM10 + SF6 + N2O, data=Element, nbest=4)

install.packages("RColorBrewer")
library(RColorBrewer)
plot(Element.regsubsets, scale="adjr2", col=brewer.pal(9, "Pastel1"))

names(summary(Element.regsubsets))

summary(Element.regsubsets)$adjr2
which.max(summary(Element.regsubsets)$adjr2)
coef(Element.regsubsets,5)

install.packages("caret")
library(caret)
set.seed(123)
train <- createDataPartition(y=Element$Temp, p=0.7, list=FALSE)
Element.train <- Element[train,]
Element.test <- Element[-train,]

install.packages("glmnet")
library(glmnet)

x <- model.matrix(Temp ~ ., Element.train)[,-1]
y <- Element.train$Temp
#error
Element.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0)
plot(Element.cv)
Element.cv$lambda.min
log(Element.cv$lambda.min)

Element.gnet <- glmnet(x,y, family="gaussian", alpha=0, lambda = Element.cv$lambda.min)
coef(Element.gnet)

#error
Element.test.x <- model.matrix(Temp ~ ., Element.test)[,-1]
Element.pred <- predict(Element.gnet, newx=Element.test.x)

postResample(pred=Element.pred, obs=Element.test$Temp)

