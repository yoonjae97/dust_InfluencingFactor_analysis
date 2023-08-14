RA2 <- read.csv(file="RA2.csv", encoding="CP949")
str(RA2)
RA2 <- RA2[2:8]
library(car)
scatterplotMatrix(RA2, mtcars, pch=19, col="royalblue", cex=1.2,
                  regLine=list(method=lm, lty=1, lwd=3, col="salmon"),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth="forestgreen"))


RA2.lm <- lm(PM2.5 ~ Temp + SO2 + CO + O3 + NO2 + Prec,data=RA2)
summary(RA2.lm)
library(stargazer)
stargazer(RA2.lm, type="text", no.space=TRUE)

RA2.lm <- lm(scale(PM2.5) ~ scale(Temp) + scale(SO2) + scale(CO) + scale(O3) + scale(NO2) + scale(Prec),data=RA2)
summary(RA2.lm)
lm.beta(RA2.lm)

RA2.lm <- lm(PM2.5 ~ Temp + SO2 + CO + O3 + NO2 + Prec,data=RA2)
plot(RA2.lm)

library(car)
vif(RA2.lm)
vif(RA2.lm) > 4
vif(RA2.lm) > 10

RA2.lm <- lm(PM2.5 ~ Temp + SO2 + O3 + NO2 + Prec,data=RA2)
summary(RA2.lm)

library(car)
summary(powerTransform(RA2$PM2.5))


library(car)
spreadLevelPlot(lm(PM2.5 ~ Temp + SO2 + O3 + NO2 + Prec, data=RA2))

RA2.lm1 <- lm(PM2.5 ~ Temp + SO2 + O3 + NO2 + Prec, data=RA2)
RA2.lm2 <- lm(PM2.5 ~ Temp + SO2 + CO + O3 + NO2 + Prec, data=RA2)
anova(RA2.lm1, RA2.lm2)
AIC(RA2.lm1, RA2.lm2)

RA2.lm <- lm(PM2.5 ~ Temp + SO2 + CO + O3 + NO2 + Prec, data=RA2)
step(RA2.lm, direction="backward")

install.packages("leaps")
library(leaps)
RA2.regsubsets <- regsubsets(x=PM2.5 ~ Temp + SO2 + CO + O3 + NO2 + Prec, data=RA2, nbest=4 )

install.packages("RColorBrewer")
library(RColorBrewer)
plot(RA2.regsubsets, scale="adjr2", col=brewer.pal(9, "Pastel1"))

names(summary(RA2.regsubsets))
summary(RA2.regsubsets)$adjr2  
which.max(summary(RA2.regsubsets)$adjr2)
coef(RA2.regsubsets,21)

library(caret)
set.seed(123)
train <- createDataPartition(y=RA2$PM2.5, p=0.7, list=FALSE)
RA2.train <- RA2[train,]
RA2.test <- RA2[-train,]

x <- model.matrix(PM2.5 ~., RA2.train)[,-1]
y <- RA2.train$PM2.5

#Ridge
library(glmnet)
set.seed(123)
RA2.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0)
plot(RA2.cv)

RA2.cv$lambda.min
log(RA2.cv$lambda.min)

RA2.gnet <- glmnet(x, y, family="gaussian", alpha=0, lambda=RA2.cv$lambda.min)
coef(RA2.gnet)

RA2.test.x <- model.matrix(PM2.5 ~., RA2.test)[,-1]
RA2.pred <- predict(RA2.gnet, newx=RA2.test.x)

postResample(pred=RA2.pred, obs=RA2.test$PM2.5)

#lasso
set.seed(123)
RA2.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=1)
RA2.cv$lambda.min
log(RA2.cv$lambda.min)

plot(RA2.cv)

RA2.cv$lambda.1se
log(RA2.cv$lambda.1se)
coef(RA2.cv, RA2.cv$lambda.min)
coef(RA2.cv, RA2.cv$lambda.1se)

RA2.gnet1 <- glmnet(x, y, family="gaussian",
                    alpha=1, lambda=RA2.cv$lambda.min)
RA2.pred1 <- predict(RA2.gnet1, newx=RA2.test.x)
postResample(pred=RA2.pred1, obs=RA2.test$PM2.5)

RA2.gnet2 <- glmnet(x, y, family="gaussian",
                    alpha=1, lambda=RA2.cv$lambda.1se)
RA2.pred2 <- predict(RA2.gnet2, newx=RA2.test.x)
postResample(pred=RA2.pred2, obs=RA2.test$PM2.5)

#elastic
library(caret)
set.seed(123)
RA2.cv <-train(form=PM2.5 ~., data=RA2.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneLength=10)

RA2.cv$bestTune
RA2.gnet <- glmnet(x, y, family="gaussian",
                   alpha=RA2.cv$bestTune$alpha,
                   lambda=RA2.cv$bestTune$lambda)
coef(RA2.gnet)
RA2.pred <- predict(RA2.gnet, newx=RA2.test.x)
postResample(pred=RA2.pred, obs=RA2.test$PM2.5)

#comparison
library(caret)
lambda <- 10^seq(-5, 5, length=100)

set.seed(123)
ridge <- train(PM2.5 ~., data=RA2.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=0, lambda=lambda))
coef(ridge$finalModel, ridge$bestTune$lambda)
ridge.pred <- predict(ridge, RA2.test)
postResample(pred=ridge.pred, obs=RA2.test$PM2.5)

set.seed(123)
lasso <- train(PM2.5 ~., data=RA2.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=1, lambda=lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, RA2.test)
postResample(pred=lasso.pred, obs=RA2.test$PM2.5)

set.seed(123)
elastic <- train(PM2.5 ~., data=RA2.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneLength=10)
coef(elastic$finalModel, elastic$bestTune$lambda)
elastic.pred <- predict(elastic, RA2.test)
postResample(pred=elastic.pred, obs=RA2.test$PM2.5)

models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models), metric="RMSE")

summary(diff(resamples(models), metric="RMSE"))
