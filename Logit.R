library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(Hmisc)
library(pROC)
library(ROCR)
library(scales)

setwd("C:/Users/simon/Desktop/r codes")
data <- read.csv("dly518.csv", header = TRUE, sep = ",")
data

str(data)
data$ind <- as.factor(data$ind)
data$hm <- as.numeric(data$hm)
data$ddhm <- as.numeric(data$ddhm)
data$hg <- as.numeric(data$hg)

str(data)
data <- data[,-c(15,16,17)]
sapply(data, function(x) sum(is.na(x)))

data1 <- na.omit(data)
data1
summary(data1)

correlations <- cor(data1[,-c(1,2,3,7,8,12,13,14)])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
)

data2 <-data1[,-c(2,3,7,8,12,13,14)]

boxplot(data2[,-c(1)])

fun <- function(x){
  quantiles <- quantile( x, c(.25, .75 ) )
  x[which(x<quantiles[1])] <- median(x)
  x[which(x>quantiles[2])] <- median(x)
  x
}

as.data.frame(sapply(data2[,-c(1)], fun))

boxplot(data2[,-c(1)])

model <- glm(ind ~ . , data=data2, family='binomial')
summary(model)

names(data2)
model <- glm(ind ~ ind+gmin+hg+sun , data=data2, family='binomial')
summary(model)

plot(model, pch =18, col = "red", which = c(4))

vif(model)

# R square (nagelkarke)
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
R2.hl<-modelChi/model$null.deviance
R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data2))
R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data2))))))
R.n

# Predicted Probabilities
prediction <- predict(model,newdata = data2,type="response")
rocCurve   <- roc(response = data2$ind, predictor = prediction, 
                  levels = levels(data2$ind))

#Metrics - Fit Statistics
predclass <-ifelse(prediction>0.5,1,0)
Confusion <- table(Predicted = predclass,Actual = data2$ind)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
Confusion 

plot(rocCurve)


