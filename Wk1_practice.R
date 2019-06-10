## clear the console
## click on the console and type ctrl-l

## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace


## ******************************** 
library(kernlab)

data(spam)

str(spam[, 1:5])
## 'data.frame':	4601 obs. of  5 variables:
##         $ make   : num  0 0.21 0.06 0 0 0 0 0 0.15 0.06 ...
## $ address: num  0.64 0.28 0 0 0 0 0 0 0 0.12 ...
## $ all    : num  0.64 0.5 0.71 0 0 0 0 0 0.46 0.77 ...
## $ num3d  : num  0 0 0 0 0 0 0 0 0 0 ...
## $ our    : num  0.32 0.14 1.23 0.63 0.63 1.85 1.92 1.88 0.61 0.19 ...

## Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)

table(trainIndicator)
## trainIndicator 
## 0 1
## 2314 2287

trainSpam = spam[trainIndicator == 1, ] 
testSpam = spam[trainIndicator == 0, ]

names(trainSpam)
head(trainSpam)

## Summaries
table(trainSpam$type)
##
## nonspam spam 
## 1381 906

## Plots
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

## Relationships between predictors
plot(log10(trainSpam[, 1:4] + 1))

## Clustering
hCluster = hclust(dist(t(trainSpam[, 1:57]))) 
plot(hCluster)

## New clustering
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1)))) 
plot(hClusterUpdated)

## Statistical prediction/modeling
trainSpam$numType = as.numeric(trainSpam$type) - 1 
costFunction = function(x, y) 
sum(x != (y > 0.5)) 
cvError = rep(NA, 55)

library(boot)
for (i in 1:55) {
        lmFormula = reformulate(names(trainSpam)[i], response = "numType") 
        glmFit = glm(lmFormula, family = "binomial", data = trainSpam) 
        cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2] 
}

## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
## [1] "charDollar"

## Get a measure of uncertainty
## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam) 
predictedSpam = rep("nonspam", dim(testSpam)[1])

## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification table
table(predictedSpam, testSpam$type)
## predictedSpam nonspam spam
## nonspam 1346 458
## spam 61 449

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)
## [1] 0.2243


