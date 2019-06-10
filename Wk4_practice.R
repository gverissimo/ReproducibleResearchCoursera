## clear the console
## click on the console and type ctrl-l

## clear the workspace
ls() # show workspace
rm(list=ls()) # clear workspace
ls() # show empty workspace


## ******************************** 

library(devtools)
install_git("http://github.com/rdpeng/cacher")
# or
install_github("rdpeng/cacher")

library(cacher)
clonecache(id="092dcc7dda4b93e42f23e42f23e038a60e1d44dbec7b3f")
showfiles()
sourcefile

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
library(datasets)
library(stats)

## load the dataset
data(airquality)

## fit a linear model using lm()
fit <- lm(Ozone ~ Wind + Temp + Solar.R, data=airquality)
summary(fit)

## plot some diagnostics
par(mfrow=c(2,2))
plot(fit)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
## Case Study : Air Pollution

test <- as.POSIXct("4/18/1950 0130 CST", "%m/%d/%Y %H%M %Z")


