library(readxl)
library(gains)
library(caret)
library(ROCR)
library(FNN)
library(caret)
library(e1071)
library(plyr)
library(groupdata2)
library(dplyr)
library(randomForest)
library(rpart) 
library(rpart.plot)

final <- c("occ", "loanamt",	"msa",	"appinc",	"unit",	"married",	"dep",	"emp",
           "self",	"hexp", "price",	"other",	"liq",	"gdlin",	"mortg",	"pubrec",	
           "hratAmt",	"obratAmt",	"fixadj", "term",	"cosign",	"netw",	"sch",	"hispan",	
           "male",	"approve",	"mortno",	"mortperf",	"chist",	"multi")


loan <- read.csv("loan_approve_project.csv")
loan <- loan[loan$gdlin <= 1, final]
View(loan)

# Factors
cols <- c("married","self","gdlin","pubrec","fixadj","cosign","sch",
          "hispan","male","approve","mortno","mortperf","chist","multi")
loan[cols] <- lapply(loan[cols], factor)

#Random Forest
set.seed(1)
numberOfRows <- nrow(loan)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- loan[train.index,]
valid.df <- loan[-train.index,]

rfmodel  <- randomForest(approve ~.,data = train.df, importance = T)
varImpPlot(rfmodel, type=1)
rfmodel

rf.pred <- predict(rfmodel,valid.df,type="class")
summary(rf.pred)

confusionMatrix(rf.pred, as.factor(valid.df$approve))
summary(rfmodel)
accuracy(rfmodel, loan$approve)

#With upsampling
loan <- read.csv("loan_approve_project.csv")
loan <- loan[loan$gdlin <= 1, final]
View(loan)
loan[cols] <- lapply(loan[cols], factor)
Balanced <- upsample(loan, cat_col = "approve")
View(Balanced)

set.seed(1)
numberOfRows <- nrow(Balanced)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Balanced[train.index,]
valid.df <- Balanced[-train.index,]

rfmodel  <- randomForest(approve ~.,data = train.df, importance = T)
varImpPlot(rfmodel, type=1)
rfmodel

rf.pred <- predict(rfmodel,valid.df,type="class")
summary(rf.pred)

confusionMatrix(rf.pred, as.factor(valid.df$approve))

rfmodel
