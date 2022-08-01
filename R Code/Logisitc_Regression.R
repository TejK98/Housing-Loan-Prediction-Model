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

#Logistic Regression on all the columns
set.seed(1)
numberOfRows <- nrow(loan)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- loan[train.index,]
valid.df <- loan[-train.index,]

logitI.reg <- glm(approve ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logitI.reg)

confusionMatrix(table(predict(logitI.reg, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$approve == 1))

logitI.reg
mean(logitI.reg$residuals^2)
#Logistic Regression using following columns 
sel <- c("loanamt", "married", "self", "price", "gdlin", "other", "pubrec", "obratAmt","approve","hispan")
loan <- loan[,sel]

set.seed(1)
numberOfRows <- nrow(loan)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- loan[train.index,]
valid.df <- loan[-train.index,]

logitI.reg <- glm(approve ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logitI.reg)

confusionMatrix(table(predict(logitI.reg, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$approve == 1))


#upsampling the data due to errors in confusion matrix
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

logitI.reg <- glm(approve ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logitI.reg)

confusionMatrix(table(predict(logitI.reg, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$approve == 1))
