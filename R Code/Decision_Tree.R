library(rpart) 
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)
library(readxl)
library(groupdata2)
library(randomForest)
library(plyr)

final <- c("occ", "loanamt",	"msa",	"appinc",	"unit",	"married",	"dep",	"emp",
           "self",	"hexp", "price",	"other",	"liq",	"gdlin",	"mortg",	"pubrec",	
           "hratAmt",	"obratAmt",	"fixadj", "term",	"cosign",	"netw",	"sch",	"hispan",	
           "male",	"approve",	"mortno",	"mortperf",	"chist",	"multi")


loan <- read.csv("loan_approve_project.csv")
loan <- loan[loan$gdlin <= 1, final]
View(loan)

loan$approve[loan$approve==1]="Approved"
loan$approve[loan$approve==0]="Rejected"
# Factors
cols <- c("married","self","gdlin","pubrec","fixadj","cosign","sch",
          "hispan","male","approve","mortno","mortperf","chist","multi")
loan[cols] <- lapply(loan[cols], factor)

#Decision tree with all the columns
set.seed(1)
numberOfRows <- nrow(loan)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- loan[train.index,]
valid.df <- loan[-train.index,]

.ct <- rpart(approve ~ ., data = train.df, method = "class", cp = 0.00001, maxdepth = 3, minsplit = 1)

printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
.ct
ct.pred<-predict(.ct,valid.df,type="class")

confusionMatrix(ct.pred, as.factor(valid.df$approve))

#Decision tree with upsampling
loan <- read.csv("loan_approve_project.csv")
loan <- loan[loan$gdlin <= 1, final]
loan$approve[loan$approve==1]="Approved"
loan$approve[loan$approve==0]="Rejected"
View(loan)
loan[cols] <- lapply(loan[cols], factor)
Balanced <- upsample(loan, cat_col = "approve")
View(Balanced)

set.seed(1)
numberOfRows <- nrow(Balanced)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- Balanced[train.index,]
valid.df <- Balanced[-train.index,]


.ct <- rpart(approve ~ ., data = train.df, method = "class", cp = 0.00001, maxdepth = 4, minsplit = 1)
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
.ct
ct.pred<-predict(.ct,valid.df,type="class")

confusionMatrix(ct.pred, as.factor(valid.df$approve))

