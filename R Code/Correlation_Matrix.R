library(rpart) 
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)
library(readxl)
library(groupdata2)
library(randomForest)
library(plyr)
library(corrplot)

final <- c("occ", "loanamt",	"appinc",	"unit",	"married",	"dep",	"emp",
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

#Correlation matrix
loan <- data.matrix(loan)


mydata.cor = cor(loan)
corrplot(mydata.cor)

palette = colorRampPalette(c("green", "white", "red")) (30)
heatmap(x = mydata.cor, col = palette, symm = TRUE)


corrplot(mydata.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 100)
