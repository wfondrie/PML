
# Uncomment to download files
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "data.csv")
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "test.csv")

library(ggplot2)
library(caret)
library(plyr)
library(reshape2)
library(randomForest)
set.seed(1234)

dat <- read.csv("data.csv")
val <- read.csv("test.csv")

selectVars <- function(df) {
    facs <- c("user_name","cvtd_timestamp","classe") #dont change to numeric
    for(i in 1:ncol(df)) {
        if(!(names(df)[i] %in% facs)) {
            df[,i] <- as.numeric(df[,i])
            if(is.na(mean(df[,i]))) {
                names(df)[i] <- "drop"
            }
        } 
    }
    drop <- c("X","new_window", grep("drop",names(df),value=T))
    df <- df[,!(names(df) %in% drop)]
}

val <- selectVars(val)
dat <- dat[,c(names(val[,1:(length(names(val))-1)]),"classe")]
#dat <- selectVars(dat)
inTrain <- createDataPartition(dat$classe, p = 0.7, list = F)
training <- dat[inTrain,]
testing <- dat[-inTrain,]

mod <- train(classe~., data=training, method="rf",
             tuneGrid = data.frame(.mtry =sqrt(ncol(training)-1)),
             trControl=trainControl(method="none"))

#mod <- randomForest(classe~., data=training, mtry=sqrt(ncol(training)-1))

testPred <- predict(mod, newdata = testing)
confusionMatrix(testPred,testing$classe)


valPred <- predict(mod, newdata = val)

answers <- (as.character(valPred))

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)
