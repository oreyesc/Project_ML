# Loading Libraries
library (caret)
library (ggplot2)
library (knitr)
library (lattice)
library (randomForest)
library (rattle)
library (RColorBrewer)
library (rpart)
library (rpart.plot)
set.seed(1234)


##Loading Data
home = setwd(Sys.getenv("HOME"))

# Source Directory
swdMain <- "/Users/oreyesc"
swdSubDir <- "MachineLearningProject"
getswd <- getwd()

# URLs
trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# File names:
trainingfile <- "pml-training.csv"
testingfile <- "pml-testing.csv"


if (getswd != swdMain){
        if (!file.exists(swdMain)){
                dir.create(file.path(swdMain,
                                     swdSubDir),
                           showWarnings = FALSE)
        }

} else if (!file.exists(swdSubDir)){
        dir.create(file.path(swdSubDir),
                   showWarnings = FALSE)
}
setwd (file.path(swdMain, swdSubDir))

# Downloading the information:
if (!file.exists(trainingfile)){
        download.file(url = trainingURL,
                      destfile = trainingfile,
                      method = "curl")
}
if (!file.exists(testingfile)){
        download.file(url = testingURL,
                      destfile = testingfile,
                      method = "curl")
}
# Loading information from files:
trainingData <- read.csv(trainingfile,
                         row.names = 1,
                         header = TRUE,
                         sep = ",",
                         na.strings = c("NA",
                                        "#DIV/0!",
                                        ""))

testingData <- read.csv(testingfile,
                        row.names = 1,
                        header = TRUE,
                        sep = ",",
                        na.strings = c("NA",
                                       "#DIV/0!",
                                       ""))
trainingData <- trainingData[, -1]

#Datasets division
##Create data partitions from training dataset: training 60% and testing 40%
trainingDivision = createDataPartition(trainingData$classe,
                                       p = 0.60,
                                       list = FALSE)
training <- trainingData[trainingDivision, ]
testingTraining <- trainingData[-trainingDivision, ]
dim(training); dim(testingTraining)

#Exploration & Cleaning
#Remove all information with less than 60% of fields filled
#Quantity of columns that have less than 60% of fields filled
sum((colSums(!is.na(training[,
                             -ncol(training)])) < 0.6 * nrow(training)))

#Removing columns with poor data (less than 60%)
filledColumns <- c((colSums(!is.na(training[,
                                            -ncol(training)])) >= 0.6 * nrow(training)))
training <- training[,
                     filledColumns]
testingTraining <- testingTraining[, filledColumns]

#Random Forest Model
# Cross validation or separate test set is not required in random forest to obtain an unbiased estimate of the test set error.
# The test set error is evaluated by the aplication, on the execution.

randomForestModel <- randomForest(classe ~.,
                                  method = "rf",
                                  data = training,
                                  importance = TRUE)
print (randomForestModel)

#Evaluating the Model
# Variable importance measures:
importance(randomForestModel)

#Confusion Matrix
predictTraining <- predict(randomForestModel,
                           newdata = testingTraining[,
                                                     -ncol(testingTraining)])
confusionMatrix(predictTraining,
                testingTraining$classe)

#Accuracy validation
accuracyRandomForest <- c(as.numeric(predictTraining == testingTraining$classe))
accuracyRandomForest <- sum(accuracyRandomForest) * 100 / nrow(testingTraining)
accuracyRandomForest
outOfSampleError <- 100 - accuracyRandomForest
outOfSampleError

importantVariable <- varImp(randomForestModel)$importance
varImpPlot(randomForestModel,
           sort = TRUE,
           type = 1,
           pch = 19,
           col = 1,
           cex = 1,
           main = "Predictors - Level of Importance")


#Testing the Model
testingD <- testingData[, -1]
testingD <- testingD[,
                     filledColumns]
testingD <- testingD[,
                     -ncol(testingD)]
testing <- rbind(training[100, -58],
                 testingD)
row.names(testing) <- c(100, 1:20)

predictions <- predict(randomForestModel,
                       newdata = testing[-1, ])

print (predictions)

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("./answers/problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(predictions)

#Decision Trees
decisionTree <- rpart(classe ~.,
                      data = training,
                      method = "class")
fancyRpartPlot(decisionTree)
predictionDecisionTree <- predict(decisionTree,
                                   testingTraining,
                                   type = "class")

confusionMatrix(predictionDecisionTree,
                testingTraining$classe)

accuracyDecisionTree <- c(as.numeric(predictionDecisionTree == testingTraining$classe))
accuracyDecisionTree <- sum(accuracyDecisionTree) * 100 / nrow(testingTraining)
accuracyDecisionTree

outOfSampleErrorDT <- 100 - accuracyDecisionTree
outOfSampleErrorDT



