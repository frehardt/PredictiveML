library(caret)
library(ggplot2)
library(randomForest)



trainingData <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings=c("", "NA", "NULL"), header=TRUE, sep = ",")
testingData <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings=c("", "NA", "NULL"), header=TRUE, sep = ",")

# Show the 5 diffrent classes and its frequency
plot(trainingData$classe, col="blue", main="Frequency of the 5 different classes", xlab="classes", ylab="Frequency")


summary(trainingData)
summary(testingData)


#trainingDataIndex <- createDataPartition(trainingData$classe,p=0.01,list=FALSE)
#trainingData <- trainingData[trainingDataIndex, ]

# Delete columns with NA values
trainingDataCleaned <- trainingData[,colSums(is.na(trainingData)) == 0]
testingDataCleaned <- testingData[,colSums(is.na(testingData)) == 0]

summary(trainingDataCleaned)


# Clean non numeric variables for prediction
trainingDataCleaned <- trainingDataCleaned[,-c(1:7)]
#trainingDataCleaned <- which(lapply(trainingDataCleaned,class) %in% c('numeric'))
testingDataCleaned <- testingDataCleaned[,-c(1:7)]

dim(trainingDataCleaned)
dim(testingDataCleaned)

#Building the sample data
samplesAll <- createDataPartition(y=trainingDataCleaned$classe, p=0.75, list=FALSE)
samplesTraining <- trainingDataCleaned[samplesAll, ] 
samplesTesting <- trainingDataCleaned[-samplesAll, ]


modelRF <- randomForest(classe ~. , data=samplesTraining, method="class")

# Predicting:
predictionRF <- predict(modelRF, samplesTesting, type = "class")

# Test results on subTesting data set:
confusionMatrix(predictionRF, samplesTesting$classe)



