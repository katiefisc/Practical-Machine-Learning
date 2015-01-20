training_URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainingData <- read.csv(textConnection(getURL(training_URL)), na.strings = c("NA", ""))
testData <- read.csv(textConnection(getURL(test_URL)), na.strings = c("NA", ""))

https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = "./Reproducable_Research/activity.csv")
#unzip the file manually
data <- read.csv("./Reproducable_Research/activity.csv", colClasses = c("numeric", "character", "numeric"),
                 header = TRUE, sep = ",", na.strings=c("NA"))


library(RCurl)
library(caret)
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
train_data <- read.csv(text=getURL(train_url), na.strings=c("", "NA", "<NA>"))
test_data <- read.csv(text=getURL(test_url), na.strings=c("", "NA", "<NA>"))
summary(train_data)
describe(train_data)
head(train_data)
colnames(train_data)
trainDataSmall <- train_data[, -c (1:7)]
testDataSmall <- test_data[, -c (1:7)]

training_data <- trainDataSmall[ , !apply(is.na(trainDataSmall), 2, any)]
testing_data <- testDataSmall[ , !apply(is.na(testDataSmall), 2, any)]

inTrain <- createDataPartition(y=training_data$classe,
                               p=0.75, list=FALSE)
training <- training_data[inTrain,]
CVtesting <- training_data[-inTrain,]


setseed(1000)
cvControl = trainControl(method='cv',number=5,repeats=2,returnResamp='none')

modelFit <- train(training$classe~.,data=training,method="rf", trControl=cvControl)

predictions <- predict(modelFit,newdata=CVtesting)
confusionMatrix(predictions,CVtesting$classe)
modelFit$finalModel

predictionsTest <- predict(modelFit,newdata=testing_data)
confusionMatrix(predictions,testing_data$classe)

