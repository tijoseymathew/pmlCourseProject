set.seed(123)

trData <- read.csv("pml-training.csv", 
                   stringsAsFactors=F, 
                   na.strings=c("NA",""))
newData <- read.csv("pml-testing.csv", 
                            stringsAsFactors=F, 
                            na.strings=c("NA",""))

# Task :- Predict the classe variable

# Pre-Processing data

# Observation
# 1. 67 columns in training data have more than 97% missing values
# 2. Other possible irrelevant columns removed X, user_name, *time*, *_window
# 3. Principal component on the remaining variables show large dispersion hence 
#    PCA reduction is not advisable.


no.MissingVal <- sapply(trData, function(col) sum(is.na(col)))
print(sprintf("%d attributes with missing values identified with %f%% rows having missing values",
              sum(no.MissingVal!=0), max(no.MissingVal)/nrow(trData)*100))
missingCol <- no.MissingVal == max(no.MissingVal)

trivialCol <-  grepl("^X|^user|*time*|*_window", names(trData))
print(sprintf("%s also neglected from analysis since it is trivial", 
              names(trData)[trivialCol]))

trData <- trData[, !(missingCol|trivialCol)]
newData <- newData[, !(missingCol|trivialCol)]
trData$classe <- as.factor(trData$classe)
# PCA analysis
x <- as.matrix(trData[, !names(trData)%in%"classe"])
pcaM <- prcomp(x, scale=T)
plot(pcaM)
pcCumStdDev <- cumsum(pcaM$sdev)/sum(pcaM$sdev)
print(sprintf("%d dimensions will be required to retain 90%% of variance!",
              min(which(pcCumStdDev>.9))))
# Data spliting
trIdx <- createDataPartition(trData$classe, p=0.7)$Resample1
teData <- trData[-trIdx, ]
trData <- trData[trIdx, ]

# Modelling with 6 fold - Cross Validation 
require(caret)
folds <- createFolds(trData$classe, k=5)
foldModel <- list()
foldAcc <- numeric()
for (fold in folds){
  model <- randomForest(classe~., trData[fold,])
  acc <- mean(predict(model, trData[-fold,])==trData[-fold,"classe"])
  foldModel <- c(foldModel, list(model))
  foldAcc <- c(foldAcc, acc)
}
print("Summary statistics for accuracy of different folds is")
summary(foldAcc)

answers <- predict(foldModel[[which.max(foldAcc)]], newData)
