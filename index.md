Human Activity Recognition
========================================================

The rapid development of cheap and portable technology have enabled companies to develop new and exciting products. One such area in focus in the recent years is monitoring the daily activities of humans. Smart watches, shoes are all being fitted with sensors that continously monitor the users various movements. Although collection of sensor data is cheap and easy analysing them to make useful predictions is a challenging problem.

This report tries to develop an algorithm that tries to quantify "how" well a human subject performs a weight lifiting exercise. The data set used here is obtained from http://groupware.les.inf.puc-rio.br/har . The authors of the data set have collected sensory information from accelerometers on the belt, forearm, arm, and dumbell of 6 participants that were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

The problem being addressed here is the prediction of the class of performing exercise (A-E) from the sensory information.

## Data set summary
The training data set consists of 19622 observations with 152 attributes collected from various sensors attached to the body. Additionally the observations are indexed, subject name and time stamp of experiment are noted. 

```r
set.seed(123)

trData <- read.csv("pml-training.csv", stringsAsFactors = F, na.strings = c("NA", 
    ""))
newData <- read.csv("pml-testing.csv", stringsAsFactors = F, na.strings = c("NA", 
    ""))
```

For the purpose of testing the Coursera team has provided an additional 20 observations that will be used for testing the accuracy of the developed model.

## Pre-processing the data set
Since the aim of this project is to predict the activity type from sensory information the fields indicating the index, time, subject name, and window information are removed from the original data set.


```r
trivialCol <- grepl("^X|^user|*time*|*_window", names(trData))

no.MissingVal <- sapply(trData, function(col) sum(is.na(col)))
missingCol <- no.MissingVal != 0
no.MissingCol <- sum(missingCol)
perMissing <- max(no.MissingVal)/nrow(trData) * 100
```


The columns that are neglected are

```r
names(trData)[trivialCol]
```

```
## [1] "X"                    "user_name"            "raw_timestamp_part_1"
## [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
## [7] "num_window"
```


Additionally it is observed that 100 columns contain 97.9309% of NA values, hence these columns are also removed from analysis.


```r
trData <- trData[, !(missingCol | trivialCol)]
newData <- newData[, !(missingCol | trivialCol)]
trData$classe <- as.factor(trData$classe)  #For converting to classification problem
```


### Principal Component Analysis
Principal Component Analysis is performed on the remaining attributes to assertain wheather the attributes are correlated.


```r
x <- as.matrix(trData[, !names(trData) %in% "classe"])
pcaM <- prcomp(x, scale = T)
plot(pcaM, type = "l", main = "Varaince retained v/s no. of attributes")
```

![plot of chunk PCA](figure/PCA.png) 

```r
pcCumStdDev <- cumsum(pcaM$sdev)/sum(pcaM$sdev)
minDim9 <- min(which(pcCumStdDev > 0.9))
```


It can be observed that the PCA is not able to capture most of the variance in the data for low dimensions. The first principal component capture only 7.5671% of the total variance in the data set. It thus requires 33 components to capture 90% of the variance from the original 52 dimensional space.

Hence dimensionality reduction using PCA was not performed for this dataset.

## Data Partitioning
To develop the classification algorithm, the data set is divided into two subsets training and testing randomly in the ratio of 7:3.


```r
require(caret)
trIdx <- createDataPartition(trData$classe, p = 0.7)$Resample1
teData <- trData[-trIdx, ]
trData <- trData[trIdx, ]
```


## Modelling
For modelling the random forest algorithm was chosen. Random forest algroithm was trained using a five fold cross validation to evaluate the out of sample error.


```r
require(randomForest)
folds <- createFolds(trData$classe, k = 5)
foldModel <- list()
foldAcc <- numeric()
for (fold in folds) {
    model <- randomForest(classe ~ ., trData[fold, ])
    acc <- mean(predict(model, trData[-fold, ]) == trData[-fold, "classe"]) * 
        100
    foldModel <- c(foldModel, list(model))
    foldAcc <- c(foldAcc, acc)
}
bestModel <- foldModel[[which.max(foldAcc)]]
summary(foldAcc)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    96.1    96.1    96.3    96.4    96.6    96.7
```

It can be seen that the average cross validation accuracy of random forest is 96.3529% with a minimum of 96.0502% and maximum of 96.7152%.


```r
testLabel <- predict(bestModel, teData)
testAcc <- mean(testLabel == teData$classe) * 100
```


The testing accuracy (which is out of sample since it was not used for training) is 96.4826% which is within the expected range from cross validation results.





