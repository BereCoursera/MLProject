Weight Lifting excercise
========================================================

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the 
other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

**The out of sample error would be between .2 and .3 percent.**

```r
library(ggplot2)
library(lattice)
library(caret)

# load data
training <- read.csv("pml-training.csv", header = T, na.strings = c(NA, "", 
    "NA", "<NA>"))
testing <- read.csv("pml-testing.csv", header = T, na.strings = c(NA, "", "NA", 
    "<NA>"))

nonNA <- 1:ncol(training) == 0
for (i in 1:ncol(training)) {
    nonNA[i] = sum(is.na(training[, i])) == 0
}

training <- training[, nonNA]
testing <- testing[, nonNA]

training$user_name <- as.numeric(training$user_name)  # same users for testing also
testing$user_name <- as.numeric(testing$user_name)
training$new_window <- as.numeric(training$new_window)
testing$new_window <- as.numeric(testing$new_window)

isTraining <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
crossValidation <- training[-isTraining, ]
training <- training[isTraining, ]
use <- grep("classe|X|cvtd_timestamp", names(training), invert = T)  #|new_window|user_name

set.seed(1)

gbmGrid <- expand.grid(interaction.depth = c(5, 9), n.trees = (1:4) * 60, shrinkage = c(0.05, 
    0.1))

fitControl <- trainControl(method = "cv", number = 4, classProbs = TRUE)

system.time(modFit <- train(training$classe ~ ., data = training[, use], method = "gbm", 
    trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid))
```

```
## Loading required package: gbm
## Loading required package: survival
## Loading required package: splines
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: parallel
## Loaded gbm 2.1
## Loading required package: plyr
```

```
##     user   system  elapsed 
## 1738.117    8.811 1820.839
```

```r

cvPrediction <- predict(modFit, crossValidation[, use])
confusionMatrix(cvPrediction, crossValidation$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    2    0    0    0
##          B    0 1137    0    0    0
##          C    0    0 1024    0    0
##          D    0    0    2  963    1
##          E    0    0    0    1 1081
## 
## Overall Statistics
##                                     
##                Accuracy : 0.999     
##                  95% CI : (0.998, 1)
##     No Information Rate : 0.284     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 0.999     
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    0.998    0.998    0.999    0.999
## Specificity             1.000    1.000    1.000    0.999    1.000
## Pos Pred Value          0.999    1.000    1.000    0.997    0.999
## Neg Pred Value          1.000    1.000    1.000    1.000    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.164    0.184
## Detection Prevalence    0.285    0.193    0.174    0.164    0.184
## Balanced Accuracy       1.000    0.999    0.999    0.999    0.999
```

```r

prediction <- predict(modFit, testing[, use])
answers = as.character(prediction)

pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_b", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}
pml_write_files(answers)
```


