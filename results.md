> # Download training and testing data files
> download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
+               destfile = "pml-training.csv")
trying URL 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
Content type 'text/csv' length 12202745 bytes (11.6 Mb)
opened URL
==================================================
downloaded 11.6 Mb
>
> download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
+               destfile = "pml-testing.csv")
trying URL 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
Content type 'text/csv' length 15113 bytes (14 Kb)
opened URL
==================================================
downloaded 14 Kb
> 
> # Pre-processing data
> # Read the raw data and set the classe as factor.??? 
> library(caret)
Loading required package: lattice
Loading required package: ggplot2
> set.seed(12463)
> data <- read.csv("pml-training.csv", stringsAsFactors=FALSE)
> data$classe <- as.factor(data$classe)
> data_test <- read.csv("pml-testing.csv", stringsAsFactors=FALSE)
> 
> # Then clean the data: 
> # a. Removing columns with features not related to precition outcomes (first 7 columns)
> data <- data[,-c(1:7)]
> data_test <- data_test[,-c(1:7)]
> 
> # b. Removing columns with too little data that are not valuable enough in prediction
> data <- data[,-nearZeroVar(data)]
> data_test <- data_test[,-nearZeroVar(data_test)]
> 
> # c. Removing columns with too many NAs (>90%)
> # function 1: 'def_NAs' define variables with na > 90% 
> def_NAs <- function(value){
+         if(sum(is.na(value))/length(value) > 0.9){ # if vector is made of more than 90% NAs
+                 res <- TRUE;                             # return true
+         }else{                                       # if it doesn't
+                 res <- FALSE;                            # return false
+         }
+         invisible(res);                              # return a invisible copy of the results
+ }
> 
> vars_90NAs <- sapply(data, def_NAs) # find columns with NA > 90%
> data <- data[,!vars_90NAs] # no NA in data anymore in this case
> 
> # For the data_test (testing set) 
> vars_90NAs_test <- sapply(data_test, def_NAs) 
> data_test <- data_test[,!vars_90NAs_test] 
> 
> # Split training data into training and validation
> inTrain <- createDataPartition(y=data$classe, p= 0.7, list=FALSE)
> training <- data[inTrain,]
> testing <- data[-inTrain,]
> 
> # Use pca method to pre-process data (this part can be included into training process)
> # preObj <- preProcess(training[,-length(training)], method="pca", thresh = 0.9)
> 
> # Prediction
> # Use "rf" method in caret package and all other variables in the training set in the prediction
> modelFit <- train(training$classe ~., data = training, method = "rf")
Loading required package: randomForest
randomForest 4.6-10
Type rfNews() to see new features/changes/bug fixes.
> test <- predict(modelFit, newdata = testing)
> confusionMatrix(testing$classe, test)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1674    0    0    0    0
         B    8 1129    2    0    0
         C    0    3 1016    7    0
         D    0    0   10  953    1
         E    0    0    2    2 1078

Overall Statistics
                                          
               Accuracy : 0.9941          
                 95% CI : (0.9917, 0.9959)
    No Information Rate : 0.2858          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9925          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9952   0.9973   0.9864   0.9906   0.9991
Specificity            1.0000   0.9979   0.9979   0.9978   0.9992
Pos Pred Value         1.0000   0.9912   0.9903   0.9886   0.9963
Neg Pred Value         0.9981   0.9994   0.9971   0.9982   0.9998
Prevalence             0.2858   0.1924   0.1750   0.1635   0.1833
Detection Rate         0.2845   0.1918   0.1726   0.1619   0.1832
Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
Balanced Accuracy      0.9976   0.9976   0.9922   0.9942   0.9991
> 
> # Use cross validation to valid the model  
> 
> # Final predictions on data_test 
> prediction <- predict(modelFit, newdata = data_test)
