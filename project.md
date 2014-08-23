# Practical machine learning - Project #

The main goal of this project was to build predictive model for given dataset:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

Second part of project was to predict 20 test 

Loading libraries
-----------------

First thing that is required to do is to load all necessary libraries and set working directory
```r
library(caret)
setwd(getScriptPath())
```

Loading datasets
----------------

After library initialization, it is required to load datasets
script will automatically download required datasets
```r
urls <- c("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
```

```r
path <- getwd()
download.file(urls[1], file.path(path, "pml-training.csv"),method="auto")
download.file(urls[2], file.path(path, "pml-testing.csv"),method="auto")
```

after download, it is required to load files so we can work with it
```r
trainset<-read.csv("pml-training.csv")
testset<-read.csv("pml-testing.csv")

```

Splitting data and cleaning data
--------------

Next is required to divide training set into training and cross validation

Data will be divided in 60:40 ratio (60% training, 40% for validation). It is also important to remove some columns which are not important for training or which have NA values.

```r

training<-createDataPartition(y=trainset$classe,p=0.60,list=FALSE)

crossvalidation<-trainset[-training,]
training<-trainset[training,]

nums <- sapply(training, is.numeric)
nums["classe"]<-TRUE
nums["X"]<-FALSE
nums["raw_timestamp_part_1"]<-FALSE
nums["raw_timestamp_part_2"]<-FALSE
nums["cvtd_timestamp"]<-FALSE
nums["num_window"]<-FALSE
training<-training[,nums]
testset<-testset[,nums]
crossvalidation<-crossvalidation[,nums]
testset<-testset[colSums(is.na(training))==0]
crossvalidation<-crossvalidation[colSums(is.na(training))==0]
training<-training[colSums(is.na(training))==0]
```

Training
--------

Next step is training a model. I will use lda and random forest algorithms and compare them

```r
ldaFit <- train(classe ~ .,data=training,method="lda",verbose=FALSE)
rfModel <- randomForest(classe ~ ., data = training)
```

```r

pvalidation <- predict(ldaFit, crossvalidation)
print(confusionMatrix(pvalidation, crossvalidation$classe))

pvalidation <- predict(rfModel, crossvalidation)
print(confusionMatrix(pvalidation, crossvalidation$classe))


```
Results using lda:

    Confusion Matrix and Statistics
    
              Reference
    Prediction    A    B    C    D    E
             A 1844  235  144   79   58
             B   54  962  121   46  238
             C  176  201  892  156  125
             D  146   62  174  939  131
             E   12   58   37   66  890
    
    Overall Statistics
                                              
                   Accuracy : 0.7044          
                     95% CI : (0.6942, 0.7145)
        No Information Rate : 0.2845          
        P-Value [Acc > NIR] : < 2.2e-16       
                                              
                      Kappa : 0.6257          
     Mcnemar's Test P-Value : < 2.2e-16       
    
    Statistics by Class:
    
                         Class: A Class: B Class: C Class: D Class: E
    Sensitivity            0.8262   0.6337   0.6520   0.7302   0.6172
    Specificity            0.9081   0.9275   0.8984   0.9218   0.9730
    Pos Pred Value         0.7814   0.6770   0.5755   0.6467   0.8373
    Neg Pred Value         0.9293   0.9135   0.9244   0.9457   0.9186
    Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    Detection Rate         0.2350   0.1226   0.1137   0.1197   0.1134
    Detection Prevalence   0.3008   0.1811   0.1976   0.1851   0.1355
    Balanced Accuracy      0.8671   0.7806   0.7752   0.8260   0.7951


Results using random forest algorithm:

    Confusion Matrix and Statistics
    
              Reference
    Prediction    A    B    C    D    E
             A 2230    3    0    0    0
             B    2 1515   16    0    0
             C    0    0 1351   23    0
             D    0    0    1 1260    6
             E    0    0    0    3 1436
    
    Overall Statistics
                                             
                   Accuracy : 0.9931         
                     95% CI : (0.991, 0.9948)
        No Information Rate : 0.2845         
        P-Value [Acc > NIR] : < 2.2e-16      
                                             
                      Kappa : 0.9913         
     Mcnemar's Test P-Value : NA             
    
    Statistics by Class:
    
                         Class: A Class: B Class: C Class: D Class: E
    Sensitivity            0.9991   0.9980   0.9876   0.9798   0.9958
    Specificity            0.9995   0.9972   0.9964   0.9989   0.9995
    Pos Pred Value         0.9987   0.9883   0.9833   0.9945   0.9979
    Neg Pred Value         0.9996   0.9995   0.9974   0.9960   0.9991
    Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    Detection Rate         0.2842   0.1931   0.1722   0.1606   0.1830
    Detection Prevalence   0.2846   0.1954   0.1751   0.1615   0.1834
    Balanced Accuracy      0.9993   0.9976   0.9920   0.9894   0.9977

As we can see, random forest algorithm has better accuracy so it will be used for predicting classe variable for test set

And the last step is to print results to txt files for second part of the project



```r

ptraining <- predict(rfModel, testset)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(ptraining)

```

