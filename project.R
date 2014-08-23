#

#first thing that is required to do is to load all necessary libraries and set working directory
library(caret)
setwd(getScriptPath())

#after that library initialization, it is required to load datasets
#script will automatically download required datasets

urls <- c("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")



path <- getwd()
download.file(urls[1], file.path(path, "pml-training.csv"),method="auto")
download.file(urls[2], file.path(path, "pml-testing.csv"),method="auto")


#after download, it is required to load files so we can work with it
trainset<-read.csv("pml-training.csv")
testset<-read.csv("pml-testing.csv")


#next is required to divide training set into training and cross validation
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



#next step is training a model
#I will use lda and random forest algorithms and compare them
ldaFit <- train(classe ~ .,data=training,method="lda",verbose=FALSE)
rfModel <- randomForest(classe ~ ., data = training)

pvalidation <- predict(ldaFit, crossvalidation)
print(confusionMatrix(pvalidation, crossvalidation$classe))

pvalidation <- predict(rfModel, crossvalidation)
print(confusionMatrix(pvalidation, crossvalidation$classe))

ptraining <- predict(rfModel, testset)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(ptraining)

