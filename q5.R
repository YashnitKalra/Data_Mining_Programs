df=read.csv("breast_cancer/breast-cancer-wisconsin.data",header = FALSE)
colnames(df)=c("ID", "Clump_Thickness", "Uniformity_of_Cell_Size", "Uniformity_of_Cell_Shape", "Marginal_Adhesion", "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class")

df=replace(df,df=="?",NA) # replace "?" in any cell with NA
df=df[complete.cases(df),] # remove rows having NA in any column
df$Bare_Nuclei=as.integer(df$Bare_Nuclei)
df$Class=as.factor(df$Class)
head(df)

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(class)
library(naivebayes)

fnormalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}

createModel=function(df,split_ratio,sampling_method,model_type){
  
  # random subsampling
  if(sampling_method=="random"){
    accuracies=c()
    for(i in seq(1,10)){
      acc=createModel(df,split_ratio,"holdout",model_type)
      accuracies=c(accuracies,acc)
    }
    return(mean(accuracies))
  }
  
  else{
    # splitting
    split=sample.split(df$Class,SplitRatio = split_ratio)
    training_set = subset(df, split == TRUE)
    testing_set = subset(df, split == FALSE)
    
    # holdout
    if(sampling_method == "holdout"){
      
      # decision tree
      if(model_type == "decisionTree"){
        myModel=rpart(Class~.,training_set,method = "class")
        predictions=predict(myModel,testing_set,type="class")
      }
      
      # knn
      else if(model_type=="knn"){
        temp=as.data.frame(lapply(training_set[1:10], fnormalize))
        temp$Class=training_set$Class
        training_set=temp
        
        temp=as.data.frame(lapply(testing_set[1:10], fnormalize))
        temp$Class=testing_set$Class
        testing_set=temp
        
        predictions=knn(training_set,testing_set,cl=training_set$Class,k=13)
      }
      
      # naive bayes
      else if(model_type=="naiveBayes"){
        myModel=naiveBayes(training_set$Class~.,data=training_set)
        predictions=predict(myModel,testing_set[-11])
      }
      
    }
    
    # cross validation
    else if(sampling_method=="cross"){
      
      # Decision Tree
      if(model_type=="decisionTree"){
        myModel=train(Class~., data = training_set, method='rpart', trControl = trainControl(method = 'cv', number = 10))
      }
      
      # knn
      else if(model_type=="knn"){
        myModel = train(Class~., data=training_set, method="knn", trControl=trainControl(method="cv", number=20))
      }
      
      # naive bayes
      else if(model_type=="naiveBayes"){
        myModel=train(Class~.,data=training_set,method="naive_bayes",trControl=trainControl(method="cv",number = 10))
      }
      
      predictions=predict(myModel,testing_set,method="class")
    }
    
  }
  #rpart.plot(myModel)
  #rpart.plot(myModel,type=4,extra=101)
  p=confusionMatrix(predictions,testing_set$Class)
  p$overall[1] # Accuracy
}

set.seed(5)
for(i in c("decisionTree","knn","naiveBayes")){
  for(j in c("holdout","random","cross")){
    for(k in c(0.75,0.6666)){
      acc=createModel(df,k,j,i)
      print(sprintf("Model: %s, Sampling: %s, Ratio: %f, Accuracy: %f",i,j,k,acc))
    }
  }
  cat("\n")
}