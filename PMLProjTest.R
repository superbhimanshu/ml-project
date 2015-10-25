## Author- Himanshu Verma
library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(dplyr)
library(reshape2)


 set.seed(1234)
 x <<- read.csv("pml-training.csv")
 finalTest <<- read.csv("pml-testing.csv")
 y <- names(x)
 filterData <<- data.frame()
 ## cols vector is being used to track the column numbers which will be considered in the training
 cols <<- numeric()
 
 
 #based on correlation between columns, I figured out that following columns should not be considered in 
 #training our model
 #########Correlation estimation is done only on numeric columns which are considered relevant
 #########for our training
 ####Correlation Code ######
 #### d_cor <<- as.matrix(cor(filterData[,1:44]))
 #### d_cor_melt <<- arrange(melt(d_cor), -abs(value))
 #### highCors <<- subset(d_cor_melt, value > .5)
 ###########################
 ignoreCols <- c("roll_belt","gyros_dumbbell_z",
                 "magnet_belt_x","accel_belt_y","accel_dumbbell_z","gyros_forearm_y",
                 "magnet_arm_y","accel_arm_x","accel_dumbbell_x")
 
 ###### I also found out that a lot of columns values are more than 97% empty or NA, it turned out
 ##that 19216 is the exact count when these columns were NA or empty therefore its hardcoded in the loop)
 ### I have ignored column 1:6 because they dont provide any relevant information for modelling
 j <- 1
 for(i in y) {
     s<- paste(i,sum(is.na(x[[i]])),sum(x[[i]] == ""), sum(is.null(x[[i]])), sep="  ")
     if(sum(is.na(x[[i]]))!=19216 && sum(x[[i]] == "")!=19216 && sum(is.null(x[[i]]))!=19216 && (j>6)) {
         if(!(i %in% ignoreCols)) {
             cols <<- append(cols, j)
         }
     }
     j <- j+1
  #   print(s)

 }

# filtering the training data based on all above arguements
 filterData <<- subset(x[,cols])
 
 # same analogy will be applied for the actual test data
 filterTestData <<- subset(finalTest[,cols])
 
 ## creating data partition within training set with 70-30 split
 inTrain <- createDataPartition(y=filterData$classe, p=.7, list=FALSE)
 trainingData <<- filterData[inTrain,]
 testingData <<- filterData[-inTrain,]
 
 ### defining trainControl although repeats is set to 1 because time consumption was too high 
 ctrl <- trainControl(method = "repeatedcv", repeats = 1)
 ### preprocessing with Principal Component Analysis 
 preObj <- preProcess(trainingData[,-45], method=c("center", "scale", "pca"), thresh=0.9)
 trainPC <- predict(preObj, trainingData[,-45])
 ## training random forest model
 modelFit <<- train(trainingData$classe ~ ., data=trainPC, method="rf", trControl=ctrl)
 
 ###### Testing phase starts ######
 ##using same preprocessing object for testing data
 testPC <- predict(preObj, testingData[,-45])
 predictions <- predict(modelFit, newdata=testPC)
 confusionMatrix(predictions, testingData$classe)
 
 # Here we check our model on actual test set provided in the project
 finaltestPC <- predict(preObj, filterTestData[,-45])
 predictions <- predict(modelFit, newdata=finaltestPC)
 predictions

