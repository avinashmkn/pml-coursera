Practical Machine Learning Course Project:
Avinash M

Feb 22, 2015

##Summary
The goal of this course project is to utilize the activity monitor device data and generate a machine learning model]. Once the model is built, after assessing the performance the training model is to be applied to a new set of testing data to make predictions. 

##Input Data
The input data consisted of various movement measurments including acceleration components of the arms and pitch and roll orientations of the dumbell. More information can be found at the original data authors website linked to below.

The data used here was downloaded from the course website, where the training and testing data were already partitioned:

##Training Data
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

##Testing Data
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

##Data Analysis and Predictions

Following are packages to be loaded:
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(caret)

set.seed(12345)

Now, we need to read in the data and perform some dataset filtering. Given the complexity of the underlying activity features and limited documentation on what they actually are, I will use a very simple approach: remove all features with missing values. In this data, these could be NA’s or simply empty strings

## Read in the training and testing data data
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))E)

## Partition training and testing data sets on 60:40 ratio
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]
dim(myTraining); dim(myTesting)

## Cleaning NearZeroVariance Variables Run this code to view possible NZV Variables:
myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)

## Removing first ID variable so that it does not interfer with ML Algorithms:
 myTraining <- myTraining[c(-1)]
 
## Cleaning Variables with too many NAs.
trainingV3 <- myTraining #creating another subset to iterate in loop
for(i in 1:length(myTraining)) { #for every column in the training dataset
        if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { #if n?? NAs > 60% of total observations
        for(j in 1:length(trainingV3)) {
            if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
                trainingV3 <- trainingV3[ , -j] #Remove that column
            }   
        } 
    }
}

## Do the same transformations on testing data

clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58]) #already with classe column removed
myTesting <- myTesting[clean1]
testing <- testing[clean2]

#Decision Tree Predictions
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")

predictionsA1 <- predict(modFitA1, myTesting, type = "class")

confusionMatrix(predictionsA1, myTesting$classe)

          Reference
Prediction    A    B    C    D    E
         A 2157   59   11    3    0
         B   56 1248   70   61    0
         C   19  200 1262  142   53
         D    0   11   15  868   76
         E    0    0   10  212 1313

Overall Statistics
                                          
               Accuracy : 0.8728          
                 95% CI : (0.8652, 0.8801)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.839           
 Mcnemar's Test P-Value : NA 

#Random Forests Prediction

modFitB1 <- randomForest(classe ~. , data=myTraining)

predictionsB1 <- predict(modFitB1, myTesting, type = "class")

confusionMatrix(predictionsB1, myTesting$classe)

Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2232    1    0    0    0
         B    0 1517    1    0    0
         C    0    0 1367    2    0
         D    0    0    0 1283    1
         E    0    0    0    1 1441

Overall Statistics
                                          
               Accuracy : 0.9992          
                 95% CI : (0.9983, 0.9997)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.999           
 Mcnemar's Test P-Value : NA  

Random Forests yielded better Results, as expected!

## Predictions :

B	B	B	TRUE
A	A	A	TRUE
B	B	B	TRUE
A	A	A	TRUE
A	A	A	TRUE
E	E	E	TRUE
D	D	D	TRUE
B	B	B	TRUE
A	A	A	TRUE
A	A	A	TRUE
B	B	B	TRUE
C	C	C	TRUE
B	B	B	TRUE
A	A	A	TRUE
E	E	E	TRUE
E	E	E	TRUE
A	A	A	TRUE
B	B	B	TRUE
B	B	B	TRUE
B	B	B	TRUE


## This uses the code supplied by the class instructions
  answers <- pred.df$rf.pred

  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
  
  pml_write_files(answers)

The predictions above were submitted to for automated grading, and all were found to be correct.
