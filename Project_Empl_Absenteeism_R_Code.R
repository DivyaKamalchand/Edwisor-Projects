#Install Excel package
install.packages("readxl")
install.packages("DMwR")
install.packages("usdm")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("Hmisc")

library("randomForest")
library("readxl")
library("DMwR")
library("rpart")
library("MASS")
library("C50")


library("ggplot2")
library("scales")
library("psych")
library("gplots")

#Remove all Global variables
rm(list=ls())

#Read the Excel File
Empl_Absent = read_xls("C:/Users/user/Desktop/Edwisor_Divya/Project_Employee_Absent/Absenteeism_at_work_Project.xls",col_names = TRUE)

#Dimension of Data Set
dim(Empl_Absent)

#Structure of Dataset
str(Empl_Absent)

Empl_Absent <- data.frame(Empl_Absent)
##################################Missing Values Analysis###############################################
Empl_Abs_Miss_Val = data.frame(apply(Empl_Absent,2,function(x){sum(is.na(x))}))
Empl_Abs_Miss_Val$Columns = row.names(Empl_Abs_Miss_Val)
names(Empl_Abs_Miss_Val)[1] =  "Missing_percentage"
Empl_Abs_Miss_Val$Missing_percentage = (Empl_Abs_Miss_Val$Missing_percentage/nrow(Empl_Absent)) * 100
Empl_Abs_Miss_Val = Empl_Abs_Miss_Val[order(-Empl_Abs_Miss_Val$Missing_percentage),]
row.names(Empl_Abs_Miss_Val) = NULL
Empl_Abs_Miss_Val = Empl_Abs_Miss_Val[,c(2,1)]


# Plot missing value percentage
ggplot(Empl_Abs_Miss_Val, aes(x = Empl_Abs_Miss_Val$Columns ,  y = Empl_Abs_Miss_Val$Missing_percentage)) +
  geom_bar(stat = "identity", width = 0.8) +
  xlab("Column Names") + ylab('Percentage')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Apply kNN Imputation for missing value method
Empl_Absent= knnImputation(Empl_Absent)

#Check for the summary of dataset
summary(Empl_Absent)
anyNA(Empl_Absent)

#Visualization

# 1. Compare Reason.for.absence Vs  Absenteeism.time.in.hours

  ggplot(Empl_Absent, aes(x = Empl_Absent$Reason.for.absence, y = Empl_Absent$Absenteeism.time.in.hours) )+
  geom_line(stat = "identity", width = 0.8,color="blue") +
  xlab("Absence Reason") + ylab('Number of Absence hour') +
  scale_y_continuous(breaks=c(seq(0,1000,100)))
  scale_x_continuous(breaks = unique(round(Empl_Absent$Reason.for.absence))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

  # 2. Compare Reason.for.absence Vs  Absenteeism.time.in.hours Vs Seasons
  
  ggplot(Empl_Absent, aes_string(x = Empl_Absent$Seasons, y = Empl_Absent$Absenteeism.time.in.hours,fill = factor(round(Empl_Absent$Reason.for.absence)))) +
  geom_bar(stat = "identity", width = 0.8) +
  xlab("Seasons") + ylab('Number of Absence hour') +
  scale_y_continuous(breaks=c(seq(0,2000,100))) 
  
  # 3. Compare Reason.for.absence Vs  Count Vs Social.Drinker

  ggplot(Empl_Absent, aes(x = Empl_Absent$Reason.for.absence, y = Empl_Absent$ID,fill = factor(round(Social.drinker))))+
    geom_bar(stat = "identity", width = 0.8) +
    xlab("Reason") + ylab('Count') +
    scale_x_continuous(breaks = unique(round(Empl_Absent$Reason.for.absence)))+
    scale_y_discrete(breaks=c(seq(0,200,20))) 

# 4. Compare Reason.for.absence Vs  Count Vs Social.Somker

  ggplot(Empl_Absent, aes(x = Empl_Absent$Reason.for.absence, y = Empl_Absent$ID,fill = factor(round(Social.smoker))))+
  geom_bar(stat = "identity", width = 0.8) +
  xlab("Reason") + ylab('Count') +
    scale_x_continuous(breaks = unique(round(Empl_Absent$Reason.for.absence)))+
  scale_y_discrete(breaks=c(seq(0,200,20)))  
 
  
  # 5. Compare Reason.for.absence Vs  Count Vs Body Mass Index
  
  ggplot(Empl_Absent, aes(x = Empl_Absent$Reason.for.absence, y = Empl_Absent$Body.mass.index))+
    geom_bar(stat = "identity", width = 0.8) +
    xlab("Reason") + ylab('Body Mass Index') +
    scale_x_continuous(breaks = unique(round(Empl_Absent$Reason.for.absence)))

  
 #check Multicollinerity
library(usdm)

vif(Empl_Absent[,-21])
vifcor(Empl_Absent[,-21], th = 0.9)


## correlation plots 


library(Hmisc)

numeric_index = sapply(Empl_Absent,is.numeric) #selecting only numeric
library(corrplot)
dev.new(width=5, height=4)
M = cor(Empl_Absent[,numeric_index])

corrplot(M,title = "Correlation Plot", method = "square",type ="full")
dev.off()


#Delete the collinear variable
Empl_Absent$Weight = NULL

#Divide the data into train and test
train_index = sample(1:nrow(Empl_Absent), 0.8 * nrow(Empl_Absent))
train = Empl_Absent[train_index,]
test = Empl_Absent[-train_index,]


# Decision Tree rpart for regression
fit = rpart(Absenteeism.time.in.hours ~., data = train, method = "anova")
summary(fit)
#Predict for new test cases
predictions_DT = predict(fit, test[,-20])
regr.eval(test[,20], predictions_DT)


# random Forest model

RF_Model = randomForest(Absenteeism.time.in.hours ~., data = train,importance = TRUE,na.action=na.roughfix, ntree =100)

Prediction_RF = predict(RF_Model,test[,-20])

regr.eval(test[,20], Prediction_RF)

