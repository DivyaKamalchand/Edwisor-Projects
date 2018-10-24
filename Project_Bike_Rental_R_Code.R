rm(list = ls())

# data
data = read.csv('C:/Users/monish/Documents/Edwisor/Project Bike Rental Count/day.csv')

# look at the data
head(data)

tail(data)

library(dplyr)

# dimentions of the data
dim(data)

# missing values

sum(is.null(data))

# discriptive analysis

names(data)

continuous = list('temp', 'atemp', 'hum', 'windspeed',
                  'casual', 'registered', 'count')

summary(data)

# box plot

library(ggplot2)

library(Rmisc)

#d <- density(train$account.length) # returns the density data 
p1 <- ggplot(data, aes(x = "",y=data$temp)) +
  geom_boxplot() + labs(title="box plot on temperature") # plots the results
p2 <- ggplot(data, aes(x = "",y=data$hum)) +
  geom_boxplot() + labs(title="box plot on humidity")
p3 <- ggplot(data, aes(x = "",y=data$atemp)) +
  geom_boxplot() + labs(title="box plot on atemp")
p4 <- ggplot(data, aes(x = "",y=data$windspeed)) +
  geom_boxplot() + labs(title="box plot on windspeed")
multiplot(p1, p2, p3, p4, cols=2)


p1 <- ggplot(data, aes(x = "",y=data$casual)) +
  geom_boxplot() + labs(title="box plot on casual") # plots the results
p2 <- ggplot(data, aes(x = "",y=data$registered)) +
  geom_boxplot() + labs(title="box plot on registered")
p3 <- ggplot(data, aes(x = "",y=data$count)) +
  geom_boxplot() + labs(title="box plot on count")

multiplot(p1, p2, p3, cols=2)

# treating extream values

data$hum = replace(data$hum, c(50), c(0.507463))

data$windspeed = replace(data$windspeed, c(50), c(0.187917))

library(dplyr)

d = data %>% filter(between(instant,62,75))
mean(d$hum)


# after treating extream values, lets see  box plot again

p1 <- ggplot(data, aes(x = "",y=data$hum)) +
  geom_boxplot() + labs(title="box plot on humidity") # plots the results
p2 <- ggplot(data, aes(x = "",y=data$windspeed)) +
  geom_boxplot() + labs(title="box plot on windspeed")


multiplot(p1, p2, cols=1)


# categorical variables
weather = table(data$weathersit)

barplot(weather,xlab='weather',main='barplot of weather')



# visualization of casual, registered, count


# 24 days visualization
d = data %>% filter(between(instant,1,24))

ggplot(d, aes(dateday, count,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))


# casual

data$Month_Yr <- format(as.Date(data$dateday), "%Y-%m")
ggplot(data, aes(Month_Yr,casual,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# registered 

ggplot(data, aes(Month_Yr,data$registered,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# count

ggplot(data, aes(Month_Yr,data$count,group = 1)) + geom_line() 
xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# viz of avg per month

avg_use_month <- data %>% select(casual,registered,count,month,year,Month_Yr) %>% group_by(Month_Yr) %>%
  summarise(casual = mean(casual),registered = mean(registered),count = mean(count))

ggplot(avg_use_month, aes(Month_Yr,registered,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_registered_per_month")

ggplot(avg_use_month, aes(Month_Yr,casual,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_casual_per_month")

ggplot(avg_use_month, aes(Month_Yr,count,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_count_per_month")

# avg use by day

avg_use_day <- data %>% select(casual,registered,weekday) %>% group_by(weekday) %>%
  summarise(casual = mean(casual),registered = mean(registered))

ggplot(avg_use_day, aes(weekday,casual,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_casual_per_day")

ggplot(avg_use_day, aes(weekday,registered,group = 1)) + geom_line() +
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Avg_registered_per_day")



ggplot(d_week_end_day, aes(Month_Yr,group=1)) +geom_line(aes(y = True, colour = "True")) + 
  geom_line(aes(y = False, colour = "False"))+
  xlab("Date") + ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Date vs Count (weekday)")



# humidity vs count
ggplot(data, aes(data$hum,data$count)) + geom_point() +
  xlab("Humidity") + ylab("Count")+theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  ggtitle("Humidity vs Count")


## correlation plots 

library(Hmisc)

numeric_index = sapply(data,is.numeric) #selecting only numeric
library(corrplot)
dev.new(width=5, height=4)
M = cor(data[,numeric_index])

corrplot(M)
dev.off()

#Divide the data into train and test
train_index = sample(1:nrow(data), 0.8 * nrow(data))
train = data[train_index,]
test = data[-train_index,]


# Decision Tree rpart for regression
fit = rpart(Absenteeism.time.in.hours ~., data = train, method = "anova")
summary(fit)
#Predict for new test cases
predictions_DT = predict(fit, test[,-21])
regr.eval(test[,21], predictions_DT)


# random Forest model

RF_Model = randomForest(Absenteeism.time.in.hours ~., data = train,importance = TRUE,na.action=na.roughfix, ntree =100)

Prediction_RF = predict(RF_Model,test[,-21])

regr.eval(test[,21], Prediction_RF)

