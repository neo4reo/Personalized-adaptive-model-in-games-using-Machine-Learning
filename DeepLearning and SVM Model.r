library(tidyverse)
library(lattice)
install.packages("lattice")
library(keras)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Predict using Deep Learning

install_keras()
setwd("C:/Users/nrangara/Downloads/WorldOfWarcraft/output/")
setwd("A:/Dataset/New folder")
data<- read_csv("wowah_data.csv")

data<- data%>% mutate(Date=as.Date(data$timestamp, "%m/%d/%y"))

tempdata<-data[data$Date>"2006-12-31",]

# number of 2008 interactions for threshold
threshold <- 0 
#tempdata<-data%>%filter(Date>"2006-12-31")
userLevel<- data %>%
  mutate(n2008=ifelse(Date>as.Date("2007-12-31"),1L,0L)) %>%
  group_by(char) %>%
  summarise(n=n(), 
            n2008=sum(n2008),
            minGuild=min(guild), 
            maxGuild=max(guild), 
            minDate = min(Date), 
            maxDate = max(Date), 
            maxLevel=max(level)) %>%
  arrange(desc(n)) %>%
  mutate(TimeDiff = as.numeric(difftime(maxDate,minDate, units="days")),
         y2008=ifelse(as.integer(n2008)>threshold,1,0))


# create x variables (pre Nov 1)
x <- filter(data, Date<"2008-01-01") %>%
  group_by(char) %>%
  summarise(n=n(), 
            minGuild=min(guild), 
            maxGuild=max(guild), 
            minDate = min(Date), 
            maxDate = max(Date), 
            maxLevel=max(level)) %>%
  arrange(desc(n)) %>%
  mutate(TimeDiff = as.numeric(difftime(maxDate,minDate, units="days")))

# create y variable
y <- data %>%
  mutate(n2008=ifelse(Date>=as.Date("2008-01-01"),1L,0L)) %>%
  group_by(char) %>%
  summarise(yCount = sum(n2008)) %>%
  mutate(y=ifelse(yCount>threshold,1,0))

dataset <- merge(x,y,by="char")

#vectorArray<-as.vector(userLevel)
#myts <- ts(vectorArray, start=c(2005, 12), end=c(2009, 1), frequency=12)
#plot(myts)

require(ggplot2)

data%>% group_by(char)%>%

dataset[c("minDate","maxDate")]<-list(NULL)

set.seed(101)
sample <- sample.int(n = nrow(dataset), size = floor(.80*nrow(dataset)), replace = F)
train <- dataset[sample, ]
test  <- dataset[-sample, ]
x_train<-train[,2:7]
#y_train<-as.factor(train[,10])
y_train<-train[,8]
x_test<-test[,2:7]
#y_test<-as.factor(test[,10])
y_test<-test[,8]


x_train<-as.matrix(x_train)
y_train<-as.matrix(y_train)
x_test<-as.matrix(x_test)
y_test<-as.matrix(y_test)

# keras deep learning

dim(x_train) <- c(nrow(x_train), 6)
dim(x_test) <- c(nrow(x_test), 6)

y_train <- to_categorical(y_train, 2)
y_test <- to_categorical(y_test, 2)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 512, activation = "relu", input_shape = c(6)) %>% 
  layer_dropout(rate = 0.6) %>%
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = "softmax")

optimizer <- optimizer_rmsprop(lr = 0.01)

summary(model)
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 50,batch_size = nrow(x_test),
  validation_split = 0.2
)
plot(history)

model %>% evaluate(x_test, y_test,verbose = 0)



#Predict using SVM with the same x and y
library("e1071")

# http://rischanlab.github.io/SVM.html

svm_tune <- tune(svm, y_train~.,data=cbind(x_train,y_train),
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)

#test<-test[-nrow(test),]
#attach(iris)
#head(iris,5)
nrow(x_train)
#svm_model <- svm(as.matrix(train$y2008)~as.matrix(train[2:7]),data=as.matrix(train))
svm_model <- svm(y_train~.,data=cbind(x_train,y_train))
summary(svm_model)
Prediction <- predict(svm_model,x_test)
#table(Prediction,as.matrix(test$y2008))

## Predictions

accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}
# function to compute precision
precision <- function(ypred, y){
  tab <- table(ypred, y)
  return((tab[2,2])/(tab[2,1]+tab[2,2]))
}
# function to compute recall
recall <- function(ypred, y){
  tab <- table(ypred, y)
  return(tab[2,2]/(tab[1,2]+tab[2,2]))
}


#predY <- ifelse(Prediction > mean(ifelse(y_train=="1",1,0)),1L,0L)
table(Prediction, y_test)

# accuracy measures
accuracy(Prediction, y_test)
precision(Prediction, y_test)
recall(Prediction, y_test)

# F1 measure








error<-test$y2008-Prediction
svrPredictionRMSE <- rmse(error)
point(test$y2008, Prediction, col = "red", pch=4)

predictionTest <- predict(svm_model, as.matrix(x_test))
points(as.matrix(test$y2008), predictionTest, col = "red", pch=2)
plot(svm_model,as.matrix(train))
tab

svm_model %>% evaluate(x_test, y_test,verbose = 0)

pred <- predict(svm_model,x_train)
system.time(pred <- predict(svm_model,x_train))
table(pred,y_train)
svm_tune <- tune(svm, train.x=x_train, train.y=y_train, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))



dtdf<-filter(userLevel,n<17000)
ggplot(data=dtdf,mapping = aes(x=dtdf$n))+
  geom_histogram() +
  labs(x="Number of Interactions by user",y="Frequency of User Interactions",title="Histogram")
summary(data)


#userLevel$charclass<-ifelse((userLevel$char==data$char)
 #                           &&(userLevel$maxDate==data$Date)&&(userLevel$maxLevel==data$level)&&(userLevel$maxGuild==data$guild),data$charclass)


#Relation between guild and # of days online
plot(userLevel$n, userLevel$maxGuild, main="guild and # of days online",
     xlab="# of days online", ylab="guild", ylim=c(-1, ymax))

#Relation between charclass and # of days online
data %>%
  group_by(Date, charclass) %>%
  summarise(Count=n()) %>%
  ggplot() +
  geom_line(aes(x = Date, y = Count)) + 
  facet_wrap(~charclass)


data %>% mutate(datetime = strptime(data$timestamp, "%m/%d/%y %H:%M:%S")) %>% head(n=10)

mutate(datetime = strptime(data$timestamp[1], "%m/%d/%y %H:%M:%S"))

#any way to predict fade-out games based on months


