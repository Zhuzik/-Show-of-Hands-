train<-read.csv("train.csv", na.strings = "")

# pre-process
# замена пропущенных ответов на 0
write.csv(train,"trainC.csv",na="0", row.names = FALSE)
trainC<-read.csv("trainC.csv")

# замена нереалистичных данных о рождении на NA
table(trainC$YOB)
trainC$YOB[which(trainC$YOB=="1900")]<-NA 
trainC$YOB[which(trainC$YOB=="2011")]<-NA
trainC$YOB[which(trainC$YOB=="2039")]<-NA

# восстановление NA значений 
library(mice)
set.seed(1)
trainV<-complete(mice(trainC))
table(is.na(trainV))

# перекодировка факторов
trainV[trainV=="Yes" | trainV=="Check!" | trainV=="Optimist" | trainV=="Mom" | trainV=="Own" | trainV=="Yay people!" | trainV=="In-person"| trainV=="Yes!"| trainV=="Socialize"| trainV=="Risk-friendly" | trainV=="Mac" | trainV=="Supportive" | trainV=="Tunes" | trainV=="People" | trainV=="Mysterious" | trainV=="Start" | trainV=="Me" | trainV=="A.M."| trainV=="Happy"| trainV=="Hot headed"|trainV=="Standard hours" |  trainV=="Idealist" |  trainV=="Giving" | trainV=="Try first" | trainV=="Science" | trainV=="Public"]<-NA
write.csv(trainV,"trainV.csv",na="1", row.names = FALSE)
trainV<-read.csv("trainV.csv")

trainV[trainV=="No" | trainV=="Only-child" | trainV=="Nope" | trainV=="Pessimist" | trainV=="Dad" | trainV=="Rent" | trainV=="PC"| trainV=="Cautious"| trainV=="Grrr people"| trainV=="TMI" | trainV=="Circumstances"| trainV=="Pragmatist"| trainV=="Receiving" | trainV=="Study first" | trainV=="Private" | trainV=="Art"| trainV=="Odd hours"| trainV=="Cool headed"| trainV=="Umm..."| trainV=="Right"|trainV== "P.M." |  trainV=="End" |  trainV=="Space" | trainV=="Demanding" | trainV== "Technology"| trainV=="Online" | trainV=="Talk"]<-NA
write.csv(trainV,"trainV.csv",na="-1", row.names = FALSE)
trainV<-read.csv("trainV.csv")

trainV[,2]<-as.factor(trainV[,2])
trainV[,3]<-as.factor(trainV[,3])
trainV[,4]<-as.factor(trainV[,4])
trainV[,5]<-as.factor(trainV[,5])
trainV[,6]<-as.factor(trainV[,6])

trainV<-trainV[,c(2:110)]

# предобработка тестовой выборки
test<-read.csv("test.csv",  na.strings = "")

write.csv(test,"testC.csv",na="0", row.names = FALSE)
testC<-read.csv("testC.csv")

table(testC$YOB)
testC$YOB[which(testC$YOB=="2039")]<-NA
testC$YOB[which(testC$YOB=="2013")]<-NA
testC$YOB[which(testC$YOB=="1881")]<-NA
testC$YOB[which(testC$YOB=="1900")]<-NA

set.seed(2312)
testV<-complete(mice(testC))
table(is.na(testV))

testV[testV=="Yes" | testV=="Check!" | testV=="Optimist" | testV=="Mom" | testV=="Own" | testV=="Yay people!" | testV=="In-person"| testV=="Yes!"| testV=="Socialize"| testV=="Risk-friendly" | testV=="Mac" | testV=="Supportive" | testV=="Tunes" | testV=="People" | testV=="Mysterious" | testV=="Start" | testV=="Me" | testV=="A.M."| testV=="Happy"| testV=="Hot headed"|testV=="Standard hours" |  testV=="Idealist" |  testV=="Giving" | testV=="Try first" | testV=="Science" | testV=="Public"]<-NA
write.csv(testV,"testV.csv",na="1", row.names = FALSE)
testV<-read.csv("testV.csv")

testV[testV=="No" | testV=="Only-child" | testV=="Nope" | testV=="Pessimist" | testV=="Dad" | testV=="Rent" | testV=="PC"| testV=="Cautious"| testV=="Grrr people"| testV=="TMI" | testV=="Circumstances"| testV=="Pragmatist"|testV=="Receiving" | testV=="Study first" | testV=="Private" | testV=="Art"| testV=="Odd hours"|testV=="Cool headed"| testV=="Umm..."| testV=="Right"|testV== "P.M." |  testV=="End" |  testV=="Space" | testV=="Demanding" | testV== "Technology"| testV=="Online" | testV=="Talk"]<-NA
write.csv(testV,"testV.csv",na="-1", row.names = FALSE)
testV<-read.csv("testV.csv")

testV[,2]<-as.factor(testV[,2])
testV[,3]<-as.factor(testV[,3])
testV[,4]<-as.factor(testV[,4])
testV[,5]<-as.factor(testV[,5])
testV[,6]<-as.factor(testV[,6])

testV<-testV[,c(2:109)]

# построение моделей
# glm
GLM<-glm(y~., data=trainV, family="binomial")
summary(GLM)
# Если начать убирать незначимые переменные, точность модели падает (но незначительно). Можно уменьшить количество переменных до 30 с уменьшением точности на 2%.  

predictTest<-predict(GLM, type="response", newdata=testV)

# randomForest
library(randomForest)

set.seed(2312)
mtry <- tuneRF(trainRF[-trainRF$y], trainRF$y, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
RF<- randomForest(y~., data=trainRF, mtry=36, keep.forest=TRUE, importance=TRUE, test=testTransformed)
prRF<-predict(RF)
predRFtest<-predict(RF, newdata=testTransformed)

predict_all<-(0.4*predictTest+0.6*predRFtest)
result = data.frame(UserID = test$UserID, Probability1 = predict_all)
write.csv(result, "result.csv", row.names =F)
