titanic <- read_csv("IST 707/train.csv")
str(titanic)
 
titanic$PassengerId=factor(titanic$PassengerId)
str(titanic)

length(which(is.na(titanic$PassengerId)))
length(which(is.na(titanic$Survived)))
length(which(is.na(titanic$Pclass)))
length(which(is.na(titanic$Name)))
length(which(is.na(titanic$Sex)))
length(which(is.na(titanic$Age)))
length(which(is.na(titanic$SibSp)))
length(which(is.na(titanic$Parch)))
length(which(is.na(titanic$Ticket)))
length(which(is.na(titanic$Fare)))
length(which(is.na(titanic$Cabin)))
length(which(is.na(titanic$Embarked)))

myVars=c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Embarked")

titanic_new <- titanic[myVars]
titanic_new$Age[is.na(titanic_new$Age)] <- mean(titanic_new$Age, na.rm = TRUE)
titanic_new2 <- titanic_new[complete.cases(titanic_new),]

table(titanic$Pclass)
mean(titanic$Pclass)

titanic_Pclass3 <- titanic[titanic$Pclass==3,]
boxplot(titanic_Pclass3$Fare)

table(titanic$Embarked, titanic$Survived)

titanic_new3 <- aggregate(Fare ~ Sex, data=titanic, FUN = mean)

fare <- cut(titanic$Fare, breaks = c(0,10,20,30,40,50,60,70,80,Inf), labels = c("0","10","20","30","40","50","60","70","80","Over 80"))
plot(titanic$Fare, log(titanic$Fare))

zscoreFare<- scale(titanic$Fare, center = TRUE, scale = TRUE)

plot(titanic$Fare, zscoreFare)

Min_max <- (titanic$Fare-min(titanic$Fare,na.rm=TRUE))/(max(titanic$Fare,na.rm=TRUE)-min(titanic$Fare,na.rm=TRUE))

plot(Min_max, titanic$Fare)

randomsample <- titanic[sample(1:nrow(titanic), 100, replace = FALSE),]

nrow(titanic)/100
ss=titanic[seq(1,nrow(titanic),8.91),]
nrow(ss)

