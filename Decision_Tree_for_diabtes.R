data <- read.csv("C:/Users/LENOVO/Desktop/R programs/Logistic_Regressioin_Diabetes_dataset.csv")

##############################################################
#                    1.  DATA TYPE                           #
##############################################################

head(data)
names(data)
str(data)

data$Pregnancies <- as.numeric(data$Pregnancies)
data$Glucose <- as.numeric(data$Glucose)
data$BloodPressure <- as.numeric(data$BloodPressure)
data$SkinThickness <-as.numeric(data$SkinThickness)
data$Insulin <- as.numeric(data$Insulin)
data$Age <- as.numeric(data$Age)
data$Outcome <- as.factor(data$Outcome)
str(data)

summary(data)

##############################################################
#                    2.   EDA/MV                             #
##############################################################

sapply(data, function(x) sum(is.na(x)))
boxplot(data)

##############################################################
#                  3.   DATA-PREPROCESSING                   #
##############################################################


########################   Pregnancies   #####################
boxplot(data$Pregnancies)
summary(data$Pregnancies)
upper <- 6.0 + 1.5 * IQR(data$Pregnancies)
upper

data$Pregnancies[data$Pregnancies > upper] <- upper
boxplot(data$Pregnancies)

########################   Glucose  ##########################

boxplot(data$Glucose)
summary(data$Glucose)


lower <- 99 - 1.5 * IQR(data$Glucose)
lower
data$Glucose[data$Glucose < lower] <- lower
boxplot(data$Glucose)

######################    BloodPressure    ###################

boxplot(data$BloodPressure)
summary(data$BloodPressure)
upper <- 80 + 1.5 * IQR(data$BloodPressure)
upper


data$BloodPressure[data$BloodPressure > upper] <- upper
boxplot(data$BloodPressure)

lower <- 62 - 1.5 * IQR(data$BloodPressure)
lower
data$BloodPressure[data$BloodPressure < lower] <- lower
boxplot(data$BloodPressure)

######################    SkinThickness    ###################

boxplot(data$SkinThickness)
summary(data$SkinThickness)
upper <- 32 + 1.5 * IQR(data$SkinThickness)
upper


data$SkinThickness[data$SkinThickness > upper] <- upper
boxplot(data$SkinThickness)

#######################    Insulin  ##########################
boxplot(data$Insulin)
summary(data$Insulin)
upper <- 127 + 1.5 * IQR(data$Insulin)
upper


data$Insulin[data$Insulin > upper] <- upper
boxplot(data$Insulin)


#######################    BMI    ###########################

boxplot(data$BMI)
summary(data$BMI)
upper <- 36.60 + 1.5 * IQR(data$BMI)
upper


data$BMI[data$BMI > upper] <- upper
boxplot(data$BMI)

lower <- 27.30 - 1.5 * IQR(data$BMI)
lower
data$BMI[data$BMI < lower] <- lower
boxplot(data$BMI)

#################   DiabetesPedigreeFunction    ##############

boxplot(data$DiabetesPedigreeFunction)
summary(data$DiabetesPedigreeFunction)
upper <- 0.6262 + 1.5 * IQR(data$DiabetesPedigreeFunction)
upper


data$DiabetesPedigreeFunction[data$DiabetesPedigreeFunction > upper] <- upper
boxplot(data$DiabetesPedigreeFunction)

######################## Age #################################

boxplot(data$Age)
summary(data$Age)
upper <- 41 + 1.5 * IQR(data$Age)
upper


data$Age[data$Age > upper] <- upper
boxplot(data$Age)

boxplot(data)

##############################################################
#                4.   DATA PARTITION                         #
##############################################################

library(caret)
Train <- createDataPartition(data$Outcome , p= 0.7, list = FALSE)
training <- data[ Train, ]
testing <- data [-Train, ]

###############################################################
#                    MODEL BUILDING                           #
###############################################################

library(tree)
model1 = tree(Outcome ~ ., data = data)
model1
plot(model1)
text(model1,pretty=0)

###############################################################
#                    PRUNING                                  #
###############################################################

cv.data = cv.tree(model1, FUN=prune.misclass)
plot(cv.data$size,cv.data$dev , type="b")
prune.data = prune.misclass(model1 , best = 3)
plot(prune.data)
text(prune.data, pretty=0)

# prediction on trainig  data 

data$Predicted = predict(model1,data,type ="class")
table(data$Predicted,data$Outcome)
library(caret)
confusionMatrix(data$Predicted,data$Outcome)

#prediction on testing data


testing$Predicted<-predict(model1,testing,type="class")
table(testing$Predicted,testing$Outcome)
library(caret)
confusionMatrix(testing$Predicted,testing$Outcome)

