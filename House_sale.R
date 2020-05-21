
#Set Working directory
setwd('J:/ML')

#Reading test and train data
data_train <- read.csv("train_datarent.csv", stringsAsFactors=F)
data_test <- read.csv("test_datarent.csv", stringsAsFactors=F)

#loading all the required libraries
library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr')
library('randomForest') 
library('data.table')
library('gridExtra')
install.packages('corrplot')
library('corrplot') 
library('e1071')

#checking the dimension of training dataset
dim(data_train)

#checking the structure of training dataset
str(data_train)

#checking the summary of training dataset
summary(data_train)

#checking the sum test dataset
str(data_test)

#checking number of character variables in the training dataset
sum(sapply(data_train[,1:81], typeof)=='character')

#checking number of integer variables in the training dataset
sum(sapply(data_train[,1:81], typeof)=='integer')

#seperating integer variables from the training dataset
int_names <- names(data_train)[which(sapply(data_train, is.numeric))]

data_train_int <- data_train[int_names]

#summary of integer dataset
summary(data_train[,sapply(data_train[,1:81], typeof)=='integer'])

#Number of missing values in training dataset
sum(is.na(data_train))/(nrow(data_train)*ncol(data_train))

#Number of missing value in test dataset
sum(is.na(data_test))/(nrow(data_test)*ncol(data_test))

#Number of duplicate rows in training dataset
nrow(data_train)-unique(nrow(data_train))

#Number of duplicate rows in test dataset
nrow(data_test)-unique(nrow(data_test))

#Adding extra column to make it easier to join training and test dataset
data_train$Isittrain <- T
data_test$Isittrain <- F

#Adding extra column of dependent variable to be predicted in the test set
data_test$SalePrice <- NA

#Combining the two datasets
data <- rbind(data_train, data_test)

#Structure of data dataset after combining
str(data)

#Boxplot visualization 
ggplot(data_train, aes(x = Neighborhood, y = SalePrice, main='BoxPlot')) +
  geom_boxplot() + geom_hline(aes(yintercept=80), colour='blue', lwd=1.5) +  scale_y_continuous() +
  theme_bw()

#Histogram visualization
hist(data_train$SalePrice, main = 'Histogram of SalePrice')

#Scatter Plot visualization
ggplot(data_train,aes(y=SalePrice,x=GrLivArea))+geom_point(col='red')

#Removing Outliers from GrLivArea variable
summary(data_train$GrLivArea)

data_train <- data_train[data_train$GrLivArea<=4000,]

colSums(sapply(data[1:80], is.na))

#REmoval of missing values by adding it to characterstic 'None' OR taking the mean OR by taking the meadian OR by making it 0

mean_MasVnrArea <- mean(data$MasVnrArea,na.rm=T)
data$MasVnrArea[which(is.na(data$MasVnrArea))] <- mean_MasVnrArea

data$Alley1 <- as.character(data$Alley)
data$Alley1[which(is.na(data$Alley))] <- "None"
table(data$Alley1)
data$Alley <- as.factor(data$Alley1)
data <- subset(data,select = -Alley1)


data$MasVnrType1 <- as.character(data$MasVnrType)
data$MasVnrType1[which(is.na(data$MasVnrType))] <- "None"
data$MasVnrType <- as.factor(data$MasVnrType1)
data <- subset(data,select = -MasVnrType1)
table(data$MasVnrType)


meadian_LotFrontage <- median(data$LotFrontage,na.rm = T)
data$LotFrontage[which(is.na(data$LotFrontage))] <- meadian_LotFrontage


data$FireplaceQu1 <- as.character(data$FireplaceQu)
data$FireplaceQu1[which(is.na(data$FireplaceQu))] <- "None"
data$FireplaceQu <- as.factor(data$FireplaceQu1)
data <- subset(data,select = -FireplaceQu1)


data$PoolQC1 <- as.character(data$PoolQC)
data$PoolQC1[which(is.na(data$PoolQC))] <- "None"
data$PoolQC <- as.factor(data$PoolQC1)
data <- subset(data,select = -PoolQC1)


data$Fence1 <- as.character(data$Fence)
data$Fence1[which(is.na(data$Fence))] <- "None"
data$Fence <- as.factor(data$Fence1)
data <- subset(data,select = -Fence1)


data$MiscFeature1 <- as.character(data$MiscFeature)
data$MiscFeature1[which(is.na(data$MiscFeature))] <- "None"
data$MiscFeature <- as.factor(data$MiscFeature1)
data <- subset(data,select = -MiscFeature1)


data$GarageType1 <- as.character(data$GarageType)
data$GarageType1[which(is.na(data$GarageType))] <- "None"
data$GarageType <- as.factor(data$GarageType1)
data <- subset(data,select = -GarageType1)


data$GarageYrBlt[which(is.na(data$GarageYrBlt))] <- 0 


data$GarageFinish1 <- as.character(data$GarageFinish)
data$GarageFinish1[which(is.na(data$GarageFinish))] <- "None"
data$GarageFinish <- as.factor(data$GarageFinish1)
data <- subset(data,select = -GarageFinish1)


data$GarageQual1 <- as.character(data$GarageQual)
data$GarageQual1[which(is.na(data$GarageQual))] <- "None"
data$GarageQual <- as.factor(data$GarageQual1)
data <- subset(data,select = -GarageQual1)


data$GarageCond1 <- as.character(data$GarageCond)
data$GarageCond1[which(is.na(data$GarageCond))] <- "None"
data$GarageCond <- as.factor(data$GarageCond1)
data <- subset(data,select = -GarageCond1)


data$BsmtQual1 <- as.character(data$BsmtQual)
data$BsmtQual1[which(is.na(data$BsmtQual))] <- "None"
data$BsmtQual <- as.factor(data$BsmtQual1)
data <- subset(data,select = -BsmtQual1)


data$BsmtCond1 <- as.character(data$BsmtCond)
data$BsmtCond1[which(is.na(data$BsmtCond))] <- "None"
data$BsmtCond <- as.factor(data$BsmtCond1)
data <- subset(data,select = -BsmtCond1)


data$BsmtExposure1 <- as.character(data$BsmtExposure)
data$BsmtExposure1[which(is.na(data$BsmtExposure))] <- "None"
data$BsmtExposure <- as.factor(data$BsmtExposure1)
data <- subset(data,select = -BsmtExposure1)


data$BsmtFinType11 <- as.character(data$BsmtFinType1)
data$BsmtFinType11[which(is.na(data$BsmtFinType1))] <- "None"
data$BsmtFinType1 <- as.factor(data$BsmtFinType11)
data <- subset(data,select = -BsmtFinType11)


data$BsmtFinType21 <- as.character(data$BsmtFinType2)
data$BsmtFinType21[which(is.na(data$BsmtFinType2))] <- "None"
data$BsmtFinType2 <- as.factor(data$BsmtFinType21)
data <- subset(data,select = -BsmtFinType21)



data$Electrical1 <- as.character(data$Electrical)
data$Electrical1[which(is.na(data$Electrical))] <- "None"
data$Electrical <- as.factor(data$Electrical1)
data <- subset(data,select = -Electrical1)


data_train$BsmtFinType <- as.character(data_train$BsmtFinType2)
data_train$BsmtFinType[which(is.na(data_train$BsmtFinType))] <- "None"
data_train$BsmtFinType2 <- as.factor(data_train$BsmtFinType)
data_train <- subset(data_train,select = -BsmtFinType)

install.packages('corrplot')

library(corrplot)

require(corrplot)

corrplot(data_train_int)

str(data_train_int)

#Finding the correlation of the variables
correlations <- cor(na.omit(data_train_int[,-1]))

row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations)

#Converting character into factor for variables
data$MSZoning<- factor(data$MSZoning)
data$Street <- factor(data$Street)
data$LotShape <-factor(data$LotShape )
data$LandContour<-factor(data$LandContour)
data$Utilities<-factor(data$Utilities)
data$LotConfig<-factor(data$LotConfig)
data$LandSlope<-factor(data$LandSlope)
data$Neighborhood<-factor(data$Neighborhood)
data$Condition1<-factor(data$Condition1)
data$Condition2<-factor(data$Condition2)
data$BldgType<-factor(data$BldgType)
data$dataStyle<-factor(data$dataStyle)
data$RoofStyle<-factor(data$RoofStyle)
data$RoofMatl<-factor(data$RoofMatl)
data$Exterior1st<-factor(data$Exterior1st)
data$Exterior2nd<-factor(data$Exterior2nd)
data$ExterQual<-factor(data$ExterQual)
data$ExterCond<-factor(data$ExterCond)
data$Foundation<-factor(data$Foundation)
data$Heating<-factor(data$Heating)
data$HeatingQC<-factor(data$HeatingQC)
data$CentralAir<-factor(data$CentralAir)
data$KitchenQual<-factor(data$KitchenQual)
data$Functional<-factor(data$Functional)
data$PavedDrive<-factor(data$PavedDrive)
data$SaleType<-factor(data$SaleType)
data$SaleCondition<-factor(data$SaleCondition)

str(data)

#Seperating data dataset into training and test
data_train <- data[data$Isittrain==T,]
data_test <- data[data$Isittrain==F,]

data_train <- select(data_train, -Isittrain)
data_test <- select(data_test, -Isittrain)

#Feature selection using Boruta method
library(Boruta)

set.seed(1234)

colSums(sapply(data_train[1:80], is.na))

boruta <- Boruta(SalePrice~., data=data_train, doTrace=2, maxRuns=500)

table(is.na(data_train))

plot(boruta, las=2)

#Getting Important variables to be used for the model and removing unimportant variables
bor <- TentativeRoughFix(boruta)
getNonRejectedFormula(boruta)

#######RANDOM FOREST METHOD########################
library(randomForest)

#RandomForest Model
model <- randomForest(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Alley + 
                              LotShape + LandContour + LandSlope + Neighborhood + BldgType + 
                              HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
                              RoofStyle + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + 
                              ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + 
                              BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + 
                              Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + 
                              FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
                              TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + 
                              GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + 
                              GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + SaleCondition,
                            data = data_train)

#Predicting the values of dependent variable using test dataset
prediction <- predict(house_model,test)


#########XGBOOST METHOD#####################
library(caret)
library(xgboost)
library(metrics)

custom_summary <- function(data_train, lev=NULL, model=NULL)(
    out = rmsle(data_train[, "obs"], data_train[, "pred"])
    names(out) = c("rmsle")
    out
)


#Using cross validation with 5 folds
control <- trainControl(method="cv", number=5, summaryFunction = custom_summary)


#Testing values for boosting rounds,tree depth, learning rate
grid <- expand.grid(nrounds=c(1000, 1200, 1500), max_depth=c(6, 8, 10),
                             ets=c(0, 0.025, 0.01),
                              gamma=c(0,1),
                              colsample_bytree=c(1),
                              min_child_weight=c(1),
                              subsample=c(0, 8))



set.seed(1234)

xgb_model <- train(Saleprice~MSSubClass + MSZoning + LotFrontage + LotArea + Alley + 
                     LotShape + LandContour + LandSlope + Neighborhood + BldgType + 
                     HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
                     RoofStyle + Exterior1st + Exterior2nd + MasVnrType + MasVnrArea + 
                     ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + 
                     BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + 
                     Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + 
                     FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
                     TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType + 
                     GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + 
                     GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + SaleCondition,
                   data=train,
                   method="xgbtree"
                   trControl=control
                   tuneGrid=grid,
                   metric='rmsle',
                   maximize=F)

#Checking model results
xgb_model$results


xgb_model$bestTune

#Predicting values
prediction <- predict(xgb_model, newdata=data_test)

#Writing it into csv file for submission
submit <- data.frame(Id=test$Id,SalePrice=prediction)
write.csv(submit,file="House_Price.csv",row.names=F)
