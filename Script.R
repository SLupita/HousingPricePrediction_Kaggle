## Automatic loading of the libraries
list.of.packages <- c("tidyverse", "caret","data.table","dummies","corrplot",
                      "robustHD","gbm","gridExtra","missForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(caret)
library(data.table)
library(dummies)
library(corrplot)
library(missForest)
library(gbm)
library(robustHD)
library(gridExtra)

## Loading the data
unzip("house-prices-advanced-regression-techniques.zip", exdir="house-prices-advanced-regression-techniques")

## Reading files after unzipping
list_of_files <- list.files("./house-prices-advanced-regression-techniques", full.names = TRUE)

# Loading train and test datasets
train <- read.csv(list_of_files[4])
test <- read.csv(list_of_files[3])

##################################################################
################### Outlier treatment ############################
##################################################################
# Removing few outliers
train <- train[train$GrLivArea<5000,]
train <- train[train$LotFrontage<200 | is.na(train$LotFrontage)==TRUE,]

# Keeping SalePrice aside and removing the same from train set before
# combining train and test datasets
SalePrice <- train$SalePrice
total <- rbind(train[-which(names(train) %in% c("SalePrice"))],test)


## Converting factor variables which were initially stored as numeric
total$MSSubClass <- as.factor(total$MSSubClass)
total$OverallQual <- as.factor(total$OverallQual)
total$OverallCond <- as.factor(total$OverallCond)
total$MoSold <- as.factor(total$MoSold)
total$YrSold <- as.factor(total$YrSold)

# Removing variables which are highly correlated with other variables (cor >0.8)
total <- total[,-which(names(total) %in% c("GarageArea", "TotRmsAbvGrd", 
                                           "X1stFlrSF", "GarageYrBlt"))]

# Removing redundant variable
total <- total[,-which(names(total) %in% c("MSSubClass"))]

##################################################################
########## Removing/replacing NA values in Train dataset #########
##################################################################

#MSZoning
total[is.na(total$MSZoning)==TRUE,]$MSZoning <- "RM"

#Electrical
total[is.na(total$Electrical)==TRUE,]$Electrical <- "SBrkr"

#Exterior1st and Exterior2nd
total[is.na(total$Exterior1st)==TRUE,]$Exterior1st <- "Wd Sdng"
total[is.na(total$Exterior2nd)==TRUE,]$Exterior2nd <- "Wd Sdng"

#SaleType
total[is.na(total$SaleType)==TRUE,]$SaleType <- "WD"

#GarageFinish when GarageType value is not NA
total[is.na(total$GarageFinish)==TRUE & !is.na(total$GarageType)==TRUE,]$GarageFinish <- "Fin"

# GarageCars
x <- predict(lm(GarageCars~OverallQual+GrLivArea+YearBuilt,total),newdata=total[is.na(total$GarageCars)==TRUE,])
total[is.na(total$GarageCars)==TRUE,]$GarageCars <- round(x)
rm(x)

#BsmtFinSF1 and BsmtUnfSF
total[is.na(total$BsmtFinSF1)==TRUE,]$BsmtFinSF1 <- 0
total[is.na(total$BsmtUnfSF)==TRUE,]$BsmtUnfSF <- 0

#Alley
total$Alley <- as.character(total$Alley)
total$Alley[is.na(total$Alley)] <- "None"
total$Alley <- as.factor(total$Alley)

#Fence
total$Fence <- as.character(total$Fence)
total$Fence[is.na(total$Fence)] <- "None"
total$Fence <- as.factor(total$Fence)

#GarageType when No garage
total$GarageType <- as.character(total$GarageType)
total$GarageType[is.na(total$GarageType)] <- "None"
total$GarageType <- as.factor(total$GarageType)

#GarageFinish when No garage
total$GarageFinish <- as.character(total$GarageFinish)
total$GarageFinish[is.na(total$GarageFinish)] <- "None"
total$GarageFinish <- as.factor(total$GarageFinish)

#PoolQC
total$PoolQC <- as.character(total$PoolQC)
total$PoolQC[is.na(total$PoolQC)] <- "None"
total$PoolQC <- as.factor(total$PoolQC)

#MiscFeature
total$MiscFeature <- as.character(total$MiscFeature)
total$MiscFeature[is.na(total$MiscFeature)] <- "None"
total$MiscFeature <- as.factor(total$MiscFeature)

# FirePlaceQu
total$FireplaceQu <- as.character(total$FireplaceQu)
total$FireplaceQu[is.na(total$FireplaceQu)] <- "None"
total$FireplaceQu <- as.factor(total$FireplaceQu)

#KitchenQual
total$KitchenQual <- as.character(total$KitchenQual)
total$KitchenQual[is.na(total$KitchenQual)] <- "TA"
total$KitchenQual <- as.factor(total$KitchenQual)

#Basement related variables when no basement
bsmt_ind <- which(is.na(total$BsmtExposure)==TRUE & is.na(total$BsmtQual)==TRUE & is.na(total$BsmtFinType1)==TRUE)

#BsmtExposure
total$BsmtExposure <- as.character(total$BsmtExposure)
total[bsmt_ind,]$BsmtExposure <- "None"
total$BsmtExposure <- as.factor(total$BsmtExposure)

#BsmtQual
total$BsmtQual <- as.character(total$BsmtQual)
total[bsmt_ind,]$BsmtQual <- "None"
total$BsmtQual <- as.factor(total$BsmtQual)

#BsmtFinType1
total$BsmtFinType1 <- as.character(total$BsmtFinType1)
total[bsmt_ind,]$BsmtFinType1 <- "None"
total$BsmtFinType1 <- as.factor(total$BsmtFinType1)

# BsmtExposure and BsmtQual when other basement related variables are not NA 
# (means there is basement)
total[is.na(total$BsmtExposure)==TRUE,]$BsmtExposure <- "No"
total[is.na(total$BsmtQual)==TRUE,]$BsmtQual <- "TA"

#MasVnrType and MasVnrArea
total$MasVnrType <- as.character(total$MasVnrType)

#Setting MasVnrType when MasVnrArea is also NA
total[is.na(total$MasVnrType)==TRUE & is.na(total$MasVnrArea)==TRUE,]$MasVnrType <- "None"
total$MasVnrArea[is.na(total$MasVnrArea)] <- 0

#Setting MasVnrType to valid value when MasVnrArea is not NA
total$MasVnrType[is.na(total$MasVnrType)] <- "BrkFace"
total$MasVnrType <- as.factor(total$MasVnrType)

#TotalBsmtSF
total$TotalBsmtSF[is.na(total$TotalBsmtSF)] <- 0 

#BsmtFullBath
total$BsmtFullBath[is.na(total$BsmtFullBath)] <- 0

#LotFrontage
imputed_data_mf <- missForest(total)
newLF2 <- rowMeans(as.matrix(imputed_data_mf$ximp$LotFrontage))
total$LotFrontage <- newLF2

# Identifying variables with near zero variability
ind <- nearZeroVar(total)

# Removing variables with near zero variability except "OpenPorchSF" as it has
# good correlation value with SalePrice
ind <- ind[-17]

#Removing variables with near zero variances
total <- total[,-ind]

#Standardizing numeric variables so that different ranges are treated the same way
total$LotFrontage <- standardize(total$LotFrontage)
total$LotArea <- standardize(total$LotArea)
total$MasVnrArea <- standardize(total$MasVnrArea)
total$BsmtFinSF1 <- standardize(total$BsmtFinSF1)
total$BsmtUnfSF <- standardize(total$BsmtUnfSF)
total$TotalBsmtSF <- standardize(total$TotalBsmtSF)
total$X2ndFlrSF <- standardize(total$X2ndFlrSF)
total$GrLivArea <- standardize(total$GrLivArea)
total$WoodDeckSF <- standardize(total$WoodDeckSF)
total$OpenPorchSF <- standardize(total$OpenPorchSF)

# Label encoding few factor variables
# Converting the levels of all quality factor variables to 1-5
levels <- c("Po","Fa","TA","Gd","Ex")
total$ExterQual <- as.numeric(factor(total$ExterQual, levels = levels))
total$ExterCond <- as.numeric(factor(total$ExterCond, levels = levels))
total$BsmtQual <- as.numeric(factor(total$BsmtQual, levels = levels))
total$BsmtQual[is.na(total$BsmtQual)] <- 0
total$KitchenQual <- as.numeric(factor(total$KitchenQual, levels = levels))
total$FireplaceQu <- as.numeric(factor(total$FireplaceQu, levels = levels))
total$FireplaceQu[is.na(total$FireplaceQu)] <- 0
total$HeatingQC <- as.numeric(factor(total$HeatingQC, levels = levels))
total$OverallQual <- as.numeric(total$OverallQual)
total$OverallCond <- as.numeric(total$OverallCond)

# CentralAir has 2 levels, so making them binary
total$CentralAir <- as.numeric(factor(total$CentralAir, levels = c("N","Y")))-1

total$LotShape <- as.numeric(factor(total$LotShape, levels = c("IR3","IR2","IR1","Reg")))

total$BsmtExposure <- as.numeric(factor(total$BsmtExposure, levels = c("No","Mn","Av","Gd")))
total$BsmtExposure[is.na(total$BsmtExposure)] <- 0

levels <- c("Unf","LwQ","Rec","BLQ","ALQ","GLQ")
total$BsmtFinType1 <- as.numeric(factor(total$BsmtFinType1, levels = levels))
total$BsmtFinType1[is.na(total$BsmtFinType1)] <- 0

total$GarageFinish <- as.numeric(factor(total$GarageFinish, levels = c("Unf","RFn","Fin")))
total$GarageFinish[is.na(total$GarageFinish)] <- 0

# Collecting only categorical variables for applying one-hot encoding
nums <- sapply(total, is.numeric) 
categorical <- total[,!nums]

#Converting before using the function "dummy.data.frame
total <- data.frame(total)

# Hot encoding of factor variables for better interpretation by machine learning algorithms
total <- dummy.data.frame(total, names=colnames(categorical),sep="_")

# Dividing total data set back in to train and test data sets
set.seed(1)
inTest <- which(total$Id >= 1461)
train <- total[-inTest,]
test <- total[inTest,]

# Adding SalePrice back in train data set
train <- cbind(train,SalePrice=SalePrice)

##############Dividing train data set into training and testing sets####
#Removing Id before applying regression model
train <- train[,-which(names(train) %in% c("Id"))]

# Divide this train dataset into training and testing datasets
set.seed(1)
inTrain <- createDataPartition(train$SalePrice, p=0.7, list=FALSE)

# Creating training and testing datasets
training <- train[inTrain,]
testing <- train[-inTrain,]

################## Machine Learning algorithms#################
# Running GBM with parameters finalized after hypertuning

# For reproducibility
set.seed(1)

# training the GBM model
gbm.fit.final <- gbm(
    formula = SalePrice ~ .,
    distribution = "gaussian",
    data = training,
    n.trees = 198,
    interaction.depth = 7,
    shrinkage = 0.05,
    n.minobsinnode = 5,
    bag.fraction = 0.65,
    train.fraction = 1,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
)

#Prediction on testing dataset for cross-validation
yhat <- predict(gbm.fit.final, newdata = testing, n.trees = gbm.fit.final$n.trees)

# RMSE
rmse_gbm_final <- RMSE(log(yhat),log(testing$SalePrice))

#Printing RMSE
print(rmse_gbm_final)

# Finally applying model on test data for submission
# For reproducibility
set.seed(1)

# The final model on train dataset
gbm.fit.final <- gbm(
    formula = SalePrice ~ .,
    distribution = "gaussian",
    data = train,
    n.trees = 198,
    interaction.depth = 7,
    shrinkage = 0.05,
    n.minobsinnode = 5,
    bag.fraction = .65,
    train.fraction = 1,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
)

# Prediction on test dataset
gbm_predict <- predict(gbm.fit.final, test, n.trees=gbm.fit.final$n.trees)

# Combining Id and SalePrice
final_test <- cbind(test$Id,gbm_predict)

#Writing to CSV file
write.csv(final_test,"final.csv")