# R program - Linear Regression and Lasso Regression

# Read the datasets
data_train <- read.csv("/Project data analysis for business/train.csv")
data_test <- read.csv("/Project data analysis for business/test.csv")

# Split Train and Test using sample()
train <- sample(1:nrow(data_train), nrow(data_train)/2)
test <- (-train)

# Exploratory Data Analysis: verify correlation between the response SalePrice and every other predictor
pdf("my_plot.pdf")
par(mfrow = c(4,3))
for(i in 1:ncol(data_train)){
plot(data_train[,i], data_train$SalePrice, xlab=names(data_train[i]),
ylab="SalePrice", pch=20)
}
dev.off()

# Print how many NAs there are inside each predictor column
for (i in 1:ncol(data_train)){
    n <- sum(is.na(data_train[,i]))
    if (n > 100){
        cat("[", i, "]: ", n, "\n")
    }
}

# Remove predictors that shows a number of NAs > 500.
indexes_NA <- c() #boolean vector: TRUE means that
predictor i has NA values
for (i in 1:ncol(data_train)){
    n <- sum(is.na(data_train[,i]))
    if (n > 500){
        indexes_NA <- c(indexes_NA, TRUE)
    }
    else{ indexes_NA <- c(indexes_NA, FALSE) }
}
#remove:
data_train <- data_train[,!indexes_NA]

# Remove the predictor "Utilities", which contains only one level of factor.
data_train$Utilities <- NULL

# Solve other NAs issues
data_train$GarageFinish <- as.character(data_train$GarageFinish)
data_train$GarageFinish[is.na(data_train$GarageFinish)] <- "No"
data_train$GarageFinish <- as.factor(data_train$GarageFinish)
data_train$GarageQual <- as.character(data_train$GarageQual) > data_train$GarageQual[is.na(data_train$GarageQual)] <- "No"
data_train$GarageQual <- as.factor(data_train$GarageQual)
data_train$GarageYrBlt[is.na(data_train$GarageYrBlt)] <- 0
data_train$MasVnrArea[is.na(data_train$MasVnrArea)] <- mean(data_train$MasVnrArea, na.rm = TRUE) #substitute with mean
data_train$LotFrontage[is.na(data_train$LotFrontage)] <- mean(data_train$LotFrontage, na.rm = TRUE)
data_train$Electrical[is.na(data_train$Electrical)] <- "SBrkr"


# ++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++
# MODEL 1 - Fitting a Linear Regression

lm.fit <- lm(SalePrice~., data=data_train[train,])
summary(lm.fit)

# Compute "studentized residuals" to identify outliers
out <- abs(rstudent(lm.fit))
for(i in 1:length(out)){
    if(out[i] > 3){ print(out[i]) }
}

lm.fit <- lm(SalePrice ~ KitchenQual + X2ndFlrSF + X1stFlrSF + BsmtQual
+ MasVnrArea + OverallQual + OverallCond + LotArea + HouseStyle +
LotFrontage + YearBuilt +TotalBsmtSF, data = data_train[train,])

# Compute the Variance Inflaction Factor (VIF) to detect collinearity
library(car)
vif(lm.fit)


# ++++++++++++++++++++++++++++++++++++++++++++++++
# K-Fold Cross-Validation

mse.lm <- rep(0,10)
for(i in 1:10){
    set.seed(i)
    train <- sample(1:nrow(data_train), nrow(data_train)/2)
    test <- (-train)
    y <- data_train[test,]$SalePrice
    lm.fit <- lm(SalePrice ~ KitchenQual + X2ndFlrSF + X1stFlrSF +
BsmtQual + MasVnrArea + OverallQual + OverallCond + LotArea +
HouseStyle + LotFrontage + YearBuilt +TotalBsmtSF, data =
data_train[train,])
    pred <- predict(lm.fit, data_train[test,])
    mse.lm[i] <- mean((y - pred)^2)
}
print(sqrt(mean(mse.lm)))
# Average RMSE is 29,826.37




# ++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++
# MODEL 2 - Fitting the Lasso

install.packages("glmnet")
library(glmnet)

mse.lasso <- rep(0,10)
for (i in 1:10){
    x <- model.matrix(SalePrice~., data_train)[,-1] #Predictors
    y <- data_train$SalePrice #Response
    set.seed(i)
    train <- sample(1:nrow(x), nrow(x)/2) #split train and test
    test <- (-train)
    y.test = y[test]
    lasso.mod <- glmnet(x[train,], y[train], alpha=1) #fitting a lasso model
    cv.out <- cv.glmnet(x[train,], y[train], alpha=1) #find best lambda
    bestlam <- cv.out$lambda.min
    lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
    mse.lasso[i] <- mean((y.test - lasso.pred)^2)
}
print(sqrt(mean(mse.lasso)))
# Average MSE is 26,409.52




# R-squared
rsq <- function(predicted, actual){
    mean <- mean(actual)
    tss <- 0
    rss <- 0
    for(i in 1:length(predicted)){
        tss <- tss + (actual[i] - mean)^2
        rss <- rss + (actual[i] - predicted[i])^2
}
    rs <- 1 - rss/tss
}


# Compute R-squared among 10 different dataset splits
rs.lasso <- rep(0,10)
for (i in 1:10){
    x <- model.matrix(SalePrice~., data_train)[,-1] #Predictors
    y <- data_train$SalePrice #Response
    set.seed(i)
    train <- sample(1:nrow(x), nrow(x)/2) #split train and test
    test <- (-train)
    y.test = y[test]
    lasso.mod <- glmnet(x[train,], y[train], alpha=1) #fitting a lasso model
    cv.out <- cv.glmnet(x[train,], y[train], alpha=1) #find best lambda
    bestlam <- cv.out$lambda.min
    lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
    rs.lasso[i] <-rsq(lasso.pred, y.test)
}
print(sqrt(mean(rs.lasso)))
# R2 is 0.94



