# Models in this script:
# lm.a	linear model with all predictors. 
# lm.0	linear model with predictors having |r| > .5 with SalePrice. split data testing.
# lm.cv.0	like lm.0 but with CV instead of split data

# Split "train" data indices for train and test subsets
set.seed(19)
index <- sample.int(nrow(train), size=nrow(train)*0.7)
outdex <- setdiff(c(1:nrow(train)), index)

##################
# LRM - all predictors
# using split sample

lm.a <- lm(SalePrice~., data=train.dummy[index,]) 
summary(lm.a)
plot(predict(lm.a), residuals(lm.a))
plot(cooks.distance(lm.a), abs(residuals(lm.a)))

##################
# lm.0 - all predictors having |r| > .5 with SalePrice
# using split sample
lm.0 <- lm(SalePrice ~ OverallQual + YearBuilt + 
	YearRemodAdd +  BsmtQual.Ex + 
	TotalBsmtSF + X1stFlrSF + 
	GrLivArea + FullBath + 
	KitchenQual.Ex + TotRmsAbvGrd + 
	GarageCars + GarageArea, 
	data = train.dummy[index,])

summary(lm.0)

# Plots
plot(predict(lm.0, newdata=train.dummy[index,]), 
	train.dummy[index,]$SalePrice, xlab="Predicted Price", 
	ylab="Actual Price", main="lm.0 Training")
plot(predict(lm.0, newdata=train.dummy[-index,]), 
	train.dummy[-index,]$SalePrice, xlab="Predicted Price", 
	ylab="Actual Price", main="lm.0 Testing")
plot(predict(lm.0), residuals(lm.0), xlab="Predicted", ylab="Residuals", main="lm.0")
plot(cooks.distance(lm.0), residuals(lm.0), xlab="Cook's distance", ylab="Residuals")

# Multicollinearity in the model
# Note: standardization reduces these values
vif.lm.0 <- vif(lm.0)

# R^2, MSE, AIC, and BIC
lm.pred.0 <- predict(lm.0, newdata=train.dummy[-index,])
rss.lm.0 <- sum((lm.pred.0 - train[-index,]$SalePrice)^2)
tss.lm.0 <- sum((train.dummy[-index,]$SalePrice - mean(train.dummy[-index,]$SalePrice))^2)
rsq.lm.0 <- 1 - (rss.lm.0/tss.lm.0)
rmse.lm.0 <- sqrt(mean((lm.pred.0-train.dummy[-index,]$SalePrice)^2))
aic.lm.0 <- AIC(lm.0)
bic.lm.0 <- BIC(lm.0)
cat("MODEL:\t lm.0\nR^2:\t", rsq.lm.0, "\nRMSE:\t", rmse.lm.0, "\nAIC:\t", aic.lm.0, "\nBIC:\t", bic.lm.0, "\n")

# training rmse
rmse.tr.lm.0 <- sqrt(mean(lm.0$residuals^2))

##################
# lm.cv.0 - all predictors having |r| > .5 with SalePrice
# using CV
lm.cv.0 <- glm(SalePrice ~ OverallQual + YearBuilt + 
	YearRemodAdd +  BsmtQual.Ex + 
	TotalBsmtSF + X1stFlrSF + 
	GrLivArea + FullBath + 
	KitchenQual.Ex + TotRmsAbvGrd + 
	GarageCars + GarageArea, 
	data = train.dummy, family=gaussian)

summary(lm.cv.0)

# Plots
plot(predict(lm.cv.0, newdata=train.dummy), 
	train.dummy$SalePrice, xlab="Predicted Price", 
	ylab="Actual Price", main="lm.cv.0")
plot(train.dummy$SalePrice, residuals(lm.cv.0),
	xlab="Actual", ylab="Residuals", main="lm.cv.0") 
plot(cooks.distance(lm.cv.0), residuals(lm.cv.0),
	xlab="Cook's distance", ylab="Residuals", main="lm.cv.0")

resid.lm.cv.0 <- as.numeric(names(which(abs(rstandard(lm.0)) > 2)))
resid.lm.cv.0
influ.lm.cv.0 <- as.numeric(names(which(cooks.distance(lm.cv.0) > .005)))
influ.lm.cv.0

# Multicollinearity in the model. same as last one
vif.lm.cv.0 <- vif(lm.cv.0)

# R^2, MSE, AIC, and BIC
cv.0 <- cv.glm(train.dummy, lm.cv.0, K=10)
lm.cv.pred.0 <- predict(lm.cv.0)
rsq.lm.cv.0 <- 1 - (cv.0$delta[1]/var(train.dummy$SalePrice))
rmse.lm.cv.0 <- sqrt(mean((lm.cv.pred.0 - train.dummy[-index,]$SalePrice)^2))
aic.lm.cv.0 <- AIC(lm.cv.0)
bic.lm.cv.0 <- BIC(lm.cv.0)
cat("MODEL:\t lm.cv.0, K=10\nR^2:\t", rsq.lm.cv.0, "\nRMSE:\t", rmse.lm.cv.0, "\nAIC:\t", aic.lm.cv.0, "\nBIC:\t", bic.lm.cv.0, "\n")

# Create version of data without points with large residuals
train.dummy.out <- train.dummy[-(resid.lm.cv.0),]
train.nummy.out <- train.nummy[-(resid.lm.cv.0),]
train.num.out <- train.num[-(resid.lm.cv.0),]