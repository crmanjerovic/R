# Models in this script:
# ridge.fit.0
# lasso.fit.1
# net.fit.2

# Format data for glmnet, which requires response vars in matrix formatS

x <- as.matrix(train.nummy.out[,-which(colnames(train.nummy.out) %in% "SalePrice")])
y <- train.nummy.out$SalePrice

##################
# RIDGE 
# variable shrinkage
# Pure ridge regression - Minimize cross validated test MSE
cv.ridge.0 <- cv.glmnet(x, y, alpha=0, type.measure="mse", 
	nfolds=10)
ridge.fit.0 <- cv.ridge.0$glmnet.fit
print(ridge.fit.0)
plot(ridge.fit.0, xvar="dev")
plot(ridge.fit.0, xvar="lambda")
plot(cv.ridge.0)

# Get cross validated lambdas
min.lam.0 <- cv.ridge.0$lambda.min 
lse.lam.0 <- cv.ridge.0$lambda.1se

# plot predicted vs actual
plot(predict(ridge.fit.0, s=min.lam.0, x), y)
# residuals
plot(y, (y-predict(ridge.fit.0, s=min.lam.0, x)), main="Ridge", ylab="residuals", xlab="actual")

# Iterate through lambda vals and find R^2, MSE, and MAE (only this test data)
ridge.metrics.0 <- ridge.metrics(ridge.fit.0, ridge.fit.0$lambda, y, x)
ridge.metrics.0
plot(ridge.metrics.0$lambda, ridge.metrics.0$rsq, xlab="Lambda", ylab="Test R^2")

# Get coefficient estimates for model with lambda that minimizes MSE
ridge.best.0 <- glmnet(x, y, alpha=0, type.measure="mse", 
	nfolds=10, lambda=min.lam.0)
coef(ridge.best.0)


##################
# LASSO 
# variable shrinkage and selection
# Pure LASSO regression
cv.lasso.1 <- cv.glmnet(x, y, alpha=1, type.measure="mse", 
	nfolds=10)
lasso.fit.1 <- cv.lasso.1$glmnet.fit
print(lasso.fit.1)
plot(lasso.fit.1, xvar="dev")
plot(lasso.fit.1, xvar="lambda")
plot(cv.lasso.1)

# Get cross validated lambdas
min.lam.1 <- cv.lasso.1$lambda.min 
lse.lam.1 <- cv.lasso.1$lambda.1se

# plot predicted vs actual
plot(predict(lasso.fit.1, s=min.lam.1, newx=x), y)
# residuals
plot(y, y-(predict(lasso.fit.1, s=min.lam.1, x)), main="LASSO", ylab="residuals", xlab="actual")

# Iterate through lambda vals and find test R^2, MSE, and MAE (only this test data)
lasso.metrics.1 <- ridge.metrics(lasso.fit.1, lasso.fit.1$lambda, y, x)
lasso.metrics.1
plot(lasso.metrics.ts.1$lambda, lasso.metrics.ts.1$rsq, xlab="Lambda", ylab="Test R^2")

# Get coefficient estimates for model with lambda that minimizes MSE
lasso.best.1 <- glmnet(x, y, alpha=1, type.measure="mse", 
	nfolds=10, lambda=min.lam.1)
coef(lasso.best.1)
sum(coef(lasso.best.1)[1:261] != 0)
attributes(coef(lasso.best.1))


##################
# EXPORT PREDICTIONS ON NEW DATA
#

test.nummy <- mutate_if(test.dummy, is.character, as.factor)
test.nummy <- mutate_if(test.nummy, is.factor, as.numeric)

x.test <- as.matrix(test.nummy)
test.predict <- predict(ridge.fit.0, s=min.lam.0, x.test)
write.csv(test.predict, "C:\\Users\\rando\\Desktop\\HousePrices.csv")
