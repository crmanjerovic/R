# Packages in this project:
install.packages("dplyr")
install.packages("factoextra")
install.packages("ggfortify")
install.packages("car")
install.packages("pls")
install.packages("corrplot")
install.packages("glmnet")
install.packages("boot")
install.packages("caret")
library(dplyr)
library(factoextra)
library(ggfortify)
library(car)
library(pls)
library(corrplot)
library(glmnet)
library(boot)
library(caret)


#############
# Print NA values
print.na <- function(data) {
	# convert all non numeric columns to numeric
  	data.cor <- mutate_if(data, is.character, as.factor)
  	data.cor <- mutate_if(data.cor, is.factor, as.numeric)
	for(i in 1:ncol(data.cor)) {
		cat( "[", i, "] ", sum(is.na(data.cor[,i])), "\t", colnames(data.cor)[i], "\n")
	}
}

###############
# Give test rsq, mse for range of lambda vals on a ridge/lasso model

ridge.metrics <- function(ridge.fit, lambdas, y.this, newx.this) {
	rsq.lambda <- data.frame(matrix(nrow=0, ncol=4))
	colnames(rsq.lambda) <- c("lambda", "rsq", "rmse", "mae")

	for (i in lambdas) {
		ridge.pred <- predict(ridge.fit, s=i, newx.this)

		rss <- sum((ridge.pred - y.this)^2)
		tss <- sum((y.this - mean(y.this))^2)
		rsq <- 1 - (rss/tss)
		rmse <- sqrt(mean((y.this - ridge.pred)^2))
		mae <- sum(abs(ridge.pred-y.this))/length(y.this)
		
		rsq.lambda[nrow(rsq.lambda)+1,] <- c(i, rsq, rmse, mae)
	}
	return (rsq.lambda)
}


################
# CORRELATIONS #
# Find all correlations from the dataset with |correlation| > sig
# All combinations are left in (same pairs in different orders)

correlations <- function(data, sig) {

	# create version with all non numeric columns converted to numeric
	data.cor <- mutate_if(data, is.character, as.factor)
	data.cor <- mutate_if(data.cor, is.factor, as.numeric)

	# create data frame to hold correlations
	cor.table <- data.frame(matrix(ncol=5, nrow=0))
	colnames(cor.table) <- c("p1", "p2", "p1.name", "p2.name", "cor")

	# find correlation between every combo of predictors (columns)
	for(i in 1:ncol(data.cor)) {
		for(j in 1:ncol(data.cor)){
			correlation <- cor(data.cor[i], data.cor[j])
			if(i != j & !is.na(correlation) & abs(correlation)>sig ){
				p1.name <- colnames(data.cor)[i]
				p2.name <- colnames(data.cor)[j]
				cor.table[nrow(cor.table)+1,] <- c(i, j, p1.name, p2.name, correlation)
			}
		}
	}
	return(cor.table)
}


#########
# DUMMY #
# Automatically create and name dummy columns for all character columns
# Scale numeric data except for SalePrice
dummy.cols <- function(data, excluded, scale=TRUE) {

	# Ensure NA vals are "None"
	data[is.na(data)] <- "None"
	# Create empty data frame to hold output
	data.dummy <- data.frame(matrix(ncol=0, nrow=nrow(data)))

	cat("Removing baseline columns...\n\n")
	for (i in 1:ncol(data)) {
		if (!colnames(data)[i] %in% excluded) {
			
			# If not in excluded, split into dummy
      		this.model <- model.matrix(~data[, i] - 1)
			n <- colnames(this.model)
			colnames(this.model) <- make.names(paste0(colnames(data)[i], ".", substring(n, first = regexpr("]", n) + 1)))

			# Remove either "None" value column or column the most values to be baseline
			if (TRUE %in% grepl("None", colnames(this.model))) {
				none.col <- grepl("None", colnames(this.model))
				cat("[", i, "]\t NULL \t", colnames(subset(this.model, select = none.col)), "\n")
				this.model <- subset(this.model, select = !none.col)
			}
			else {
				max.col <- max(colSums(this.model))
				this.subset <- subset(this.model, select = !apply(this.model, 2, function(x) sum(x) == max.col))
				removed <- subset(this.model, select = apply(this.model, 2, function(x) sum(x) == max.col))
				cat("[", i, "]\t", max.col, "\t", colnames(removed),"\n")
				this.model <- this.subset
			}
			
			# Append columns
     			data.dummy <- cbind(data.dummy, this.model)
		}
		else {
			# If numeric, simply append the column (don't convert to dummy)
			if(scale==TRUE & colnames(data)[i]!="SalePrice") 
				{this.model <- data.frame(scale(as.numeric(data[,i])))}
			else {this.model <- data.frame(as.numeric(data[,i]))}
			colnames(this.model) <- colnames(data)[i]
			data.dummy <- cbind(data.dummy, this.model)
		}	
	}
	return (data.dummy)
}

########
