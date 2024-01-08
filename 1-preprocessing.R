set.seed(19)
train <- read.csv("C:/Users/rando/Documents/~ Spring 2023/Predictive Modeling Algorithms/Final/house-prices-advanced-regression-techniques/train.csv")
test <- read.csv("C:/Users/rando/Documents/~ Spring 2023/Predictive Modeling Algorithms/Final/house-prices-advanced-regression-techniques/test.csv")

#################
# PREPROCESSING #

print.na(train)
# convert all NA values to "None"
train[is.na(train)] <- "None"
test[is.na(test)] <- "None"

#
sale.bind <- data.frame(SalePrice=rep(0, 1459))
test.bind <- cbind(test, sale.bind)
total.bind <- rbind(train, test.bind)


# create version with all non numeric columns converted to numeric
train.num <- mutate_if(train, is.character, as.factor)
train.num <- mutate_if(train.num, is.factor, as.numeric)

test.num <- mutate_if(test, is.character, as.factor)
test.num <- mutate_if(test.num, is.factor, as.numeric)

#########
# DUMMY #
# Convert categorical variables to dummy variables 
# Also scales numeric variables 

# Names of numeric columns to exclude in conversion to dummy variable:
excluded.cols <- c(colnames(train[,sapply(train, is.numeric)]), 
	"LotFrontage", "MasVnrArea", "GarageYrBlt")

# convert to dummy
total.dummy <- dummy.cols(total.bind, excluded.cols, scale=TRUE)
# Split back into training and testing sets
train.dummy <- total.dummy[1:1460,]
test.dummy <- total.dummy[1461:2919,-269]

# Delete ID column
train.dummy <- train.dummy[,-1]
test.dummy <- test.dummy[,-1]

print.na(train.dummy)
print.na(test.dummy)

# Replace newly created NAs with 0s (original data had NAs)
train.dummy[is.na(train.dummy$GarageYrBlt),]$GarageYrBlt <- "None"
train.dummy[is.na(train.dummy)] <- 0

test.dummy[is.na(test.dummy$GarageYrBlt),]$GarageYrBlt <- "None"
test.dummy[is.na(test.dummy)] <- 0

colnames(train.dummy)
colnames(test.dummy)
ncol(train)
ncol(train.dummy)

# Create numeric verions of dummy data
train.nummy <- mutate_if(train.dummy, is.character, as.factor)
train.nummy <- mutate_if(train.nummy, is.factor, as.numeric)



################
# CORRELATIONS #

# count non numeric rows
print("Non-numeric rows in data:")
sum(sapply(train, function(x) !is.numeric(x)))

# create table of correlations for train
train.cor <- correlations(train, .5)
train.cor[train.cor$p1 == 81,]

# Get correlations for dummy
train.dummy.cor <- correlations(train.dummy, 0.5)
train.dummy.cor.75 <- correlations(train.dummy, 0.75)

# correlated with SalePrice
top.predictors <- train.dummy.cor[which(train.dummy.cor$p1.name %in% "SalePrice"),]$p2.name
print(top.predictors)

# collinearity between top predictors
for (i in 1:nrow(train.dummy.cor)) {
	if (train.dummy.cor[i,]$p1.name %in% top.predictors & train.dummy.cor[i,]$p2.name %in% top.predictors) {
		cat(train.dummy.cor[i,]$p1.name, "\t", train.dummy.cor[i,]$p2.name, "\t", train.dummy.cor[i,]$cor,"\n")
	}
}

