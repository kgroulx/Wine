# Wine quality analysis from northern Portugal.
# For privacy reasons, there is no data about brand, grape type, price, etc.
# Data from http://archive.ics.uci.edu/ml/datasets/Wine+Quality
library('randomForest')
library('ggplot2')
library('rattle')
library('rpart')
library('rpart.plot')
library('RColorBrewer')

# read.csv will read in numericals as factors, and uncasing them proves difficult.
# to work around this, read the url directly via read.table
url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
red <- read.table(url, sep = ';', header = TRUE)
# quick look at data
head(red)
str(red)
# look at distribution of wine quality
barplot(table(red$quality))
# the dataset is centered more or less normally around wines.
#  let's tease these apart to learn a bit more, via a new variable - critic.
#  this can reflect critic score - accordingly, we'll be tougher than usual.
red$critic <- ifelse(red$quality < 5, 'bad','good')
red$critic[red$quality > 4 & red$quality < 7] <- 'normal'
red$critic <- as.factor(red$critic)
# random forest measure of both types of wine.
# first, divide up the red wine dataset into a training and testing case.
set.seed(123)
redsub <- sample(nrow(red), 0.6 * nrow(red))
redtrain <- red[redsub,]
redtest <- red[-redsub,]
# now, fit our model to infer values for other parameters to determine critic score.
#  leave out quality, since it has a high autocorrelation with critic.
fit <- randomForest(critic ~ . - quality,importance=TRUE,data=redtrain)
varImpPlot(fit)
# of note here: alcohol seems to dominate the quality of red wine
#  sulphates also seem to be important.

# how accurate is all of this?
# let's test this on our test dataset
fit
red_predict <- predict(fit, newdata = redtest)
table(red_predict,redtest$critic)
# reds were more likely to classify as normal than as one of the outliers, 
#  but still our model is 87.5% accurate
acc <- (50+510)/nrow(redtest)
# for reds: alcohol v quality, sulphates v quality
qplot(quality, alcohol, data = red) + geom_smooth(method = "lm")
qplot(quality, sulphates, data = red) + geom_smooth(method = "lm")
# to look at this in a bit more depth, let's get a better look at one decision tree
treeFit <- rpart(critic ~ . - quality, data=red)
rpart.plot(treeFit)


# let's do the same analysis, but with white wine
url2 <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
white <- read.table(url2, sep = ';', header = TRUE)
barplot(table(white$quality))
# we can be a bit more critical with white wine
white$critic <- ifelse(white$quality < 5, 'bad','good')
white$critic[white$quality == 6] <- 'normal'
white$critic <- as.factor(white$critic)

set.seed(123)
whitesub <- sample(nrow(white), 0.6 * nrow(white))
whitetrain <- white[whitesub,]
whitetest <- white[-whitesub,]
fit2 <- randomForest(critic ~ . - quality,importance=TRUE,data=whitetrain)
varImpPlot(fit2)
# alcohol still seems to dominate quality of white wine, but sulphates don't.
# instead, free SO2 seems to assume importance.

white_predict <- predict(fit2, newdata = whitetest)
table(white_predict,whitetest$critic)
acc2 <- (12+745+575)/nrow(whitetest)
# whites had a 68% accuracy, due to the classes being far more balanced than reds

## LOOK AT THIS
fitnew <- randomForest(as.factor(quality) ~ . - critic, importance = T, data = whitetrain)
varImpPlot(fitnew)
nuWhite <- predict(fitnew, newdata = whitetest)
table(nuWhite,whitetest$quality)
ax <- (9+400+708+170+25)/nrow(whitetest)

# for whites: alcohol v quality, free SO2 v quality
qplot(quality, alcohol, data = white) + geom_smooth(method = "lm")
qplot(quality, free.sulfur.dioxide, data = white) + geom_smooth(method = "lm")
# oddly, free SO2 doesn't seem to be an important factor
# both low and high rated white wines have the same rough amount of free SO2
treeFit2 <- rpart(critic ~ . - quality, data=white)
rpart.plot(treeFit2)
