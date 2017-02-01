##### Load Data #####

credit.data <- read.csv("http://homepages.uc.edu/~maifg/7040/credit0.csv", header = T)

#remove id column
credit.data <- credit.data[,-1]

##### Split Training/Test Data #####

subset <- sample(nrow(credit.data), nrow(credit.data) * 0.9)
credit.train = credit.data[subset, ]
credit.test = credit.data[-subset, ]

##### Logistic Regression with glm function #####


glm.model <- glm(Y~., family = binomial, data = credit.train)

summary(glm.model)

step.glm.model <- step(glm.model)

#with BIC instead of AIC 
step.glm.model <- step(glm.model, k = log(nrow(credit.train)))

glm.model.alt <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)

# look at AIC and BIC for each model
AIC(glm.model)

AIC(glm.model.alt)

BIC(glm.model)

BIC(glm.model.alt)

# Histogram of response - margin too large error 
hist(predict(glm.model, type = "response"))

# Take probabilities and create binary outcome variable
prob.glm1.insample <- predict(glm.model.alt, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.2
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)

# Confusion Matrix

table(credit.train$Y, predicted.glm1.insample, dnn = c("Truth", "Predicted"))

# Calculate Error Rate (within training) - multiple ways to do this

mean(ifelse(credit.train$Y != predicted.glm1.insample, 1, 0))


# Apply prediction to test set

prob.glm1.outsample <- predict(glm.model.alt, credit.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(credit.test$Y, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))

# Error Rate
mean(ifelse(credit.test$Y != predicted.glm1.outsample, 1, 0))

# Doing the same prediction only with logodds to show it comes out the same
glm1.outsample.logodds <- predict(glm.model.alt, credit.test)
predicted.glm1.outsample <- exp(glm1.outsample.logodds)/(1 + exp(glm1.outsample.logodds)) > 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(credit.test$Y, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))
mean(ifelse(credit.test$Y != predicted.glm1.outsample, 1, 0))


###### ROC Curve #######

install.packages("verification")

library("verification")

# input the probabilities for each record calcuated by predict(.., type = "response")
# plot the ROC curve

roc.plot(credit.test$Y == "1", prob.glm1.outsample)

# get area under ROC Curve
roc.plot(credit.test$Y == "1", prob.glm1.outsample)$roc.vol

#plot multiple curves on one graph

prob.glm0.outsample <- predict(glm.model, credit.test, type = "response")
roc.plot(x = credit.test$Y == "1", pred = cbind(prob.glm0.outsample, prob.glm1.outsample), 
         legend = TRUE, leg.text = c("Full Model", "X_3, X_8, and X_11_2"))$roc.vol


# another library for ROC Curves

install.packages("ROCR")
library(ROCR)
pred <- prediction(prob.glm1.outsample, credit.test$Y)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)


##### Cross Validation and Cost Function #####

pcut = .02

#symmetric cost

cost1 <- function (r,pi){
  mean(((r==0) & (pi > pcut)|((r==1)& (pi < pcut))))
}

# Asymmetric cost
cost2 <- function(r, pi) {
  weight1 = 2
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

# 10-fold cross validation - using full dataset

install.packages("boot")
library(boot)

credit.glm3 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.data)
cv.result = ?cv.glm(credit.data, credit.glm3, cost1, 10)
cv.result$delta


# A Grid Search for best cuttoff probability with a defined cost function(some repeated code from above)

# define the searc grid from 0.01 to 0.99
searchgrid = seq(0.01, 0.99, 0.01)
# result is a 99x2 matrix, the 1st col stores the cut-off p, the 2nd column
# stores the cost
result = cbind(searchgrid, NA)
# in the cost function, both r and pi are vectors, r=truth, pi=predicted
# probability
cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
credit.glm1 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)
for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(credit.train$Y, predict(credit.glm1, type = "response"))
}

plot(result, ylab = "Cost in Training Set")


# Determine best probability cutoff using cross validation

searchgrid = seq(0.01, 0.6, 0.02)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
credit.glm1 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)
for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  result[i, 2] <- cv.glm(data = credit.train, glmfit = credit.glm1, cost = cost1, 
                         K = 3)$delta[2]
}
plot(result, ylab = "CV Cost")


