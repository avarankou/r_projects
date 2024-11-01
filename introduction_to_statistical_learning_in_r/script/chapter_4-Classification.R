# this file contains worked examples and solutions to the problems from Chapter 4 "Classification" of
# James, G. et al. 2023. "An Introduction to Statistical Learning in R", Springer.

library(ISLR2)
library(corrplot)
library(MASS)
library(e1071)

stock_data <- ISLR2::Smarket
# this is a pretty meager dataset, which contains, basically, the volume of shares traded on a single trading day and percentage return for that day
# the results of previous five trading days are appended to each record, so that it is easier to tinker a simple model
# one might expect, that the results of previous days influence that of today: this could be captured by an ARMA model
# likewise, one expects that the results of consecutive days are positively correlated, with the exception of extremely high (low) prices which are typically followed by a decrease (increase)
# it is worthwhile checking if the price and the volume are correlated

# first, check if the results of consecutive days are correlated
# apart from Volume vs Year, there is no correlation at all
cor(stock_data[1:8])
corrplot(cor(stock_data[1:8]), method = "circle", type = "upper")


# Logistic Regression -----------------------------------------------------
# then, consider a naive way to predict if the price goes up or down based on the volume of trade and the prices of five previous days
glm(data = stock_data,
    family = "binomial",
    formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume
    ) -> glm_stock

# observe that all p-values are way above 5% threshold
summary(glm_stock)

# check the way the response variable is encoded: 0 for Down, 1 for Up
contrasts(glm_stock$data$Direction)

# compute a confusion matrix to see how good the training predictions are
ifelse(glm_stock$fitted.values > 0.5, 1, 0) -> training_prediction
table(glm_stock$y, training_prediction, dnn = c("Y / Data", "Y-hat / Prediction")) -> conf_mat
# with the accuracy little about 50%, the prediction is no good
(conf_mat[1, 1] + conf_mat[2, 2]) / sum(conf_mat) -> accuracy

# let us repeat the process of creating a logistic regression model with the data split into train and test datasets
split_by_year <- 2005
stock_data$Year <  split_by_year -> train_idx
rep_len(FALSE, NROW(stock_data)) -> test_idx
test_idx[!train_idx] <- TRUE
cat("traininig records: ", sum(train_idx), "\ntest records: ", sum(test_idx), "\n")

glm(data = stock_data,
    subset = train_idx,
    family = "binomial",
    formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume) -> glm_stock_train

# note that the coefficients computed based on a subset of the entire datum are quite different, viz. Intercept and Volume have different sign!
summary(glm_stock_train)
rbind(glm_stock$coefficients, glm_stock_train$coefficients)

# now,
# (i)   make predictions for Year 2005, i.e. the test dataset,
# (ii)  compute the confusion matrix for the test dataset,
# (iii) compute the accuracy of test predictions,
# (iv)  compare (iii) with the accuracy obtained previously
ifelse(predict(glm_stock_train, stock_data[test_idx, ], type = "response") > 0.5, 1, 0) -> glm_test_prediction
stock_data[test_idx, "Direction"] -> test_y
table(test_y, glm_test_prediction, dnn = c("Y / Data", "Y-hat / Prediction")) -> glm_conf_mat_test
# with the accuracy little below 50%, the prediction is no good again
(glm_conf_mat_test[1, 1] + glm_conf_mat_test[2, 2]) / sum(glm_conf_mat_test) -> accuracy_test


# Linear Discriminant Analysis --------------------------------------------
# now let us try if an LDA classifier bids better
MASS::lda(formula = Direction ~ Lag1 + Lag2,
    data = stock_data,
    subset = train_idx) -> lda_stock_train

lda_stock_train

# compute predictions for the test dataset
predict(lda_stock_train, stock_data[test_idx, ]) -> lda_test_prediction
table(test_y, lda_test_prediction$class, dnn = c("Y / Data", "Y-hat / Prediction")) -> lda_conf_mat_test
# with the accuracy about 56%, the prediction is only a tad better that with the GLM above
(lda_conf_mat_test[1, 1] + lda_conf_mat_test[2, 2]) / sum(lda_conf_mat_test) -> accuracy_test


# Quadratic Discriminant Analysis -----------------------------------------
# finally, consider a non-linear method, viz. QDA
MASS::qda(formula = Direction ~ Lag1 + Lag2,
    data = stock_data,
    subset = train_idx) -> qda_stock_train

# compute predictions for the test dataset
predict(qda_stock_train, stock_data[test_idx, ]) -> qda_test_prediction
table(test_y, qda_test_prediction$class, dnn = c("Y / Data", "Y-hat / Prediction")) -> qda_conf_mat_test

TP <- qda_conf_mat_test[1, 1]
FP <- qda_conf_mat_test[1, 2]
TN <- qda_conf_mat_test[2, 2]
FN <- qda_conf_mat_test[2, 1]

# with the accuracy about 60%, the prediction is the best so far
TP / (TP + FN) -> TPR
FP / (TN + FP) -> FPR
(TP + TN) / (TP + TN + FP + FN) -> accuracy
TP / (TP + FN) -> precision


# Naive Bayes -------------------------------------------------------------
e1071::naiveBayes(formula = Direction ~ Lag1 + Lag2,
                  data = stock_data,
                  subset = train_idx) -> nb_stock_train

# compute predictions for the test dataset
predict(nb_stock_train, stock_data[test_idx, ]) -> nb_test_prediction
table(test_y, nb_test_prediction, dnn = c("Y / Data", "Y-hat / Prediction")) -> nb_conf_mat_test

TP <- nb_conf_mat_test[1, 1]
FP <- nb_conf_mat_test[1, 2]
TN <- nb_conf_mat_test[2, 2]
FN <- nb_conf_mat_test[2, 1]

# with the accuracy about 59%, the prediction matches that of the QDA above
TP / (TP + FN) -> TPR
FP / (TN + FP) -> FPR
(TP + TN) / (TP + TN + FP + FN) -> accuracy
TP / (TP + FN) -> precision


# K Nearest Neighbours ----------------------------------------------------
# TODO


# Poisson Regression ------------------------------------------------------
# TODO