# this file contains the worked examples and solutions to the problems from Chapter 4 of
# Bruce, P., Bruce, A. 2017. "Practical Statistics for Data Scientists", O'Reilly.

# ---------------
# pages [162; 171)
# simple linear regression

# simple linear regression minimises the Residual Sum of Squares (RSS) between the predicted and recorded values

setwd("D:/r_projects/practical_statistics_for_data_scientists")
lung_disease <- read.csv("data/LungDisease.csv", stringsAsFactors = FALSE)

perf_vs_exposure <- lm(PEFR ~ Exposure, data = lung_disease)
perf_vs_exposure
fitted <- predict(perf_vs_exposure)
residuals <- residuals(perf_vs_exposure)

plot(lung_disease$Exposure, lung_disease$PEFR, ylim = c(0,600))
abline(a = perf_vs_exposure$coefficients[1], b = perf_vs_exposure$coefficients[2], col = "blue")
segments(x0 = lung_disease$Exposure, y0 = fitted, x1 = lung_disease$Exposure, y1 = fitted + residuals, col = "red", lty = "dashed")

# ---------------
# pages [171; 177)
# multiple linear regression

# multiple linear regression minimises the Root Mean Squared Error (RMSE) between the predicted and recorded values
# RMSE = RSS / sqrt(n) = sqrt( (Yobs - Ypr)^2 / n), where n is the number of observations
# another metric to assess the quality of a multiple linear regression model is the Residual Standard Error (RSE)
# RSE = RSS / sqrt(df), where df is the number of the model's degrees of freedom, df = n - p - 1, where p is the number of predictors
# obviously, as the number of data points increases, the difference between RMSE and RSS becomes minuscule
# another important metric is R-squared, or the coefficient of determination, i.e. that measures the proportion of data variance explained by the regression model
# R-squared = 1 - sum(Yobs - Ypr)^2 /  sum(Yobs - mean(Yobs))^2

house_sales <- read.csv("data/house_sales.csv", sep = "\t", stringsAsFactors = FALSE)
head(house_sales)

house_lm <- lm(formula = AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade,
               data = house_sales,
               na.action = na.omit)
summary(house_sales$AdjSalePrice)
summary(house_lm)

# the important difference of data science from statistical practice is the way models are tested and validated
# statistical models are normally developed with a (relatively) small dataset available, so they use all records to estimated the model parameters and discard futher validation
# prediction models in data science, on the contrary, are normally developed with a bigger dataset in hand, so the latter is split into the training and testing subsets of data
# thus statistical model validation is called  i n - s a m p l e  model validation

# ---------------
# pages [177; 184)
# step-wise regression, weighed regression

# recall Occam's Razor and avoid model with too many predictors
# there are multiple obvious ways of (perhaps selectively selectively) iterating over the set of possible combinations of predictors and a few established metrics to compare different models
#
# Cf. penalised regression models:
# (i)  lasso regression
# (ii) ridge regression
#
# Akaike's Information Criteria (AIC)
# AIC = 2*P + n * log(RSS / n), where p is the number of predictors, n is the number of records
# AIC penalises the model for using extra predictors without significantly improving RSS
#
# step-wise regression can be guided by AIC

library(MASS)
house_big_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                     Bedrooms + BldgGrade + PropertyType + NbrLivingUnits +
                     SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                  data = house_sales,
                   na.action=na.omit)
step <- stepAIC(house_big_lm, direction="both")
summary(step)
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + Bathrooms + Bedrooms +
                 BldgGrade + PropertyType + SqFtFinBasement + YrBuilt,
               data = house_sales)

# weighed regression takes each of the records with some weight to adjust for
# (i)   measurement (in)accuracy
# (ii)  data reliability
# (iii) obsolete yet relevant data

library(lubridate)
house_sales$Year <- year(house_sales$DocumentDate)
house_sales$Weight <- house_sales$Year - min(house_sales$Year)

house_lm_weight <- lm(AdjSalePrice ~ SqFtTotLiving + Bathrooms + Bedrooms +
                 BldgGrade + PropertyType + SqFtFinBasement + YrBuilt,
               data = house_sales,
               weight = Weight)

round(
  cbind(
    lm = house_lm$coefficients,
    weighed_lm = house_lm_weight$coefficients,
    diff = house_lm$coefficients - house_lm_weight$coefficients)
  , 3)

# ---------------
# pages [184;190)
# regression with factor variables

# since regression (as indeed any mathematical function) maps numeric arguments to numeric function values, while observational data contains categorical (factor) variables, it is necessary to encode the latter as numbers in order to include them in a regression model
# the three common ways of turning factors into so-called dummy variables are
# (i)   one-hot encoding (each level of a given factor is encoded with a separate 0/1 variable)
# (ii)  reference encoding (some level of a given factor is chosen as reference to which other levels are compared)
# (iii) deviation encoding (a given factor's mean value is chosen as reference to which other levels are compared)
# (iv)  polynomial encoding (used for ordered factors)

property_type_dummies <- model.matrix(~PropertyType -1, data = house_sales)
head(property_type_dummies)

# when a factor variable contains many levels, it is worthwhile grouping them into a few ones
# this can be done either by analysing their meaning or by analysing how they bid in the regression model

library(dplyr)
zip_groups <- house_sales %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = ntile(cum_cnt, 5))

house_sales <- house_sales %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')
house_sales$ZipGroup <- as.factor(house_sales$ZipGroup)

# ---------------
# pages [190;210)
# interpreting and testing regression

# five basic problems with linear models are
# (i)   correlated predictor variables - one shall exclude the correlated predictors
# (ii)  multicollinearity, i.e. the extreme case of (i) when one predictor variable can be expressed as a linear combination of others and is thus redundant; a typical case is a factor variable with N levels that is expanded to N instead on N-1 dummies - one shall exclude the redundant predictors
# (iii) confounding variables, i.e. excluding the relevant variables from the model - one shall include the missing variables
# (iv)  interaction between the variables, i.e. the coefficient of some predictor shall depend on the value of some other predictor - one shall include the interaction in the model
# (v)   heteroskedasticity, i.e. the variance of residuals is either not normal or not the same over the range of model's prediction; these two are the prerequisites of a valid linear regression model, hence, violating any of them signals that the model fails to account for some difference between the data points
# (vi)  partial residuals are non-linear, i.e. the contribution of a given predictor to the model is poorly approximated with a linear function, where the partial residual of a predictor is the sum of the total residual and the product of the predictor's coefficient (the regression term) and the predictor's value

summary(house_lm)

# call update(...) to exclude predictors
update(house_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)

# add interaction between predictors by specifying it with '*' in the model's formula
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving * ZipGroup + SqFtLot +
                Bathrooms + Bedrooms + BldgGrade + PropertyType,
              data = house_sales, na.action = na.omit)

# just as the IQR or another threshold value is used to detect outliers in data, the standardised residuals, i.e. the ratio of a residual to the standard error of residuals, are used to detect outliers among modelled predictions

house_sales_98105 <- house_sales[house_sales$ZipCode == 98105, ]
house_lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade, data = house_sales_98105)

summary(house_lm_98105)
standardised_residuals <- rstandard(house_lm_98105)
idx <- order(standardised_residuals)
standardised_residuals[idx[1:10]]

# Cook's distance and the hat value are metrics used to detect the records with high leverage over a regression equation, i.e. the records that influence the equation much stronger than other records do
cooks_distances <- cooks.distance(house_lm_98105)
hat_values <- hatvalues(house_lm_98105)

# threshold values for Cook's distance and hat value are calculated as follows
# p - the number of predictors
# n - the number of observations
n <- length(house_sales_98105)
p <- length(house_lm_98105$coefficients) - 1
cdt <- 4 / (n - p - 1)
hvt <- 2 * (p + 1) / n
  
plot(hat_values, standardised_residuals, cex=10*sqrt(cooks_distances))
abline(h=c(-2.5, 2.5), lty=2)

house_sales_98105_cleared <-
  house_sales_98105[-2.5 < standardised_residuals &
                      standardised_residuals < 2.5 &
                      cooks_distances < cdt &
                      hat_values < hvt,
                    ]

house_lm_98105_cleared <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                               Bedrooms + BldgGrade, data = house_sales_98105_cleared)

# although in statistical practice the influence of outliers is significant, it is normally of much smaller import in data science where datasets are huge
round(cbind(all = house_lm_98105$coefficients,
            cleared = house_lm_98105_cleared$coefficients), 3)

# to test for heteroskedasticity of residuals
# (i)  plot the absolute value of residuals vs the predicted values and visually check if the variance is the same over the range of predicted values
# (ii) plot the distribution of residuals and visually check if the latter is noramal
library(ggplot2)
df <- data.frame(
  resid = residuals(house_lm_98105_cleared),
  pred = predict(house_lm_98105_cleared))

ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth()

hist(rstandard(house_lm_98105_cleared))

# to test if partial residuals are linear
# plot the partial residuals of a chosen predictor vs this predictor's values
terms <- predict(house_lm_98105_cleared, type="terms")
partial_resid <- resid(house_lm_98105_cleared) + terms

df <- data.frame(SqFtTotLiving = house_sales_98105_cleared[, "SqFtTotLiving"],
                 Terms = terms[, "SqFtTotLiving"],
                 PartialResid = partial_resid[, "SqFtTotLiving"])

ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape = 1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype = 2) +
  geom_line(aes(SqFtTotLiving, Terms))

# ---------------
# pages [210;220)
# polynomial and spline regression

house_polylm_98105 <- lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot +
     BldgGrade + Bathrooms + Bedrooms,
   data=house_sales_98105)

# spline is a series of piece-wise continuous polynomial
library(splines)
# it is necessary to specify the knots, i.e. the points at which the polynomial functions change
# a straightforward, albeit arbitrary, choice is the quantiles
knots <- quantile(house_sales_98105$SqFtTotLiving, p = c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots = knots, degree = 3) +
                  SqFtLot + Bathrooms + Bedrooms + BldgGrade, data = house_sales_98105)

# GAM (generalised additive models) are used in order to determine what polynomial functions to use and to compute where to place the knots
library(mgcv)
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot +
                Bathrooms + Bedrooms + BldgGrade,
              data=house_sales_98105)

summary(lm_gam)