## Read in the advertising dataset
library(readr)
Advertising <- read_csv("Advertising.csv")

## scatter plot of the data
plot(Advertising$TV, Advertising$sales)

## correlation
cor(Advertising$TV, Advertising$sales)

## fit the linear regression model
?lm

sales_mod <- lm(sales ~ TV, data = Advertising)
summary(sales_mod)

## add the regression line (y_hat) to the scatter plot
## use predict for lm_mod

yhat <- predict(sales_mod)
y <- Advertising$sales
n <- nrow(Advertising)
plot(Advertising$TV, Advertising$sales)
lines(Advertising$TV, yhat, col = "red", lwd = 3)

## example of getting a prediction from the model for TV = 100
## y_hat = beta0_hat + beta1_hat*TV

7.032594 + 0.047537 * 100

## use predict
new_dat <- data.frame(TV = c(100, 150))
predict(sales_mod, newdata = new_dat)

## Confidence interval for the slope parameter (TV coefficient)
## beta1 +/- 1.96*SE(beta1)
summary(sales_mod)

0.047537 + 1.96 * 0.002691

0.047537 - 1.96 * 0.002691

## Check the significance of the parameters


## H0: B1 = 0
## Ha: B1 != 0

## Test statistic beta1/se(beta1)

## Critical value for rejection based on t-distribution
qt(0.975, df = 198)

## Get predictions for the observed sales based on the model (y_hat)

## Observed values y


## Residual = observed - predicted
res <- y - yhat
sqrt(sum(res^2) / (n - 2))

## Get Residual Sum of Squares 

## no. of observations


## Residual standard error sqrt(RSS/n-2)


## Get the Total sum of squares based on sample mean TSS = sum((y - ybar)^2)


## Get r-squared (TSS - RSS)/TSS


## Compare to correlation^2


# predict for new data point 

# create data.frame with point to predict

newdata <- data.frame(TV = c(100, 150))

# predict

### Further Analysis

# observed vs predicted

# residuals vs fit
plot(y, yhat, xlab = "Observed", ylab = "Predicted")
lines(y, y, col = "red", lwd = 3)

# hist of the residuals
hist(res)

# qqnorm for residuals
qqnorm(res)
qqline(res)

# shapiro test
shapiro.test(res)
#Ho: residuals are normally distributed