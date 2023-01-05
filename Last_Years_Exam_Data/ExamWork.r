library(vcd)
bot_dat <- readRDS("/workspaces/ST201-Work/Last_Years_Exam_Data/bot_datB.rds")
bot_dat
bot_tab <- table(bot_dat)
bot_tab
addmargins(bot_tab)
# What proportion of humans selected “data” as a preference?
51 / 77
# What proportion of bots selected “data” as a preference?
38 / 73
# Create a mosaic plot and for these data.
mosaic(bot_tab, main = "Mosaic Plot of Bot Data", shade = TRUE, color = TRUE)
# Find the expected frequencies for these data.
chisq.test(bot_tab)$expected


# If I proceed to roll all 17 dice,
# ? what’s the probability that I’ll get exactly 3 skulls.
# Give your answer correct to two decimal places
dbinom(3, 17, 1 / 6)

# ? What is the probability of rolling fewer than 5 skulls?
# Give your answer correct to two decimal places.
pbinom(4, 17, 1 / 6)

# ? What is the 90th percentile value for the number of skulls rolled?
# Give your answer correct to the nearest whole number.
qbinom(0.9, 17, 1 / 6)


# If a worker starts to assemble a racing car at 4:07 pm,
# ? what is the probability that they will finish before 5:00 pm?
# Give your answer correct to two decimal places.
pnorm(53, 55, 2)

# ? What is the probability a person assembles a toy in 50 to 55 minutes?
# Give your answer correct to two decimal places.
pnorm(55, 55, 2) - pnorm(50, 55, 2)

# ? What is the 75th percentile value for the time it takes to assemble a car?
# Give your answer correct to the nearest minute.
qnorm(0.75, 55, 2)


med_dat <- readRDS("/workspaces/ST201-Work/Last_Years_Exam_Data/med_datC.rds")
med_dat

# Employees of a large coorporation are concerned about the
# declining quality of medical services provided by their group health insurance
# A random sample of consultation times for 24 visits by
# employees of this corporation to primary care physicians was obtained for 2017
# and a random sample of 27 was obtained for 2019.

# Find the upper bound of the 95% confidence interval for difference between the
# 2017 population mean wait time (𝜇1) and the 2019 population mean wait time(𝜇2)
# Give your answer correct to 2 decimal places.
t.test(med_dat$wait_2017, med_dat$wait_2019,
alternative = "two.sided")$conf.int[2]

# Using the t.test() function, carry out a 1-sided 95% hypothesis test
# such that the alternative hypothesis is 𝜇1>𝜇2,
# ? what is the p-value for this t-test? Give your answer to 2 decimal places
t.test(med_dat$wait_2017, med_dat$wait_2019,
alternative = "greater")$p.value

# ? There is evidence to support 𝜇1>𝜇2 at the 95% confidence level?
# ! TRUE

# ? Which of the following is not an assumption of simple linear regression?
# Normally distributed predictor variables
# Independence
# Linear relationship
# Normally distributed errors
# ! Normally distributed predictor variables

# ? In simple linear regression, the variable that is being predicted is the:
# The variable that's usually denoted x
# categorical variable
# independent variable
# response variable
# ! The response variable

# ? In regression analysis, the variable that is used to explain the change in y is called # nolint
# the explanatory variable
# the predictor variable
# the x variable
# all of the above are correct
# ! all of the above are correct

# * A statistician decides to examine the relationship between the price
# * of a magazine and the percentage of the magazine space that contains advertisements. # nolint
# You will find the dataset ad_datA.rds in the ST201_Exam folder on the Rstudio server. # nolint
# The dataset contains the variables percentage_ads and price (euros).
# Load this dataset into R and answer the questions below.

ad_dat <- readRDS("/workspaces/ST201-Work/Last_Years_Exam_Data/ad_datA.rds")
ad_dat

# ? Construct a scatter plot for these data with price as the response variable
# ? Do the data appear to exhibit a negative or positive linear relationship?
# Scatter plot:
plot(ad_dat$price, ad_dat$percentage_ads, xlab = "Price", ylab = "Percentage Ads") # nolint
# ! Negative linear relationship

# ? Fit a linear regression model with price as the response variable.
# ? What is the estimated regression coefficient for percentage_ads?
# Linear regression model:
ad_model <- lm(price ~ percentage_ads, data = ad_dat)
summary(ad_model)$coefficients[2, 1]
# ! -0.08

# ? TRUE or FALSE? The coefficient for percentage_ads is significant at the 5% alpha level # nolint
# ! TRUE

# ? Estimate the price of a magazine with 46 percent of its space containing ads. # nolint
# Give your answer correct to 2 decimal places.
predict(ad_model, data.frame(percentage_ads = 46))

# ? What is the lower bound of the prediction interval for the price estimate obtained in d. # nolint
# Give your answer correct to two decimal places.
predict(ad_model, data.frame(percentage_ads = 46), interval = "prediction") # nolint

# ? The prediction interval for the price estimate obtained in d is
# ? narrower than the confidence interval for the price estimate?
# ! FALSE

# ? What is the standard deviation of the residuals from this
# ? regression model? Give your answer correct to two decimal places.
summary(ad_model)$sigma

# ? The standard deviation of the residuals is less than the standard deviation of the observed data? # nolint
# ! TRUE

# ? What percentage of the variation in y has not been explained by x?
# Give your answer correct to two decimal places.
1 - summary(ad_model)$r.squared
