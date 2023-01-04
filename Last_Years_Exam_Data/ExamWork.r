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
# what is the p-value for this t-test? Give your answer to 2 decimal places