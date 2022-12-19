student_sleep <- c(5,6,9,7,4,12,7,8,8,6,5,4,7,5,7,6,7,7,4,10)

xbar <- mean(student_sleep)
s <- sd(student_sleep)
N <- length(student_sleep)

# H0 : mu = 8
# Ha: mu != 8

# carry out the single sample t-test
sleep_test <- t.test(student_sleep,
                     alternative = "two.sided",
                     mu = 8)

# get the test statistic from the output
sleep_test$statistic

# compare to calculation (xbar - mu0)/se

(xbar - 8)/(s/sqrt(N))

# look at the t-distribution under the assumption that Ho is true
x <- seq(-4,4,by = 0.1)
t_dens <- dt(x,df = N-1)

qt(0.975,N-1)

# see where critial region is for 95% test
# see where the test statistic falls
plot(x, t_dens, type = "l")
abline(v = qt(0.025,N-1))
abline(v = qt(0.975,N-1))
abline(v = sleep_test$statistic, col = "red")

# get the p value for the t-test output
sleep_test$p.value

# another way to get the p value using pt()
pt(sleep_test$statistic, N-1)

# the the 95% confidence interval from the t-test
sleep_test$conf.int

# the the 95% CI another way
xbar - qt(0.975, N-1)*(s/sqrt(N))
xbar + qt(0.975, N-1)*(s/sqrt(N))

## If doing a 1-sided test
# H0 : mu </= 8
# Ha: mu > 8

# carry out the single sample t-test, use "alternative" argument
sleep_test2 <- t.test(student_sleep,
                      alternative = "greater",
                      mu = 8)

# compare the t statistic to the crital value on the plot
plot(x, t_dens, type = "l")
abline(v = qt(0.975,N-1))
abline(v = sleep_test$statistic, col = "red")


# get the p-value from t test
sleep_test2$p.value

# the same as..
1-pt(-2.8297, N-1)

# two sample test ---------------------------------------------------------

blood_pressure <- read_csv("~/SharedFiles/ST201/blood_pressure.csv")

## mu_x is the population mean blood pressure for the placebo group
## mu_y is the population mean blood pressure for the treatment group

## Ho: mu_x </= mu_y
## Ha: mx > mu_y

# 2-sample t test 
bp_test <- t.test(blood_pressure$placebo,blood_pressure$treatment,
                  alternative = "greater")

# calc the t-stat (xbar - ybar)/se (# se <- sqrt((sx^2)/n + (sy^2)/m))
sx <- sd(blood_pressure$placebo)
sy <- sd(blood_pressure$treatment)
n <- m <- nrow(blood_pressure)

## degress of freedom
v <- (((sx^2/n) + (sy^2/m))^2)/( (((sx^2/n)^2)/(n-1)) + (((sy^2/m)^2)/(m-1)))
v
bp_test$parameter

# plot visualise rejection region
x <- seq(-4,4,by = 0.1)
t_dens <- dt(x, bp_test$parameter)
plot(x, t_dens, type = "l")
abline(v = qt(0.95, bp_test$parameter))
abline(v = 1.5845, col = "red")

# p-value using pt()
pt(1.5845,bp_test$parameter, lower.tail = FALSE)
?pt

# specify two.sided to get the 95% CI

#Ho: mux = muy
#Ha: mux != muy

# otherwise just calc CI use xbar - ybar and df = nu
