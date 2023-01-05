library(vcd)
returns_dat <- readRDS("Final_Exam/returns_datA.rds")
returns_dat
returns_tab <- table(returns_dat)
returns_tab
addmargins(returns_tab)
# What is the odds ratio for damaged returns to store A compared to store B?
wa <- 2 / 4
wa
wb <- 2 / 9
wb
wa / wb
chisq.test(returns_tab)$expected
fisher.test(returns_tab)

1 - pnorm(800, mean = 950, sd = 150)
pnorm(1000, mean = 950, sd = 150) - pnorm(900, mean = 950, sd = 150)
qnorm(0.6, mean = 950, sd = 150)

dpois(5, lambda = 5)
1 - ppois(47, lambda = 5 * 12)
qpois(0.9, lambda = 5)

exercise_dat <- readRDS("Final_Exam/exercise_datA.rds")
exercise_dat
mean(exercise_dat$under_50)
mean(exercise_dat$over_50)
sd(exercise_dat$under_50)
sd(exercise_dat$over_50)
t.test(exercise_dat$under_50, exercise_dat$over_50)$statistic
t.test(exercise_dat$under_50, exercise_dat$over_50,
alternative = "two.sided")

sleep_dat <- readRDS("Final_Exam/sleep_datA.rds")
sleep_dat
sleep_mod <- lm(Sleep ~ exercise, data = sleep_dat)
y <- sleep_dat$Sleep
y
y[141]
length(y)
yhat <- predict(sleep_mod)
yhat <- unname(yhat)
yhat <- append(yhat, 6.99227282809927)
?predict
length(yhat)
summary(sleep_mod)
-0.03392 - 0.02924
-0.03392 + 0.02924

sleep_mod$coefficients
plot(sleep_dat$exercise, sleep_dat$Sleep, xlab = "Exercise", ylab = "Sleep")
predict(sleep_mod, data.frame(exercise = 0))
summary(sleep_mod)$coefficients[2, 1]

res <- y - yhat
res
shapiro.test(res)
