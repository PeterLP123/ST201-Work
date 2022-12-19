## Sample dice roll 100 times using sample() + replace = TRUE
?sample
dice_samps <- sample(1:6, 
                     size = 100,
                     replace = TRUE)


## tabulate and plot with barplot
dice_tab <- table(dice_samps)
barplot(dice_tab)

## get the mean and sd of the samples
mean(dice_samps)
sd(dice_samps)

## Sample 1000 samples of size 100
n_experiment <- 1000
sample_n <- 10

# create vector to store the means
dice_means <- rep(NA,n_experiment)

# loop over the experiments

for(i in 1:n_experiment)
{
  dice_samps <- sample(1:6, 
                       size = sample_n,
                       replace = TRUE)
  dice_means[i] <- mean(dice_samps)
}
  

## create a histogram of the means
hist(dice_means)

## population parameters for dice roll incl. standard error

## CI for dice roll true mean - get a single sample of size 40

N <- 40

dice_exp <- sample(1:6, 
                   size = N,
                   replace = TRUE)


## estimates based on sample means
xbar <- mean(dice_exp)
s <- sd(dice_exp)
se <- s/sqrt(N)

## CI
xbar - 1.96*se
xbar + 1.96*se

## show the normal curve
x <- seq(-4,4,by = 0.1)
norm_dens <- dnorm(x,0,1)

plot(x, norm_dens, type = "l")
abline(v = qnorm(0.05, mean = 0, sd = 1), col = "red")
abline(v = qnorm(0.95, 0,1), col = "red")
# get the z value and the CI

# 95% CI (2.72,3.78)

## Normal dist compared to t dist

#A study is conducted to measure the grams of protein for a sample of energy bars. 
#The label claims that the bars have 20 grams of protein. We want to know if the labels are correct or not.
x <- c(14.98, 28.02, 26.24, 24.52, 15.00, 21.04, 21.25, 24.19, 21.57, 14.43, 22.95, 24.70, 28.86, 20.64, 19.81,
       17.24, 21.57, 18.74, 19.53, 18.42)
N <- length(x)

## Check normality assumptions

# plot hist
hist(x)

# qqnorm() + qqline()
qqnorm(x)
qqline(x)

# shapiro.test()
# Ho: data come from a normal distribution
# Ha: not normal
## alpha = 5% (0.05)
shapiro.test(x)

## Get the CI
# what does the t-distribution look like
t <- seq(-4,4,by = 0.1)

t_dens <- dt(t,df = N-1)

plot(t, t_dens, type = "l")
abline(v = 2.09)
abline(v = -2.09)
abline(v = 1.96)
abline(v = -1.96)


# get the t value
t_value <- qt(0.05,df = N-1)

# 95% CI 
s <- sd(x)
se <- s/sqrt(N)

xbar <- mean(x)

xbar - t_value*se
xbar + t_value*se