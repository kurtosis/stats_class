library(tidyverse)

# This is a quick demonstration to convince you that, when you are computing a sample variance,
# it really is better to divide by (n-1) instead of dividing by n.
# I show this by drawing a sample of size n=2 from a normal distribution with variance=1.
# Then I show that, if you divide by (n-1), on average you will correctly estimate that variance=1
# But if you divide by n, then on average you will estimate that variance=0.5

# The mathematical proof of why (n-1) is the right method is a bit tricky.
# This numerical experiment is just to convince you it's right, even if you don't totally understand why.
# These links explain the reason a bit more if you're interested:
# https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/variance-standard-deviation-sample/a/population-and-sample-standard-deviation-review
# https://www.khanacademy.org/math/ap-statistics/summarizing-quantitative-data-ap/more-standard-deviation/v/review-and-intuition-why-we-divide-by-n-1-for-the-unbiased-sample-variance

# Also - this code uses a lot of tidyverse/dplyr functions like tibble, mutate, rowwise
# It probably looks confusing at first, but you can read more in the R book to understand these better.

N_TRIALS=10000

# First let's create a data frame (also called a "tibble" if you're using tidyverse) where each
# row contains two draws from the "standard normal distribution" N(mean=0, std dev = 1)
# In other words, each row represents a separate "trial", where we draw a sample of size n=2 from the standard normal distribution
samples = tibble(
  x1 = replicate(N_TRIALS, rnorm(1, mean=0, sd=1)),
  x2 = replicate(N_TRIALS, rnorm(1, mean=0, sd=1))
  )

# Let's look at the first few rows just so we know what we did.
head(samples)

# Now we are going to add another column called x_mean
# For each row, x_mean is the average of our two observations x1, x2
# In other words, x_mean is the sample mean
# 'mutate' means "create a new column that is some function of the other columns
# I also had to include the rowwise command here to get this to work right (because I'm calling the function 'mean')
# I'll admit this is kind of a weird glitch in dplyr, just go with it for now:)
# Eventually it will make sense
samples = samples %>% rowwise() %>%
  mutate(
  x_mean = mean(c(x1, x2))
)

# Let's take a look at what we just did
head(samples)

# Now let's add two more columns diff1 and diff2
# These are just the differences between x1, x2 and x_mean
samples = samples %>% mutate(
  diff1 = x1 - x_mean,
  diff2 = x2 - x_mean
)

# Take a look again!
head(samples)

# Now let's "estimate" the variance for each row
# In other words, the variance of the two observations we have in each row
# When I say "estimate" here what I mean is - "Given the data we have observed, what
# is the best guess we can make for the value of the true population variance?"
# First I computed the "wrong" estimate, where you divide by n (remember n=2)
# Then I computed the "right" estimate where you divide by n-1
samples = samples %>% mutate(
  variance_wrong = (diff1^2 + diff2^2)/2,
  variance_right = (diff1^2 + diff2^2)/(2-1)
)


# Now let's look at the average of our "estimated variance" over all the rows.
# In other words, we just did 10,000 different trials 
# For each trial, we drew n=2 observations and tried to estimate what the 
# true population variance is for the distribution we drew from
# Remember this distribution is N(0,1) so we know its true population variance (and std dev) is 1
# It turns out that if we use the wrong formula, then on average we would end up estimating that variance=0.5
# BUT if we use the right formula then on average we would estimate that variance=1, which is correct!
mean(samples$variance_wrong)
mean(samples$variance_right)


# ADVANCED TOPIC
# Finally, instead of variance, we can ask about standard deviation
# In other words "on average, what would we estimate the standard deviation to be,
# if we use the formula sample_standard_devation = sqrt(sample_variance)?"
# This is a more complicated topic - I wouldn't worry about trying to understand this right now.
# BUT, just so you know, it turns out that even our "right" estimate doesn't give us the correct value on average
# (The correct value is 1)
# Still the right estimate is closer to 1 than the wrong estimate is.
# (The reason that even our 'right' estimate isn't actually right has to do with the fact that we're taking a square root
# but I'm not going to try to explain that right now...it's complicated!)
samples = samples %>% mutate(
  std_dev_wrong = sqrt(variance_wrong),
  std_dev_right = sqrt(variance_right)
)
mean(samples$std_dev_wrong)
mean(samples$std_dev_right)


# As a last point, we could also use the summary function to see the mean of every column in the data frame
summary(samples)

