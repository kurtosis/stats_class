# Here's a function to draw random numbers and calculate x_max, etc.
# You might want to look up the replicate, max, runif functions
# And make sure you understand what they do first.

get_sample_stats <- function(sample_size, n_trials, property="max") {
  # repeat the following process n_trials times:
  # - draw a sample of size sample_size from uniform dist on [0,1]
  # - compute the desired statistic of each sample (for example the max)
  # Btw, formally speaking I probably should've used the name "statistic" instead of "property"
  if (property=="max") {
    stat_sample <- replicate(n_trials, max(runif(sample_size)))
  } else if (property=="min") {
    stat_sample <- replicate(n_trials, min(runif(sample_size)))  
  } else if (property=="median") {
    stat_sample <- replicate(n_trials, median(runif(sample_size)))
  } else {
    # default is to compute the mean if property is not one of the above
    stat_sample <- replicate(n_trials, mean(runif(sample_size)))
  }
  sample_mean <- mean(stat_sample)
  sample_standard_dev <- sd(stat_sample)
  sample_standard_error <- sample_standard_dev / sqrt(length(stat_sample))
  return (c(sample_mean, sample_standard_dev, sample_standard_error))
}


# If you run this command it will repeat the process: "draw 2 values and find the max"
# for 100 times (or "trials").
# Then it will print the mean, standard deviation, and standard error for all 100 trials.
get_sample_stats(2, 100)

# Now let's do some numerical experimentation.
# First let's run this command a few times with n_trials=10 (the first for loop)
# When it prints the results look at the sample mean (first number) and standard deviation (the middle number)
# You should see they both bounce around a bit
for (i in 1:4) {
  print(get_sample_stats(2, 10))
}
# Now try running a similar loop with n_trials = 1000 and 100000
# The bigger n_trials gets, the less they bounce around.
# This makes sense, if you have a ton of data you are pretty much capturing
# the real sampling distribution.
# In this case, it looks like mean = 0.667 and std dev = 1/sqrt(18) = 0.2357023
# Maybe you can work out the math to prove why that's the answer.
for (i in 1:4) {
  print(get_sample_stats(2, 1000))
}
for (i in 1:4) {
  print(get_sample_stats(2, 100000))
}


# Now let's see what happens when we increase sample_size
# This is like your hw question about n=2 vs n=100.
# For this part I made n_trials really big so we should get
# close to the true values of the sampling distributions mean and standard deviation
# Look at sample_mean (the first number) in each case.
# Can you see the pattern for how sample_mean depends on sample_size?
N_TRIALS = 100000
get_sample_stats(1, N_TRIALS)
get_sample_stats(2, N_TRIALS)
get_sample_stats(3, N_TRIALS)
get_sample_stats(4, N_TRIALS)
get_sample_stats(5, N_TRIALS)
get_sample_stats(6, N_TRIALS)
get_sample_stats(10, N_TRIALS)
get_sample_stats(100, N_TRIALS)

# We can do the same thing, but using at the minimum value in the sample, instead of the max.
get_sample_stats(1, N_TRIALS, property='min')
get_sample_stats(2, N_TRIALS, property='min')
get_sample_stats(3, N_TRIALS, property='min')
get_sample_stats(4, N_TRIALS, property='min')
get_sample_stats(5, N_TRIALS, property='min')
get_sample_stats(6, N_TRIALS, property='min')
get_sample_stats(10, N_TRIALS, property='min')
get_sample_stats(100, N_TRIALS, property='min')


# This idea occured to me - what if instead of the max, we look at the median or the mean of the sample?
# Intuitively you would think that on average, both of them will be 0.5 
# (since that is the mean/median of the uniform distribution you're drawing from)
# That's true (see sample_mean). BUT you will notice that, for bigger sample_size,
# the standard deviation for the median is larger than the standard deviation for the mean
# In other words, if I draw a sample of n=100, x_median will probably stray farther from 0.5
# than x_mean will. Can you think about why that is? It's kind of a subtle point, I think you just
# have to get sort of a gut feel for it- I don't have a proof.
# Note that if sample_size is 1 or 2 then the mean and median are the same, so they'll have the same
# standard deviation in those cases.
get_sample_stats(1, N_TRIALS, property='median')
get_sample_stats(1, N_TRIALS, property='mean')

get_sample_stats(2, N_TRIALS, property='median')
get_sample_stats(2, N_TRIALS, property='mean')

get_sample_stats(10, N_TRIALS, property='median')
get_sample_stats(10, N_TRIALS, property='mean')

get_sample_stats(100, N_TRIALS, property='median')
get_sample_stats(100, N_TRIALS, property='mean')
