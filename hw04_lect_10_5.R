MoE <- c(29.8, 33.2, 33.7, 35.3, 35.5, 36.1, 36.2, 36.3, 37.5, 37.7, 38.7, 38.8, 39.6, 41.0, 42.8, 42.8, 43.5, 45.6, 46.0, 46.9, 48.0, 49.3, 51.7, 62.6, 69.8, 79.5, 80.0 )

Strength <- c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 7.0, 7.6, 6.8, 6.5, 7.0, 6.3, 7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 7.4, 7.7, 9.7, 7.8, 7.7, 11.6, 11.3, 11.8, 10.7)


# part a)

plot(MoE, Strength)

# part b)

par(mfrow = c(1,2))
boxplot(MoE)
boxplot(Strength)

# part c)

par(mfrow = c(1,2))
qqnorm(MoE)
qqnorm(Strength)


# part d)

# compute mean of each vector
meanMoE <- mean(MoE)    # 45.10741
meanStrength <- mean(Strength) # 8.140741


# vector of mean * -1 (length = number of data points)
m <- rep_len(-meanMoE,27)
s <- rep_len(-meanS,27)

m_sumDiff <- MoE + m   # new vector with elements (x_i - xbar) 
S_sumDiff <- Strength + s   # new vectors with elements (y_i - ybar)

# gets sum from i= 1 to 27 of (x_i - xbar) * (y_i - ybar)
dot <- sum(m_sumDiff*S_sumDiff)  # 491.8919

# standard deviation of each variable
sd_MoE <- sd(MoE)   # 13.26722
sd_S <- sd(Strength)  # 1.659532


# hand computed r
r <- (1/26)*(1/sd_MoE)*(1/sd_S)*dot # 0.8592721


# part d)

cor(MoE, Strength) # 0.8592721   matches with hand computed r


# part e)

# beta hat = r * (s_y / s_x) (x var = MoE, y var = Strength)
beta_hat <- r * (sd_S / sd_MoE)  # 0.1074821
beta_hat

# alpha hat = ybar - beta_hat * xbar
alpha_hat = meanStrength - beta_hat*meanMoE
alpha_hat   # 3.2925

# part g)

# The slop of the OLS equation is 0.1074821.   This means that the "best" fit model predicts that for every increase of 1 in the MoE variable, that the increase in Strength will be 0.1074821.   The Strength variable changes more slowly respect to the MoE variable. 

# part h)

# Using this model, y_hat(39.0) = 3.2925 + (0.1074821 * 39.0) = 7.4843







# part i)

# SSE = sum(i=1 to 27) (y_i - y_hat_i)^2

beta_hat_vector <- rep_len(beta_hat,27)

# vector or product of each MoE data point and beta_hat
beta_hat_x_i_vector <- (MoE*beta_hat_vector)  

alpha_hat_vector <- rep_len(alpha_hat,27)

# y_hat_i vector (y_hat_i = alpha_hat + beta_hat*x_i)
y_hat_i_vector <- (alpha_hat_vector + beta_hat_x_i_vector )
y_hat_i_vector 

diff_squared_vector <- ((Strength - y_hat_i_vector)^2)
diff_squared_vector


############## NOT SURE IF THIS IS THE RIGHT RESULT ###########

SSE <- sum(diff_squared_vector)
SSE

