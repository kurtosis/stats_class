MoE <- c(29.8, 33.2, 33.7, 35.3, 35.5, 36.1, 36.2, 36.3, 37.5, 37.7, 38.7, 38.8, 39.6, 41.0, 42.8, 42.8, 43.5, 45.6, 46.0, 46.9, 48.0, 49.3, 51.7, 62.6, 69.8, 79.5, 80.0 )

Strength <- c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 7.0, 7.6, 6.8, 6.5, 7.0, 6.3, 7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 7.4, 7.7, 9.7, 7.8, 7.7, 11.6, 11.3, 11.8, 10.7)

# [Kurt]: In R it's usually best to first put everything in a data frame 
# (each row of df_moe contains the MoE and Strength values of one observation)
df_moe = data.frame(MoE, Strength)

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


# [Kurt]: You don't need to create vectors like this in R, you can just add a scalar to MoE
# and it will broadcast it (i.e. add it to each element of MoE)

# # vector of mean * -1 (length = number of data points)
# m <- rep_len(-meanMoE,27)
# s <- rep_len(-meanStrength,27)
# 
# m_sumDiff <- MoE + m   # new vector with elements (x_i - xbar) 
# S_sumDiff <- Strength + s   # new vectors with elements (y_i - ybar)

m_sumDiff <- MoE - meanMoE
S_sumDiff <- Strength - meanStrength

# gets sum from i= 1 to 27 of (x_i - xbar) * (y_i - ybar)
dot <- sum(m_sumDiff*S_sumDiff)  # 491.8919

# standard deviation of each variable
sd_MoE <- sd(MoE)   # 13.26722
sd_S <- sd(Strength)  # 1.659532


# hand computed r
# r <- (1/26)*(1/sd_MoE)*(1/sd_S)*dot # 0.8592721

# [Kurt]: It's better to use (length(m_sumDiff) - 1) here
# instead of hard-coding "26". The principle is that this value
# is determined by the length of m_sumDiff. If you were ever to change the
# inputs MoE and Strength (say you got new data) you would need to remember
# to go to this line and change the 26 here. This is typically a bad sign if
# you need to edit your code in multiple places when one thing changes.
r <- (1/(length(m_sumDiff) - 1))*(1/sd_MoE)*(1/sd_S)*dot # 0.8592721


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


#############################################################################
#############################################################################
# data frame, ggplot approach
#############################################################################
#############################################################################

library(tidyverse)

df_moe = data.frame(MoE, Strength)
df_moe$moe_shift = df_moe$MoE - mean(df_moe$MoE)
df_moe$strength_shift = df_moe$Strength - mean(df_moe$Strength)

ggplot(data=df_moe) + geom_point(aes(x=MoE, y=Strength))

ggplot(data=df_moe) + geom_boxplot(aes(x="", y=MoE))
ggplot(data=df_moe) + geom_boxplot(aes(x="", y=Strength))

ggplot(data=df_moe) + geom_qq(aes(sample=MoE))
ggplot(data=df_moe) + geom_qq(aes(sample=Strength))

dot <- sum(df_moe$moe_shift*df_moe$strength_shift)  # 491.8919

sd_moe <- sd(df_moe$MoE)
sd_strength <- sd(df_moe$Strength)
r <- (1/( nrow(df_moe) - 1))*(1/sd_moe)*(1/sd_strength)*dot # 0.8592721

# Test for equality
all.equal(r, cor(df_moe$MoE, df_moe$Strength))

beta_hat <- r * (sd_strength / sd_moe)  # 0.1074821
alpha_hat = mean(df_moe$Strength) - beta_hat* mean(df_moe$MoE)

df_moe$predicted_strength <- alpha_hat + beta_hat*df_moe$MoE
df_moe$residual <- df_moe$Strength - df_moe$predicted_strength
df_moe$residual_squared <- df_moe$residual*df_moe$residual
head(df_moe)
summary(df_moe)

manual_SSE <- sum(df_moe$residual_squared)

# fit a linear regression using the glm function
regression_model <- glm(Strength ~ MoE, data=df_moe)
# The model summary display the SSE, referred to as "Residual deviance"
summary(regression_model)

# You can get the value of the SSE directly as regression_model$deviance
# Test for equality with our manual calculation
all.equal(manual_SSE, regression_model$deviance)


