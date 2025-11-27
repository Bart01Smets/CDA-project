### Exercise 2 ###
# Data input
ntot <- 2142
n <- ntot/6 #(357)

age <- c(11, 11, 13, 13, 15, 15)
gender <- c("Male", "Female", "Male", "Female", "Male", "Female")
mean_vector <- c(4.56, 4.38, 8.78, 8.30, 15.94, 15.13)
sds_vector <- c(4.10, 4.16, 6.97, 6.57, 16.21, 12.90)

x1 <- c(1, 0, 0, 0) # 11yo, male
x2 <- c(1, 0, 0, 1) # 11yo, female
x3 <- c(1, 1, 0, 0) # 13yo, male
x4 <- c(1, 1, 0, 1) # 13yo, female
x5 <- c(1, 0, 1, 0) # 15yo, male
x6 <- c(1, 0, 1, 1) # 15yo, female

# check simulation
n_sim <- 10000
nr_of_betas <- length(x1)
beta_simulation_vector <- matrix(rep(0, n_sim * nr_of_betas), ncol = nr_of_betas)
mse_simulation_vector <- rep(0, n_sim)
sse_simulation_vector <- rep(0, n_sim)

for(i in seq(1,n_sim)){
  
  X_simulation <- rbind(
    matrix(rep(x1, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x2, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x3, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x4, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x5, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x6, times = n), nrow = n, byrow = TRUE)
  )
  
  Y_simulation <- c(mapply(
    function(m, s) rnorm(n, mean = m, sd = s),
    mean_vector,
    sds_vector
  ))
  
  beta_simulation_vector[i,] <- solve(t(X_simulation) %*% X_simulation) %*% t(X_simulation) %*% Y_simulation
  # check fit means
  df_simulation <- data.frame(X_simulation, y = Y_simulation)
  colnames(df_simulation) <- c("intercept", "age13", "age15", "female", "y")
  fit_simulation <- lm(y ~ age13 + age15 + female, df_simulation)
  beta_simulation_vector[i,] <- coef(fit_simulation)
  res <- residuals(fit_simulation)
  mse_simulation_vector[i] <- mean(res^2)
  sse_simulation_vector[i] <- sum(res^2)
}
apply(beta_simulation_vector, 2, mean)
mean(mse_simulation_vector)
mean(sse_simulation_vector)

#### Met 10000 simulaties
# > apply(beta_simulation_vector, 2, mean)
# [1]  4.7186454  4.0673794 11.0621381 -0.4923581
# > mean(mse_simulation_vector)
# [1] 92.31798
# > mean(sse_simulation_vector)
# [1] 197745.1




### Exercise 3 ###
ntot <- 2142
n <- ntot/6 #(357)

age <- c(11, 11, 13, 13, 15, 15)
gender <- c("Male", "Female", "Male", "Female", "Male", "Female")
mean_vector <- c(4.56, 4.38, 8.78, 8.30, 15.94, 15.13)
sds_vector <- c(4.10, 4.16, 6.97, 6.57, 16.21, 12.90)

x1 <- c(1, 0, 0, 0) # 11yo, male
x2 <- c(1, 0, 0, 1) # 11yo, female
x3 <- c(1, 1, 0, 0) # 13yo, male
x4 <- c(1, 1, 0, 1) # 13yo, female
x5 <- c(1, 0, 1, 0) # 15yo, male
x6 <- c(1, 0, 1, 1) # 15yo, female


# check simulation
n_sim <- 100000
nr_of_betas <- 6
beta_simulation_vector <- matrix(rep(0, n_sim * nr_of_betas), ncol = nr_of_betas)
mse_simulation_vector <- rep(0, n_sim)
sse_simulation_vector <- rep(0, n_sim)

for(i in seq(1,n_sim)){
  
  X_simulation <- rbind(
    matrix(rep(x1, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x2, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x3, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x4, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x5, times = n), nrow = n, byrow = TRUE),
    matrix(rep(x6, times = n), nrow = n, byrow = TRUE)
  )
  
  Y_simulation <- c(mapply(
    function(m, s) rnorm(n, mean = m, sd = s),
    mean_vector,
    sds_vector
  ))
  
  # # extra columns
  # x1_extra <- c(1, 0, 0, 0, 0, 0) # 11yo, male
  # x2_extra <- c(1, 0, 0, 1, 0, 0) # 11yo, female
  # x3_extra <- c(1, 1, 0, 0, 0, 0) # 13yo, male
  # x4_extra <- c(1, 1, 0, 1, 1, 0) # 13yo, female
  # x5_extra <- c(1, 0, 1, 0, 0, 0) # 15yo, male
  # x6_extra <- c(1, 0, 1, 1, 0, 1) # 15yo, female
  # X_simulation_extra <- rbind(
  #   matrix(rep(x1_extra, times = n), nrow = n, byrow = TRUE),
  #   matrix(rep(x2_extra, times = n), nrow = n, byrow = TRUE),
  #   matrix(rep(x3_extra, times = n), nrow = n, byrow = TRUE),
  #   matrix(rep(x4_extra, times = n), nrow = n, byrow = TRUE),
  #   matrix(rep(x5_extra, times = n), nrow = n, byrow = TRUE),
  #   matrix(rep(x6_extra, times = n), nrow = n, byrow = TRUE)
  # )
  # solve(t(X_simulation_extra) %*% X_simulation_extra) %*% t(X_simulation_extra) %*% Y_simulation
  # beta_simulation_vector[i,] <- solve(t(X_simulation_extra) %*% X_simulation_extra) %*% t(X_simulation_extra) %*% Y_simulation
  # 
  
  # check fit means
  df_simulation <- data.frame(X_simulation, y = Y_simulation)
  colnames(df_simulation) <- c("intercept", "age13", "age15", "female", "y")
  fit_simulation <- lm(y ~ age13 + age15 + female + age13*female + age15*female, df_simulation)
  beta_simulation_vector[i,] <- coef(fit_simulation)
  res <- residuals(fit_simulation)
  mse_simulation_vector[i] <- mean(res^2)
  sse_simulation_vector[i] <- sum(res^2)
}
apply(beta_simulation_vector, 2, mean)
mean(mse_simulation_vector)
mean(sse_simulation_vector)

### Na 100 000 simulaties
# > apply(beta_simulation_vector, 2, mean)
# [1]  4.5602408  4.2190696 11.3774880 -0.1801640 -0.2992455 -0.6258439
# > mean(mse_simulation_vector)
# [1] 92.25154
# > mean(sse_simulation_vector)
# [1] 197602.8