
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
n_sim <- 1000
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
  
  beta_simulation_vector[i,] <- solve(t(X_simulation) %*% X_simulation) %*% t(X_simulation) %*% Y_simulation
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