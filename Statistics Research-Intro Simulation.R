# For a single mediator, binary a
simulation_treatment <- rbinom(200, 1, 0.5) # generate binary rv
simulation_outcome <- rnorm(200, 0, 0.5) + rnorm(200, 0, 0.5)
# n=200, mean=100, sd=100
# True values of parameters; set to whatever you want
b0 <- 1
b1 <- 2
b2 <- 3
t0 <- 1
t1 <- 2
t2 <- 3
t3 <- 4
t4 <- 5

# Generating a and c, and deriving m from it
a <- simulation_treatment # assuming control = 0, so sumulation_explanatory = a
c <- rnorm(200, 0, 0.5)
m <- b0 + b1*a + b2*c + rnorm(200, 0, 0.5)
y <- t0 + t1*a + t2*m + t3*a*m + t4*c + rnorm(200, 0, 0.5)
# Fitting a regression model
est_regression_m <- lm(m ~ a + c)
est_regression_m <- coef(est_regression_m)
est_regression_y <- lm(y ~ a + m + a:m + c)
est_regression_y <- coef(est_regression_y)
# Natural direct & indirect effect (calculating effect of treatment on )
NDE <- mean((t1+t3*(b0 + b1*0 + b2*c))*(1-0))
NDE
NIE <- (t2*b1+t3*b1*1)*(1-0)
NIE
est_NDE <- mean((est_regression_y[2]+est_regression_y[4]*(est_regression_m[1] + est_regression_m[2]*0 + est_regression_m[3]*c))*(1-0))
est_NDE
est_NIE <- (t2*b1+0*b1*1)*(1-0)
est_NIE

Single_Mediator <- function(beta=c(1,2,3), theta=c(1,2,3,4,5), treatment, outcome, error){
  # beta is a vector of 3, theta is a vector of 5
  a <- treatment
  c <- error
  m <- beta[1] + beta[2]*a + beta[3]*c + rnorm(200, 0, 0.5)
  y <- theta[1] + theta[2]*a + theta[3]*m + theta[4]*a*m + theta[5]*c + rnorm(200, 0, 0.5)
  print(lm(m ~ a + c))
  print(lm(y ~ a + m + a:m + c))
}
Single_Mediator(treatment=simulation_treatment, 
                outcome=simulation_outcome, error=rnorm(200, 0, 0.5), 
                beta=c(4,5,6), theta=c(4,5,6,7,8))

# Testing formula for direct and indirect effects; look at section 3.2
# Try multiple mediators after; a and c are the same from before
simulation_treatment <- rbinom(200, 1, 0.5) # generate binary rv
simulation_outcome <- rnorm(200, 0, 0.5) + rnorm(200, 0, 0.5)
a <- simulation_treatment # assuming control = 0, so sumulation_explanatory = a
c <- rnorm(200, 0, 0.5)
k <- 2 # number of mediators
m_vec <- matrix(data=0, nrow=length(a), ncol=k) # matrix with k columns
theta_vec <- 1:(k+3) # true values of theta; goes from 1 to k+3, for simplicity
theta_vec
y <- numeric(length(a))
# Code to fill in mediator values
for(i in 1:k){
  z <- b0 + b1*a + b2*c + rnorm(200, 0, 0.5)
  print(length(z))
  m_vec[, i]  <- z
}
# Finding the true value of y
#for(i in 3:k+2){
#  y <- y + theta_vec[i]*m_vec[, i-2]
#}
y <- theta_vec[4]*m_vec[,2] + theta_vec[3]*m_vec[,1] + y
y <- y + theta_vec[1] + theta_vec[2]*a + theta_vec[5]*c + rnorm(200, 0, 0.5)

# Testing the regression
m_vec
lm(m_vec ~ a + c)
# matrix of m_vec coefficients is 0; fix this
lm(y ~ a + m_vec + c)

# Maybe look into weighted approaches
