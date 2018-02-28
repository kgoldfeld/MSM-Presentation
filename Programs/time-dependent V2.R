library(broom)

invlogit <- function(x) {
  1/(1+exp(-x))
}


genTimeDep <- function(id, T, k, Theta, Gamma) {
  
  U <- vector(mode = "numeric", length = T+1)
  e <- vector(mode = "numeric", length = T+1)
  L <- vector(mode = "numeric", length = T+1)
  A <- vector(mode = "numeric", length = T+2)
  lambda <- vector(mode = "numeric", length = T+1)
  Y <- vector(mode = "numeric", length = T+1)
  Delta <- vector(mode = "numeric", length = T)
  pdeath <- vector(mode = "numeric", length = T+1)
  
  tstar <- as.integer(NA)
  death = T+1
  
  U[1] <- runif(1, 0, 1)
  e[1] <- rnorm(1, 0, 20)
  L[1] <- qgamma(U[1], shape = 3, scale = 154)
  
  A[1] <- 0 # pre-baseline
  # A[2] <- rbinom(1,1, p = invlogit(Theta[1] + Theta[3]*(L[1] - 500))) # baseline
  A[2] <- rbinom(1,1,.4)
  
  if (A[2] == 1) tstar <- as.integer(0) # baseline
  
  lambda[1] <- invlogit(Gamma[1] + Gamma[3] * A[2])
  pdeath[1] <- lambda[1]
  
  if (pdeath[1] >= U[1]) {
    Y[1] <- 1 # died just after baseline
    death <- 0
  } else {
    Y[1] <- 0
  }
  
  for (j in (1:T)) {
    
    if (Y[j] == 1) {
      # print("died")
      break
    }
    
    Delta[j] <- rnorm(1, 0, 0.05)
    U[j+1] <- min(1, max(0, U[j] + Delta[j]))
    
    if ( (j %% k) != 0) {
      
      L[j+1] <- L[j]
      A[j+2] <- A[j+1]
      
    } else {
      
      e[j+1] <- rnorm(1, 100 * (U[j+1] - 2), 50)
      L[j+1] <- max(0, L[j] + 150 * A[j + 2 - 5] * A[j + 2 - 6] + e[j+1])
      
      if (A[j+1] == 0) {
        
        # A[j+2] <- rbinom(1,1, 
        #                  p = invlogit(Theta[1] + Theta[2] * j + Theta[3]*(L[j+1] - 500))
        #           )
        
        A[j+2] <- rbinom(1,1, .25)
        
      } else{
        
        A[j+2] <- 1
        
      }
      
      if ((A[j+2] == 1) & (A[j+1] == 0)) tstar <- as.integer(j)
      
    }
    
    if (A[j+2] == 0) {
      pre[j+1] <- period
      post <- as.integer(0)
    } else {
      
    }
    
    

    # print(j)

    if (is.na(tstar)) {
      
      logitp <- Gamma[1] + 
        Gamma[2] * ((1 - A[j + 2]) * j)  + 
        Gamma[3] * A[j + 2] 
      
    } else {
      
      logitp <- Gamma[1] + 
        Gamma[2] * (( 1 - A[j + 2]) * j + ( A[j + 2] * tstar ) ) + 
        Gamma[3] * A[j + 2] +
        Gamma[4] * ( A[j + 2] * (j + 2 - tstar) ) 
    }
    
    lambda[j+1] <- invlogit(logitp)
    
    pdeath[j+1] <- 1 - prod(1 - lambda[1:(j+1)])
    
    # print(pdeath)
    
    if (pdeath[j+1] >= U[1]) {
      
      # print("hello")
      # print(j)
      
      Y[j+1] <- 1
      death <- j
    }
    
  }
  
  dti <- data.table(id, period = 0:T, L, A=A[2:(T+2)], U , Y, 
                    death, tstar, lambda, pdeath)
  
  dti <- dti[death >= period]
  
  ####### NEED TO FIX PRE
  
  
  # dti[, dperiod := period + 1]
  
  return(dti)

}


Theta <- c(-0.405, 0.0205, -0.00405)
Gamma <- c(-3.00, 0.05, -1.50, 0.10)

T = 40
k = 5

# test
set.seed(331)
set.seed(333)
set.seed(334)

d1 <- genTimeDep(1, T, k, Theta, Gamma)

set.seed(7)
n <- 20000
dt <- data.table()
for (id in 1:n) {
  dt <- rbind(dt, genTimeDep(id, T, k, Theta, Gamma))
}

# pooled logistic

glmfit <- glm(Y ~ pre + A + post + factor(period), data=dt, family = binomial(logit))

tidy(glmfit)[1:4, ]

# weighted
library(ipw)

ipwEst <- ipwtm(
  exposure = A,
  family = "binomial",
  link = "logit", 
  denominator = ~ I(L-500) + period, 
  id = "id",
  timevar = period, 
  type = "first",
  data = data.frame(dt)
)



# need to make sure L is aligned with proper A in data ...

dt[, wgts := ipwEst$ipw.weights]

adjfit <- glm(Y~pre + A + post + factor(period), data=dt, 
              family = binomial(logit), weights = wgts)

tidy(adjfit)[1:4,]

