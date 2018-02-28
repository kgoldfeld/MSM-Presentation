odds <- function (p) {
  return((p/(1 - p)))
}

defB <- defData(varname = "L", formula =0.27, 
                dist = "binary")
defB <- defData(defB, varname = "p0", formula = "1/(1+exp(2.5 - 1.75*L))", 
                dist = "nonrandom")
defB <- defData(defB, varname = "Y0", formula = "p0", 
                dist = "binary")
defB <- defData(defB, varname = "p1", formula = "1/(1+exp(1.5 - 1.75*L))", 
                dist = "nonrandom")
defB <- defData(defB, varname = "Y1", formula = "p1", 
                dist = "binary")
defB <- defData(defB, varname = "A", formula = "0.315 + 0.352 * L", 
                dist = "binary")
defB <- defData(defB, varname = "Y", formula = "Y0 + A * (Y1 - Y0)", 
                dist = "nonrandom")



defB2 <- defDataAdd(varname = "pA_actual", 
                    formula = "(A * pA) + ((1 - A) * (1 - pA))", 
                    dist = "nonrandom")

defB2 <- defDataAdd(defB2, varname = "pAN_actual", 
                    formula = "(A * pAN) + ((1 - A) * (1 - pAN))", 
                    dist = "nonrandom")

defB2 <- defDataAdd(defB2, varname = "IPW", 
                    formula = "1/pA_actual", 
                    dist = "nonrandom")

defB2 <- defDataAdd(defB2, varname = "IPWs", 
                    formula = "pAN_actual/pA_actual", 
                    dist = "nonrandom")


set.seed(2002)

result <- data.table()

for (i in 1: 2500) {
  
  dtB <- genData(5000, defB)
  true <- dtB[, log( odds( mean(Y1) ) / odds( mean(Y0) ) )]
  
  dtB[, mean(log( odds( p1 ) / odds( p0 ) ))]
  dtB[, log( odds( mean(p1) ) / odds( mean(p0) ) )]
  
  
  exposureModel <- glm(A ~ L, data = dtB, family = "binomial")
  dtB[, pA := predict(exposureModel, type = "response")]
  
  expModel2 <- glm(A ~ 1, data = dtB, family = "binomial")
  dtB[, pAN := predict(expModel2, type = "response")]
  
  dtB <- addColumns(defB2, dtB)
  
  ql <- quantile(x = dtB$IPWs, probs = c(.005))
  qh <- quantile(x = dtB$IPWs, probs = c(.995))
  
  dtB[IPWs < ql, IPWs := ql]
  dtB[IPWs > qh, IPWs := qh]
  
  glmfit <- glm(Y ~ A , data = dtB, family="binomial", weights = IPW)
  estIPW <- coef(glmfit)["A"]
  
  glmfit <- glm(Y ~ A , data = dtB, family="binomial", weights = IPWs)
  estSW <- coef(glmfit)["A"]
  
  result <- rbind(result, data.table(true, estIPW, estSW))
}


result[, .(mean(true), mean(estIPW), mean(estSW), var(estIPW), var(estSW))]

tidy(glm(Y ~ A + L , data = dtB, family="binomial")) 
tidy(glm(Y ~ A , data = dtB, family="binomial"))

