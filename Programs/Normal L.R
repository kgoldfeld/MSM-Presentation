odds <- function (p) {
  return((p/(1 - p)))
}


defB <- defData(varname = "L", formula = 0, variance = 1, 
                dist = "normal")
defB <- defData(defB, varname = "Y0", formula = "-1 + 1.5*L", 
                dist = "binary", link = "logit")
defB <- defData(defB, varname = "Y1", formula = "0 + 1.5*L", 
                dist = "binary", link = "logit")
defB <- defData(defB, varname = "A", formula = "-1 + .25 * L", 
                dist = "binary", link = "logit")
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

for (i in 1: 5000) {
  
  dtB <- genData(100, defB)
  true <- dtB[, log( odds( mean(Y1) ) / odds( mean(Y0) ) )]
  
  exposureModel <- glm(A ~ L, data = dtB, family = "binomial")
  dtB[, pA := predict(exposureModel, type = "response")]
  
  expModel2 <- glm(A ~ 1, data = dtB, family = "binomial")
  dtB[, pAN := predict(expModel2, type = "response")]
  
  dtB <- addColumns(defB2, dtB)
  
  ql <- quantile(x = dtB$IPWs, probs = c(.005))
  qh <- quantile(x = dtB$IPWs, probs = c(.995))
  
  dtB[IPWs < ql, IPWs := ql]
  dtB[IPWs > qh, IPWs := qh]
  
  ql <- quantile(x = dtB$IPW, probs = c(.005))
  qh <- quantile(x = dtB$IPW, probs = c(.995))
  
  dtB[IPW < ql, IPW := ql]
  dtB[IPW > qh, IPW := qh]
  
  
  glmfit <- glm(Y ~ A , data = dtB, family="binomial", weights = IPW)
  estIPW <- coef(glmfit)["A"]
  
  glmfit <- glm(Y ~ A , data = dtB, family="binomial", weights = IPWs)
  estSW <- coef(glmfit)["A"]
  
  result <- rbind(result, data.table(true, estIPW, estSW))
}


result[, .(mean(true), mean(estIPW), mean(estSW), var(estIPW), var(estSW))]

tidy(glm(Y ~ A + L , data = dtB, family="binomial")) 
tidy(glm(Y ~ A , data = dtB, family="binomial"))

