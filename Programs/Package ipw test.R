library(ipw)
library(broom)

defMSM <- defRead("Programs/ipwDef.csv")
defMSM

set.seed(1234)
dt <- genData(100000, defMSM)

dl <- melt(
  dt, 
  id.vars = c("id", "U", "Y"), 
  measure.vars = list(c("A0", "A1", "A2"), c("L0", "L1", "L2")),
  value.name = c("A","L"),
  variable.name = "t",
  variable.factor = TRUE
)

setkey(dl, "id")

dl[, prevA:= shift(A, 1, type="lag"), keyby=id]
dl[is.na(prevA), prevA := 0]

dl[, t := as.integer(t)]
dl[, tstart := t - 2]
dl[, futime := tstart + 1]

ipwtest <- ipwtm(
  exposure = A,
  family = "binomial",
  link = "logit", 
  denominator = ~ L + prevA, 
  id = "id",
  tstart = tstart, 
  timevar = futime, 
  type = "all",
  data = data.frame(dl)
)

dl[, ipw := ipwtest$ipw.weights]
wgts <- dl[t==3, ipw]

getWeight3 <- function(predA0, actA0, predA1, actA1, predA2, actA2) {
  predActA0 <- actA0*predA0 + (1-actA0)*(1-predA0)
  predActA1 <- actA1*predA1 + (1-actA1)*(1-predA1)
  predActA2 <- actA2*predA2 + (1-actA2)*(1-predA2)
  p <- c(predActA0 * predActA1 * predActA2)
  return(1/p)
}

fitA0 <- glm(A0 ~ L0, data = dt, family=binomial)
fitA1 <- glm(A1 ~ A0 + L1, data = dt, family=binomial)
fitA2 <- glm(A2 ~ A1 + L2, data = dt, family=binomial)

fitA0 <- glm(A0 ~ L0, data = dt, family=binomial)
fitA1 <- glm(A1 ~ L1, data = dt, family=binomial)
fitA2 <- glm(A2 ~ L2, data = dt, family=binomial)

dt[, predA0 := predict(fitA0, type = "response")]
dt[, predA1 := predict(fitA1, type = "response")]
dt[, predA2 := predict(fitA2, type = "response")]

dt[, wgt := getWeight3(predA0, A0, predA1, A1, predA2, A2)]
dt[, awgt := wgts]


tidy(lm(Y ~ A0 + A1 + A2, weights = wgt, data = dt))
tidy(lm(Y ~ A0 + A1 + A2, weights = awgt, data = dt))




