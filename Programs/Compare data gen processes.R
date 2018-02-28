defU <- defRead("Notebook/msmDef.csv")
dt <- genData(50000, defU)

fitA0 <- glm(A0 ~ L0, data = dt, family=binomial)
fitA1 <- glm(A1 ~ L0 + A0 + L1, data = dt, family=binomial)

dt[, predA0 := predict(fitA0, type = "response")]
dt[, predA1 := predict(fitA1, type = "response")]
dt[, wgt := getWeight(predA0, A0, predA1, A1)]

tidy(lm(Y ~ A0 + A1, weights = wgt, data = dt))

tidy(lm(Y ~ A0 + A1, data=dt))
tidy(lm(Y ~ L0 + L1 + A0 + A1, data=dt))

# No U

defNoU <- defRead("Notebook/noU.csv")
dt <- genData(50000, defNoU)

fitA0 <- glm(A0 ~ L0, data = dt, family=binomial)
fitA1 <- glm(A1 ~ L0 + A0 + L1, data = dt, family=binomial)

dt[, predA0 := predict(fitA0, type = "response")]
dt[, predA1 := predict(fitA1, type = "response")]
dt[, wgt := getWeight(predA0, A0, predA1, A1)]

tidy(lm(Y ~ A0 + A1, weights = wgt, data = dt))

tidy(lm(Y ~ A0 + A1, data=dt))
tidy(lm(Y ~ L0 + L1 + A0 + A1, data=dt))

## Some U

defSomeU <- defRead("Notebook/someU.csv")
dt <- genData(50000, defSomeU)

fitA0 <- glm(A0 ~ L0, data = dt, family=binomial)
fitA1 <- glm(A1 ~ L0 + A0 + L1, data = dt, family=binomial)

dt[, predA0 := predict(fitA0, type = "response")]
dt[, predA1 := predict(fitA1, type = "response")]
dt[, wgt := getWeight(predA0, A0, predA1, A1)]

tidy(lm(Y ~ A0 + A1, weights = wgt, data = dt))

tidy(lm(Y ~ A0 + A1, data=dt))
tidy(lm(Y ~ L0 + L1 + A0 + A1, data=dt))

     