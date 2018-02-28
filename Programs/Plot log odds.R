odds <- function (p) {
  return((p/(1 - p)))
}

defB <- defData(varname = "L", formula =0.5, 
                dist = "binary")
defB <- defData(defB, varname = "Y0", formula = "-3.0 + 2.0*L", 
                dist = "binary", link = "logit")
defB <- defData(defB, varname = "Y1", formula = "-1.0 + 2.0*L", 
                dist = "binary", link = "logit")
defB <- defData(defB, varname = "A", formula = "0.35 + 0 * L", 
                dist = "binary")
defB <- defData(defB, varname = "Y", formula = "Y0 + A * (Y1 - Y0)", 
                dist = "nonrandom")

set.seed(2017)
  
dtB <- genData(5000, defB)

tidy(glm(Y ~ A + L , data = dtB, family="binomial")) 
tidy(glm(Y ~ A , data = dtB, family="binomial"))

dtP <- dtB[, .(p1 = mean(Y1), p0 = mean(Y0)), keyby=L]
dtP[, `:=`(lo1 = log(odds(p1)), lo0 = log(odds(p0)))]
dtP[, lor := lo1 - lo0]

dtAvg <- dtB[, .(p1 = mean(Y1), p0 = mean(Y0))]
dtAvg[, `:=`(lo1 = log(odds(p1)), lo0 = log(odds(p0)))]
dtAvg[, lor := lo1 - lo0]

mP <- melt(data = dtP, 
           id.vars = c("L"), 
           measure.vars = list(c("p0","p1"),c("lo0","lo1")), 
           value.name = c("p","lo"),
           variable.factor = TRUE
      )

dtAvg[, L:= 999]
mAvg <- melt(data = dtAvg, 
             id.vars = c("L"), 
             measure.vars = list(c("p0","p1"),c("lo0","lo1")), 
             value.name = c("p","lo"),
             variable.factor = TRUE
)

pprob <- ggplot(data = mP, aes(x=variable, y = p)) +
  geom_point() +
  geom_line(aes(group = L)) +
  scale_y_continuous(limits=c(0,.8), name = "P(potential outcome = 1)") +
  scale_x_discrete(labels=c("No treatment", "Treatment")) +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  annotate(geom = "text", x=c(2.13, 2.13), y = c(.26, .74), label=c("L = 0", "L = 1"))

plo <- ggplot(data = mP, aes(x=variable, y = lo)) +
  geom_point() +
  geom_line(aes(group = L)) +
  scale_y_continuous(name = "Log odds P(potential outcome = 1)") +
  scale_x_discrete(labels=c("No treatment", "Treatment")) +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  annotate(geom = "text", x=c(2.13, 2.13), y = c(-1, 1), label=c("L = 0", "L = 1"))


png(filename = 'Slides/img/logoddsNoAvg.png', width = 480, height = 480)
gridExtra::grid.arrange(pprob, plo, nrow = 2)
dev.off()


pprob <- ggplot(data = mP, aes(x=variable, y = p)) +
  geom_point() +
  geom_line(aes(group = L)) +
  geom_point(data = mAvg, aes(x=variable, y = p ), color = "red") +
  geom_line(data =  mAvg, aes(x=variable, y = p, group = L), 
            color = "red", lty=3) +
  scale_y_continuous(limits=c(0,.8), name = "P(potential outcome = 1)") +
  scale_x_discrete(labels=c("No treatment", "Treatment")) +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  annotate(geom = "text", x=c(2.13, 2.13), y = c(.26, .74), label=c("L = 0", "L = 1"))

biasline <- mAvg[variable == 1, lo] + 2

plo <- ggplot(data = mP, aes(x=variable, y = lo)) +
  geom_point() +
  geom_line(aes(group = L)) +
  geom_point(data = mAvg, aes(x=variable, y = lo ), color = "red") +
  geom_line(data =  mAvg, aes(x=variable, y = lo, group = L), 
            color = "red", lty=3) +
  # geom_hline(yintercept = biasline, color = "grey60", lty = 3) +
  scale_y_continuous(name = "Log odds P(potential outcome = 1)") +
  scale_x_discrete(labels=c("No treatment", "Treatment")) +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  annotate(geom = "text", x=c(2.13, 2.13), y = c(-1, 1), label=c("L = 0", "L = 1"))


png(filename = 'Slides/img/logoddsAvg.png', width = 480, height = 480)
gridExtra::grid.arrange(pprob, plo, nrow = 2)
dev.off()
