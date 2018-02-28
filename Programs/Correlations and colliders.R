# add U to observed data

dtU <- dtPO[A1==0 & A0==1, .(id, U)]
setkey(dtU, id)
dtX <- dtU[dtObs]

# check correlations

# all correlated

dtX[,cor(A0, U)]
dtX[,cor(A0, U), keyby = .(L0, L1)]
dtX[,cor(A0, U), keyby = .(L0, L1, A1)]
dtX[,cor(A0, U), keyby = .(A1, L1)]

# uncorrelated

dtX[,cor(A0, U), keyby = .(L0)]

dtX[,cor(A1, U), keyby = .(L0, L1, A0)]
dtX[,cor(A1, U), keyby = .(A0, L1)]
