library(frequencyConnectedness)

log_ret<-readRDS("data/log_ret.RDS")
log_ret<-log_ret[-1] #no date

data(exampleSim)
# Shorten the data, rolling estimation takes quite some time
exampleSim <- exampleSim[1:600,]


# Compute the VAR(2) estimate with constant and save results
est <- VAR(exampleSim, p = 2, type = "const")
#V1 = V1.l1 + V2.l1 + V3.l1 + V1.l2 + V2.l2 + V3.l2 + const
#V2 = V1.l1 + V2.l1 + V3.l1 + V1.l2 + V2.l2 + V3.l2 + const
#V3 = V1.l1 + V2.l1 + V3.l1 + V1.l2 + V2.l2 + V3.l2 + const
#summary(est)

#Get Forecast Error Variance Decomposition (FEVD)
n.ahead = 100
no.corr=F
func="fevd"
x<-est


Phi <- irf(est, n.ahead = n.ahead, boot = F, ortho = F)
# Extract them from standard format
Phi <- lapply(1:(n.ahead + 1), function(j) sapply(Phi$irf, function(i) i[j,]))
# Estimate the covariance matrix of the residuals
Sigma <- t(residuals(est)) %*% residuals(est)/nrow(residuals(est))

# Eliminate the off-diagonal elements of the covariance matrix to only
# see the effects of the coefficients
# This is primarily useful for Lasso.
if (no.corr) {
  Sigma <- diag(diag(Sigma))
}
# Estimate the denominator of the ration of FEVD.
denom <- diag(Reduce('+', lapply(Phi, function(i) i%*%Sigma%*%t(i))))


# This computes the enumerator, essentially compute all the elements of the
# sum and then reduce them using the sum operator.
enum <- Reduce('+', lapply(Phi, function(i)
  ( chol(Sigma) %*% t(i) )^2
)
)

# Compute the ration and return the matrix.
return(
  t(
    sapply(1:ncol(enum), function(i)  enum[,i]/denom[i] )
  )
)


f <- get(func)
tab <- f(est, n.ahead, no.corr = no.corr)
rownames(tab) <- colnames(tab) <- colnames(est$y)
structure(list(tables = list(tab), bounds = c(pi+0.00001, 0), date = NULL), class = "spillover_table")




spilloverDY09(est, n.ahead = 100, no.corr = F)
sp <- spilloverDY12(est, n.ahead = 100, no.corr = T)
overall(sp)
to(sp)
from(sp)
net(sp)
pairwise(sp)

# Get the frequency connectedness on partition (pi,pi/4), (pi/4,0), roughly
# corresponding to movements of 1 to 4 days and 4 to longer.
bounds <- c(pi+0.00001, pi/4, 0)
spilloverBK09(est, n.ahead = 100, no.corr = F, partition = bounds)
collapseBounds(spilloverBK12(est, n.ahead = 100, no.corr = F, partition = bounds), 1:2)


# Get the rolling window estimates
params_est = list(p = 2, type = "const")
sp <- spilloverRollingDY09(log_ret, n.ahead = 100, no.corr = F, "VAR", params_est = params_est, window = 100)
plotOverall(sp)
plotTo(sp)
plotFrom(sp)
