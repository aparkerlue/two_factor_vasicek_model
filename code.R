require(tseries)

source('functions.R')

## Read in CMT data.
d <- read.table("tsdata.txt", header=TRUE, sep="\t")
for (i in c("cmt0.25", "cmt2", "cmt3", "cmt5", "cmt7", "cmt10"))
  d[i] <- d[i] / 100
##head(d[,c(1:3,11:16)])                  # Relevant columns.
##head(d[,c(12:15)])                      # Maturities of 2, 3, 5 and 7 years.

## Assume values for parameters.
p <- c(a.x=0.004109,
       b.x=0.0383212,
       s.x=0.0097854,
       b.y=0.448712,
       s.y=0.018566)

## Minimize RMSE.
##
## Took about 1 hour and 34 minutes.
## 
## > system.time(source('code.R'))
##     user   system  elapsed 
## 5624.946   16.212 5695.069
## > vasicek.rmse.minimization
## $par
##          a.x          b.x          s.x          b.y          s.y 
## 1.081711e-02 1.892359e-01 2.244451e-05 8.901244e-09 1.076640e-14 
## 
## $value
## [1] 0.08312986
## 
## $counts
## function gradient 
##      394       NA 
## 
## $convergence
## [1] 0
## 
## $message
## NULL
## 
## $outer.iterations
## [1] 11
## 
## $barrier.value
## [1] 5.643135e-05
## 
## > save(vasicek.rmse.minimization, file="vasicek.rmse.minimization.RData")
vasicek.rmse.minimization <-
  constrOptim(p, vasicek.rmse, NULL, ui=diag(5), ci=rep(0,5), data=d)

## Use optimized parameters.
p <- vasicek.rmse.minimization$par
ts <- factor.ts(d, p)

## Plot X and Y.
png('Problem 5 Plot.png', width=6, height=6, units='in', res=300)
matplot(as.POSIXct(ts$date), cbind(ts$x, ts$y, ts$x + ts$y),
        type='l', lty=2:4, col=2:4, xaxt="n",
        main="RMSE-Minimized Vasicek Factors and Interest Rate",
        xlab="Date", ylab="Factor Value / Interest Rate")
abline(h=0, col=8)
axis.POSIXct(1, ts$date, format="%Y")
legend(8.5e8, 0.1, expression(r[s], X[s], Y[s]),
       lty=c(4,2,3), col=c(4,2,3), merge=TRUE)
dev.off()

## First moment of X.
mean(ts$x)
unname(p['a.x']/p['b.x'])

## Second moment of X.
var(ts$x)
unname(p['s.x']^2/(2*p['b.x']))

## First moment of Y.
mean(ts$y)
unname(0/p['b.y'])

## Second moment of Y.
var(ts$y)
unname(p['s.y']^2/(2*p['b.y']))

## Par rates.
dt <- d.from.factors(ts, p)
par <- par.from.d(dt)

e <- as.matrix(par[,2:5]) - as.matrix(d[,12:15])
png('Problem 6 Plot.png', width=9, height=6, units='in', res=300)
matplot(as.POSIXct(par$date), e, type='l', lty=1, col=1:4, xaxt='n',
        main="Deviations between Observed and Predicted Par Rates",
        xlab="Date", ylab="Par Rate")
abline(h=0, col=8)
axis.POSIXct(1, ts$date, format="%Y")
legend(8.5e8, -0.007, expression(2-Year, 3-Year, 5-Year, 7-Year),
       lty=1, col=1:4, merge=TRUE)
dev.off()

n.sd.bound <- qnorm(0.025, lower.tail=FALSE)
x <- pacf(e[,1])
pacfval <- x$acf[,,1]
pacfval[abs(x$acf) < n.sd.bound/sqrt(x$n.used)] <- NA
ar(e[,1], order.max=2)
adf.test(e[,1], k=2)                    # 2-year par

x <- pacf(e[,2])
pacfval <- x$acf[,,1]
pacfval[abs(x$acf) < n.sd.bound/sqrt(x$n.used)] <- NA
ar(e[,2], order.max=2)
adf.test(e[,2], k=2)                    # 3-year par

x <- pacf(e[,3])
pacfval <- x$acf[,,1]
pacfval[abs(x$acf) < n.sd.bound/sqrt(x$n.used)] <- NA
ar(e[,3], order.max=2)
adf.test(e[,3], k=2)                    # 5-year par

x <- pacf(e[,4])
pacfval <- x$acf[,,1]
pacfval[abs(x$acf) < n.sd.bound/sqrt(x$n.used)] <- NA
ar(e[,4])
adf.test(e[,4], k=2)                    # 7-year par
