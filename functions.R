model.a <- function(t, a, b, s)
  exp((s^2/(2*b^2) - a/b)*t +
      (a/b^2 - s^2/b^3)*(1 - exp(-b*t)) +
      s^2/(4*b^3)*(1 - exp(2*b*t)))

model.b <- function(t, b)  1/b * (1 - exp(-b*t))

## Create X and Y, given data (term and yield) and parameters.
factors <- function(t1, y1, t2, y2, p) {
  A.x.1 <- model.a(t1, p['a.x'], p['b.x'], p['s.x'])
  A.x.2 <- model.a(t2, p['a.x'], p['b.x'], p['s.x'])
  A.y.1 <- model.a(t1, a=0, p['b.y'], p['s.y'])
  A.y.2 <- model.a(t2, a=0, p['b.y'], p['s.y'])
  B.x.1 <- model.b(t1, p['b.x'])
  B.x.2 <- model.b(t2, p['b.x'])
  B.y.1 <- model.b(t1, p['b.y'])
  B.y.2 <- model.b(t2, p['b.y'])
  lhs <- matrix(c(B.x.1/t1, B.x.2/t2, B.y.1, B.y.2), nrow=2)
  rhs <- matrix(c(y1 - log(A.x.1*A.y.1)/t1, y2 - log(A.x.2*A.y.2)/t2), nrow=2)
  solve(lhs, rhs)
}

## Input:
##   ts: X and Y time series data frame
##   p: named parameter vector
## Output: D(T) data frame
d.from.factors <- function(ts, p) {
  t <- seq(0.5, 7, 0.5)
  dd <- mapply(compute.dt.seq, ts$x, ts$y, MoreArgs=list(p=p, t=t))
  data.frame(date=ts$date,
             D0.5=dd[1,], D1.0=dd[2,], D1.5=dd[3,], D2.0=dd[4,], D2.5=dd[5,],
             D3.0=dd[6,], D3.5=dd[7,], D4.0=dd[8,], D4.5=dd[9,], D5.0=dd[10,],
             D5.5=dd[11,], D6.0=dd[12,], D6.5=dd[13,], D7.0=dd[14,])
}
compute.dt.seq <- function(t, x, y, p)  sapply(t, compute.dt, x=x, y=y, p=p)
compute.dt <- function(t, x, y, p) {
  A.x.t <- unname(model.a(t, p['a.x'], p['b.x'], p['s.x']))
  A.y.t <- unname(model.a(t, a=0, p['b.y'], p['s.y']))
  B.x.t <- unname(model.b(t, p['b.x']))
  B.y.t <- unname(model.b(t, p['b.y']))
  A.x.t * A.y.t * exp(-B.x.t*x - B.y.t*y)
}

## Input:
##   d: list of data frames (per date) that include t's and D(T)'s
##   t: vector of maturities
## Output: Data frame of date and one column for each par-rate-maturity.
par.from.d <- function(d) {
  t.seq <- c(2,3,5,7)
  par.val <- apply(as.matrix(d[2:ncol(d)]), 1, compute.par.seq, t.seq=t.seq)
  data.frame(date=d$date, t(par.val))
}
compute.par.seq <- function(t.seq, data)  sapply(t.seq, compute.par, data=data)
compute.par <- function(t, data) {
  i <- 2*t
  2*(1 - data[i])/sum(data[1:i])
}

## Input: two matrices
rmse <- function(a, b)  sqrt(sum((a - b)^2))/4

## p: named vector of parameters
vasicek.rmse <- function(p, data) {
  ## Generate time series for factors X and Y.
  u <- mapply(sprintf, data$year, data$month, data$day,
              MoreArgs=list(fmt="%4d-%02d-%02d"))
  v <- mapply(factors, y1=data$cmt0.25, y2=data$cmt10,
              MoreArgs=list(t1=0.25, t2=10, p=p))
  ts <- data.frame(date=u, x=v[1,], y=v[2,])

  ## Generate D(t).
  Dt <- d.from.factors(ts, p)

  ## Compute par rates.
  par <- par.from.d(Dt)

  ## Compute RMSE.
  rmse(as.matrix(par[,2:5]), as.matrix(d[,c(12:15)]))
}
