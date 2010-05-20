model.a <- function(t, a, b, s)
  exp((s^2/(2*b^2) - a/b)*t +
      (a/b^2 - s^2/b^3)*(1 - exp(-b*t)) +
      s^2/(4*b^3)*(1 - exp(2*b*t)))

model.b <- function(t, b)  1/b * (1 - exp(-b*t))

## Create X and Y, given data and parameters.
factors <- function(t1, y1, t2, y2, a.x, b.x, s.x, b.y, s.y) {
  A.x.1 <- model.a(t1, a.x, b.x, s.x)
  A.x.2 <- model.a(t2, a.x, b.x, s.x)
  A.y.1 <- model.a(t1, a=0, b.y, s.y)
  A.y.2 <- model.a(t2, a=0, b.y, s.y)
  B.x.1 <- model.b(t1, b.x)
  B.x.2 <- model.b(t2, b.x)
  B.y.1 <- model.b(t1, b.y)
  B.y.2 <- model.b(t2, b.y)
  lhs <- matrix(c(B.x.1/t1, B.x.2/t2, B.y.1, B.y.2), nrow=2)
  rhs <- matrix(c(y1 - log(A.x.1*A.y.1)/t1, y2 - log(A.x.2*A.y.2)/t2), nrow=2)
  solve(lhs, rhs)
}

## Input:
##   ts: X and Y time series data frame
##   p: named parameter vector
## Output: D(T) data frame
d.from.factors <- function(ts, p) {
  r <- list()
  for (i in 1:nrow(ts)) {
    date <- ts$date[i]
    r[[date]] <- data.frame()
    for (j in seq(0, 10, 0.5)) {
      A.x.j <- unname(model.a(j, p['a.x'], p['b.x'], p['s.x']))
      A.y.j <- unname(model.a(j, a=0, p['b.y'], p['s.y']))
      B.x.j <- unname(model.b(j, p['b.x']))
      B.y.j <- unname(model.b(j, p['b.y']))
      Dt <- A.x.j*A.y.j*exp(-B.x.j*ts$x[i] - B.y.j*ts$y[i])
      r[[date]] <- rbind(r[[date]], data.frame(t=j, Dt=Dt))
    }
  }
  return(r)
}

## Input:
##   d: list of data frames (per date) that include t's and D(T)'s
##   t: vector of maturities
## Output: Data frame of date and one column for each par-rate-maturity.
par.from.d <- function(d) {
  t <- c(2,3,5,7)
  r <- data.frame()
  for (i in names(d)) {                 # FIXME: hardcoding maturities
    r <- rbind(r, data.frame(date=i, p2=0, p3=0, p5=0, p7=0))
    i.Dt <- 2*t[1] + 1                  # FIXME: hack
    r$p2[nrow(r)] <- 2*(1 - d[[i]]$Dt[i.Dt])/sum(d[[i]]$Dt[1:i.Dt])
    i.Dt <- 2*t[2] + 1                  # FIXME: hack
    r$p3[nrow(r)] <- 2*(1 - d[[i]]$Dt[i.Dt])/sum(d[[i]]$Dt[1:i.Dt])
    i.Dt <- 2*t[3] + 1                  # FIXME: hack
    r$p5[nrow(r)] <- 2*(1 - d[[i]]$Dt[i.Dt])/sum(d[[i]]$Dt[1:i.Dt])
    i.Dt <- 2*t[4] + 1                  # FIXME: hack
    r$p7[nrow(r)] <- 2*(1 - d[[i]]$Dt[i.Dt])/sum(d[[i]]$Dt[1:i.Dt])
  }
  return(r)
}

## Input: two matrices
rmse <- function(a, b)  sqrt(sum((a - b)^2))/4

## Input: named vector of parameters
vasicek.rmse <- function(p, d) {
  ## Generate time series for factors X and Y.
  r <- data.frame(date=numeric(0), x=numeric(0), y=numeric(0))
  for (i in 1:nrow(d)) {
    xy <- factors(t1=0.25, y1=d$cmt0.25[i], t2=10, y2=d$cmt10[i],
                  a.x=p[1], b.x=p[2], s.x=p[3], b.y=p[4], s.y=p[5])
    r <- rbind(r, c(0, xy))
    r[i,1] <- sprintf("%4d-%02d-%02d", d$year[i], d$month[i], d$day[i])
  }
  names(r) <- c("date", "x", "y")

  ## Generate D(t).
  Dt <- d.from.factors(r, p)

  ## Compute par rates.
  par <- par.from.d(Dt)

  ## Compute RMSE.
  rmse(as.matrix(par[,2:5]), as.matrix(d[,c(12:15)]))
}
