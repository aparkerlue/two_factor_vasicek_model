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
