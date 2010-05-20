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
r <- constrOptim(p, vasicek.rmse, NULL, ui=t(c(0, 1, 0, 0, 0)), ci=0, data=d)
