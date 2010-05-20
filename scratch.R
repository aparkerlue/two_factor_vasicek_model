## Optimization example.
f <- function(x)  -(x[1] - 3)^2 - 5*x[2]^2 + 5
optim(c(0,0), f,
      control=list(fnscale=-1))         # maximize: fnscale < 0

## Read in CMT data.
d <- read.table("homework5data.txt", header=TRUE, sep="\t")
head(d[,c(1:3,11:16)])                  # Relevant columns.
