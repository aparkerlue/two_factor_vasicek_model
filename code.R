source('functions.R')

## Read in CMT data.
d <- read.table("homework5data.txt", header=TRUE, sep="\t")
d$cmt0.25 <- d$cmt0.25 / 100
d$cmt2 <- d$cmt2 / 100
d$cmt3 <- d$cmt3 / 100
d$cmt5 <- d$cmt5 / 100
d$cmt7 <- d$cmt7 / 100
d$cmt10 <- d$cmt10 / 100
head(d[,c(1:3,11:16)])                  # Relevant columns.
head(d[,c(12:15)])

## Assume values for parameters.
p <- c(a.x=0.0041109, b.x=0.0383212, s.x=0.0097854, b.y=0.448712, s.y=0.018566)

optim(p, vasicek.rmse, d=d)
