source('functions.R')

## Read in CMT data.
d <- read.table("homework5data.txt", header=TRUE, sep="\t")
head(d[,c(1:3,11:16)])                  # Relevant columns.
nrow(d)

## Assume values for parameters.
p <- c(a.x=0.1, b.x=0.1, s.x=0.3, b.y=0.1, s.y=0.3)

## Generate time series for factors X and Y.
r <- data.frame(x=numeric(0), y=numeric(0))
for (i in 1:nrow(d)) {
  xy <- factors(t1=0.25, y1=d$cmt0.25[i], t2=10, y2=d$cmt10[i],
                a.x=p[1], b.x=p[2], s.x=p[3], b.y=p[4], s.y=p[5])
  r <- rbind(r, c(xy))
}
names(r) <- c("x", "y")
