######################################################################
## Optimization example.                                            ##
######################################################################

f <- function(x)  -(x[1] - 3)^2 - 5*x[2]^2 + 5
## Unconstrained optimization.
optim(c(0,0), f,
      control=list(fnscale=-1))         # maximize: fnscale < 0
## Constrained optimization.
constrOptim(c(1, 1), f, NULL, ui=t(c(1, 0)), ci=0,
      control=list(fnscale=-1))         # maximize: fnscale < 0
