model.a <- function(t, a, b, s)
  exp((s^2/(2*b^2) - a/b)*t +
      (a/b^2 - s^2/b^3)*(1 - exp(-b*t)) +
      s^2/(4*b^3)*(1 - exp(2*b*t)))

model.b <- function(t, b)  1/b * (1 - exp(-b*t))

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
