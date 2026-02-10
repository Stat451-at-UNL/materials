p = 3
a = 1.22
b = 3.34
c = 2.28

midnight <- function(a, b, c, p=3) {
  D <- signif(sqrt(signif(b^2,p)-signif(4*a*c,p)),p)

  signif(c(signif(-b+D,p), signif(-b-D,p))/(2*a), p)
}


midnight2 <- function(a, b, c, p=3) {
#  https://math.stackexchange.com/questions/866331/numerically-stable-algorithm-for-solving-the-quadratic-equation-when-a-is-very
  D <- signif(sqrt(signif(b^2,p)-signif(4*a*c,p)), p)
  x1 <- signif(-b-signif(sign(b)*D,p), p)/signif(2*a, p)

  signif(c(x1, signif(c/(a*x1),p)),p)
}

midnight3 <- function(a, b, c, p=3) {

  # write ax^2 + bx + c as (ax + p)(x + c/p) with
  # p2*q = a*c
  # p2 + q = b

  q <- signif(c/2,p)
  p2 <- signif(b-q_old, p)
  repeat {
    # set q
    q_old <- q
    q <- signif(a*c/p_old, p)
    # set p
    p_old <- p2
    p2 <- signif(b-q, p)

    if (((abs(q_old-q) < 10^(p+1)) & (p_old - p2) < 10^(p+1))) break
  }

  signif(c(-c/q, q/a),p)
}


relerr <- function(obs, exp) {
  (obs-exp)/exp
}

res <- midnight(a, b, c)
fullres <- midnight(a, b, c, p = 17)
relerr(res, fullres)

res2 <- midnight2(a, b, c)
fullres2 <- midnight2(a, b, c, p = 17)
relerr(res2, fullres2)

res3 <- midnight3(a, b, c)
fullres3 <- midnight3(a, b, c, p = 17)
relerr(res3, fullres3)


a*res[1]^2 + b*res[1] + c
a*res[2]^2 + b*res[2] + c

a*res2[1]^2 + b*res2[1] + c
a*res2[2]^2 + b*res2[2] + c


f <- function(x) {
  a*x^2+b*x+c
}

relerr(uniroot(f, interval = c(-2, -1.4))$root, fullres[2])

uniroot(f, interval = c(-2, -1.4), tol=.Machine$double.eps^0.5)

a <- 1
b <- 2.31
c <- 1.33
