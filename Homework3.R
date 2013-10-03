countOdds <- function(x) {
  k <- 0
  for (n in x) {
    if (n%%2 == 1) k <- k+1     
  }
  return(k)
}


countEvens <- function(x) {
  k <- 0
  for (n in x) {
    if (n%%2 == 0) k <- k+1     
  }
  return(k)
}


hypothenuseLength <- function(a, b) {
  a > 0
  b > 0
  return(sqrt(a^2 + b^2))
}


lawofCosines <- function(a,b,t) {
  a > 0
  b > 0
  t > 0 & t < 2*pi
return(sqrt(a^2+b^2 - 2*a*b*cos(t)))
}


thetafromLengths = function(a,b,c) {
  a > 0
  b > 0
  c > 0
  return(acos((a^2 + b^2 - c^2) / (2*a*b)))
}


thetaFromLengthsTest = function(a,b,t) {
  a > 0
  b > 0
  t > 0 & t < 2*pi
  c <- sqrt(a^2+b^2 - 2*a*b*cos(t))
  theta = acos((a^2 + b^2 - c^2) / (2*a*b))
  return(theta - t)
}           #when I run it, it returns 0, which it should, because it compares theta to theta
  

