#Outlier
```{r}
outlier <- function(x) {
  outls <- rep(FALSE, length(x))
  deviation <- 1.5*(quantile(x, .75)-quantile(x, .25))
  for (n in 1:length(x)) {
    if (  (x[n] < (quantile(x, .25)-deviation)) | (x[n] > (quantile(x, .75)+deviation))  )  {
      outls[n] <- TRUE
      }
    } 
  return(outls)
  } 
test <- c(1,7,8,9,10,7,6)
outlier(test)
```
#Numbers and languages
```{r}
digitToWord <- function (x,v) {
  res <- v[x]
  return(res)
}
digitToWord(c(9,8,7,6,5,4,3,2,1), c("jedan", "dva", "tri", "cetri", "pet", "sest", "sedam", "osam", "devet")) #example in Serbian language
digitToWord(c(9,8,7,6,5,4,3,2,1), c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")) #example in English language
```
#Crossword Puzzles
```{r}
lettersMatch <- function(words, pattern) {
  res <- c()
  for (n in 1:length(words)) {
    if (grepl(pattern, words[n]) == TRUE) {
    res <- c(res, words[n] )
    }
  }
  return(res)
}
lettersMatch(c("blah","bla","stk"), "^...$")
```
#Pi Series
```{r}

piSeries <- function(n) {
  n <- c(1:n)
    pi <- (4 * sum( ((-1)^(n-1)) / (2*(n-1)+1)))
  return(pi) 
}
piSeries(100000)

howCloseToPi <- function(n) {
  n <- c(1:n)
  pi <- (4 * cumsum( ((-1)^(n-1)) / (2*(n-1)+1)))
  plot(n, pi)
  return(pi)
  }
#we would need exactly 152 terms to approximate it to 3 digits
#approximately 10^10 terms for 15 digits

```




