#Outlier

```r
outlier <- function(x) {
    outls <- rep(FALSE, length(x))
    deviation <- 1.5 * (quantile(x, 0.75) - quantile(x, 0.25))
    for (n in 1:length(x)) {
        if ((x[n] < (quantile(x, 0.25) - deviation)) | (x[n] > (quantile(x, 
            0.75) + deviation))) {
            outls[n] <- TRUE
        }
    }
    return(outls)
}
test <- c(1, 7, 8, 9, 10, 7, 6)
outlier(test)
```

```
## [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
```

#Numbers and languages

```r
digitToWord <- function(x, v) {
    res <- v[x]
    return(res)
}
digitToWord(c(9, 8, 7, 6, 5, 4, 3, 2, 1), c("jedan", "dva", "tri", "cetri", 
    "pet", "sest", "sedam", "osam", "devet"))  #example in Serbian language
```

```
## [1] "devet" "osam"  "sedam" "sest"  "pet"   "cetri" "tri"   "dva"   "jedan"
```

```r
digitToWord(c(9, 8, 7, 6, 5, 4, 3, 2, 1), c("one", "two", "three", "four", "five", 
    "six", "seven", "eight", "nine"))  #example in English language
```

```
## [1] "nine"  "eight" "seven" "six"   "five"  "four"  "three" "two"   "one"
```

#Crossword Puzzles

```r
lettersMatch <- function(words, pattern) {
    res <- c()
    for (n in 1:length(words)) {
        if (grepl(pattern, words[n]) == TRUE) {
            res <- c(res, words[n])
        }
    }
    return(res)
}
lettersMatch(c("blah", "bla", "stk"), "^...$")
```

```
## [1] "bla" "stk"
```






