## Pig Latin Function

##Rules

```r
rule2 <- function(word) {
    if (grepl("^[aeiou].+$", word) == TRUE) {
        res <- paste(word, "yay", sep = "", collapse = NULL)
    }
    return(res)
}


rule3 <- function(word) {
    if (grepl("^[^aeiou]+$", word) == TRUE) {
        res <- paste(word, "yay", sep = "", collapse = NULL)
    }
    return(res)
}


rule1 <- function(word) {
    if (grepl("^[^aeiou].+", word) == TRUE) {
        place <- as.numeric(regexpr("[aeiou]", word))
        first <- substr(word, start = 1, stop = place - 1)
        spl <- strsplit(word, split = "")[[1]]  #had to split it so that I can access the word's length
        second <- substr(word, start = place, stop = length(spl))
        secondfirst <- paste(second, first, sep = "", collapse = NULL)
        res <- paste(secondfirst, "ay", sep = "", collapse = NULL)
    }
    return(res)
}
```


##Recognizing a rule

```r
pigLatinRule <- function(word) {
    if (grepl("^[^aeiou]+$", word)) {
        res <- "novowels"
    } else {
        if (grepl("^[aeiou].+$", word)) {
            res <- "vowel"
        } else {
            if (grepl("^[^aeiou].+$", word)) {
                res <- "consonant"
            }
        }
    }
    return(res)
}
```


##Putting things together into one function:

```r
pigLatinizer <- function(word) {
    res <- switch(pigLatinRule(word), consonant = rule1(word), vowel = rule2(word), 
        novowels = rule3(word))
    return(res)
}
```


##Test Cases

```r
pigLatinizer("pig")
```

```
## [1] "igpay"
```

```r
pigLatinizer("airplane")
```

```
## [1] "airplaneyay"
```

```r
pigLatinizer("my")
```

```
## [1] "myyay"
```

