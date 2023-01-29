---
title: "R Cheat Sheet"
output: 
  html_document: 
    keep_md: yes
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
    theme: spacelab
    number_sections: yes
date: "2023-01-28"
---


# Refrence text for formatting and publishing website in R
[For publishing in R](https://bookdown.org/yihui/rmarkdown/)

# lab 1
Getting working directory

```r
getwd()
```

```
## [1] "/Users/akshanth/Desktop/BIS15W2023_asrivatsa"
```
## Doing aritmatic

You can do arithmatic in R. Order of operations do apply.
Basic Operations: +, -, /, *
Note (**) is basically the exponent (^) function

## Creating a vector
you can create a vector by 'x<- c(observation1, obs2, obs3,... )'
