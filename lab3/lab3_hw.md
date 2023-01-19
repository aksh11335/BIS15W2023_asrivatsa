---
title: "Lab 3 Homework"
author: "Akshanth Srivatsa"
date: "2023-01-18"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---

## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the tidyverse

```r
library(tidyverse)
```

## Mammals Sleep
1. For this assignment, we are going to use built-in data on mammal sleep patterns. From which publication are these data taken from? Since the data are built-in you can use the help function in R.

We are going to use the mammals sleep database called "msleep" in the R folder. This is taken from V. M. Savage and G. B. West, in the publication "A quantitative, theoretical framework for understanding mammalian sleep."

```r
?msleep
```

2. Store these data into a new data frame `sleep`.

```r
sleep<-msleep
```

3. What are the dimensions of this data frame (variables and observations)? How do you know? Please show the *code* that you used to determine this below.  

I checked the dimensions using the dim() command. This tells me the dimensions of the dataFrame, along with each observation made with each mammal. 

```r
dim(sleep)
```

```
## [1] 83 11
```

4. Are there any NAs in the data? How did you determine this? Please show your code.  

I checked if there was any sleep data values in the data frame. Then I checked that amalgamation of data to see if there were any trues, indicating that there was a NA in the data set. Since the final result returned false, we know that there wasn't any NAs.

```r
isTRUE(is.na(sleep))
```

```
## [1] FALSE
```

5. Show a list of the column names is this data frame.

```r
colnames(sleep)
```

```
##  [1] "name"         "genus"        "vore"         "order"        "conservation"
##  [6] "sleep_total"  "sleep_rem"    "sleep_cycle"  "awake"        "brainwt"     
## [11] "bodywt"
```

6. How many herbivores are represented in the data?  

```r
table(sleep$vore)[2]
```

```
## herbi 
##    32
```

7. We are interested in two groups; small and large mammals. Let's define small as less than or equal to 1kg body weight and large as greater than or equal to 200kg body weight. Make two new dataframes (large and small) based on these parameters.


```r
large_mammals<-data.frame(filter(sleep,bodywt>=200 ))

small_mammals<-data.frame(filter(sleep,bodywt<=1 ))
```

8. What is the mean weight for both the small and large mammals?

```r
mean(large_mammals$bodywt)
```

```
## [1] 1747.071
```


```r
mean(small_mammals$bodywt)
```

```
## [1] 0.2596667
```

9. Using a similar approach as above, do large or small animals sleep longer on average?  

```r
smallMammalsSleepTotal<-mean(small_mammals$sleep_total)
largeMammalsSleepTotal<-mean(large_mammals$sleep_total)
```
Check if large mammals sleep more than small mammals:


```r
largeMammalsSleepTotal>smallMammalsSleepTotal
```

```
## [1] FALSE
```

This means that large mammals get less total sleep than the small mammals.


10. Which animal is the sleepiest among the entire dataframe?

```r
filter(sleep, sleep_total==max(sleep$sleep_total))$name
```

```
## [1] "Little brown bat"
```


## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
