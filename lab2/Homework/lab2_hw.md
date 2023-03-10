---
title: "Lab 2 Homework"
author: "Akshanth Srivatsa"
date: "2023-01-15"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
  pdf_document:
    toc: no
---

## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

1. What is a vector in R?  
A vector is a way to organize data. It holds multiple data values into one data type. Vectors can be based on data types, like numeric or integer.

2. What is a data matrix in R?  
A data matrix is tool for organization as well. They hold multiple sets of data in rows and columns. They are similar to a table in the sense that one can find a particular point at (row, column)


3. Below are data collected by three scientists (Jill, Steve, Susan in order) measuring temperatures of eight hot springs. Run this code chunk to create the vectors.  

```r
spring_1 <- c(36.25, 35.40, 35.30)
spring_2 <- c(35.15, 35.35, 33.35)
spring_3 <- c(30.70, 29.65, 29.20)
spring_4 <- c(39.70, 40.05, 38.65)
spring_5 <- c(31.85, 31.40, 29.30)
spring_6 <- c(30.20, 30.65, 29.75)
spring_7 <- c(32.90, 32.50, 32.80)
spring_8 <- c(36.80, 36.45, 33.15)
```


4. Build a data matrix that has the springs as rows and the columns as scientists.  

```r
springs<- c(spring_1,spring_2, spring_3, spring_4,spring_5, spring_6, spring_7, spring_8)
measurement_of_SpringTemp<- matrix(springs, nrow = 8, ncol = 3)
measurement_of_SpringTemp
```

```
##       [,1]  [,2]  [,3]
## [1,] 36.25 29.20 30.65
## [2,] 35.40 39.70 29.75
## [3,] 35.30 40.05 32.90
## [4,] 35.15 38.65 32.50
## [5,] 35.35 31.85 32.80
## [6,] 33.35 31.40 36.80
## [7,] 30.70 29.30 36.45
## [8,] 29.65 30.20 33.15
```

5. The names of the springs are 1.Bluebell Spring, 2.Opal Spring, 3.Riverside Spring, 4.Too Hot Spring, 5.Mystery Spring, 6.Emerald Spring, 7.Black Spring, 8.Pearl Spring. Name the rows and columns in the data matrix. Start by making two new vectors with the names, then use `colnames()` and `rownames()` to name the columns and rows.

```r
springNames<- c("1. Bluebell Spring", "2.Opal_Spring", "3.Riverside Spring", "4.Too Hot Spring", "5.Mystery Spring", "6.Emerald Spring", "7.Black Spring", "8.Pearl_Spring")
measurement_personele<-c("Jill", "Steve", "Susan")
  rownames(measurement_of_SpringTemp)<-springNames
  colnames(measurement_of_SpringTemp)<-measurement_personele
  measurement_of_SpringTemp
```

```
##                     Jill Steve Susan
## 1. Bluebell Spring 36.25 29.20 30.65
## 2.Opal_Spring      35.40 39.70 29.75
## 3.Riverside Spring 35.30 40.05 32.90
## 4.Too Hot Spring   35.15 38.65 32.50
## 5.Mystery Spring   35.35 31.85 32.80
## 6.Emerald Spring   33.35 31.40 36.80
## 7.Black Spring     30.70 29.30 36.45
## 8.Pearl_Spring     29.65 30.20 33.15
```

6. Calculate the mean temperature of all eight springs.

```r
mean_1<-mean(measurement_of_SpringTemp[1,])
mean_2<-mean(measurement_of_SpringTemp[2,])
mean_3<-mean(measurement_of_SpringTemp[3,])
mean_4<-mean(measurement_of_SpringTemp[4,])
mean_5<-mean(measurement_of_SpringTemp[5,])
mean_6<-mean(measurement_of_SpringTemp[6,])
mean_7<-mean(measurement_of_SpringTemp[7,])
mean_8<-mean(measurement_of_SpringTemp[8,])


mean_1
```

```
## [1] 32.03333
```

```r
mean_2
```

```
## [1] 34.95
```

```r
mean_3
```

```
## [1] 36.08333
```

```r
mean_4
```

```
## [1] 35.43333
```

```r
mean_5
```

```
## [1] 33.33333
```

```r
mean_6
```

```
## [1] 33.85
```

```r
mean_7
```

```
## [1] 32.15
```

```r
mean_8
```

```
## [1] 31
```


7. Add this as a new column in the data matrix.  

```r
new_means<-c(mean_1,mean_2, mean_3,mean_4,mean_5,mean_6,mean_7,mean_8)
measurement_of_SpringTemp<-cbind(measurement_of_SpringTemp,new_means)
measurement_of_SpringTemp
```

```
##                     Jill Steve Susan new_means
## 1. Bluebell Spring 36.25 29.20 30.65  32.03333
## 2.Opal_Spring      35.40 39.70 29.75  34.95000
## 3.Riverside Spring 35.30 40.05 32.90  36.08333
## 4.Too Hot Spring   35.15 38.65 32.50  35.43333
## 5.Mystery Spring   35.35 31.85 32.80  33.33333
## 6.Emerald Spring   33.35 31.40 36.80  33.85000
## 7.Black Spring     30.70 29.30 36.45  32.15000
## 8.Pearl_Spring     29.65 30.20 33.15  31.00000
```

8. Show Susan's value for Opal Spring only.

```r
measurement_of_SpringTemp[2,4]
```

```
## [1] 34.95
```

9. Calculate the mean for Jill's column only.  

```r
meanCol1<-mean(measurement_of_SpringTemp[,1])
meanCol1
```

```
## [1] 33.89375
```

10. Use the data matrix to perform one calculation or operation of your interest.

```r
fahrenheitMeanTemp<-measurement_of_SpringTemp[,4]*1.8+32
measurement_of_SpringTemp<-cbind(measurement_of_SpringTemp,fahrenheitMeanTemp)
measurement_of_SpringTemp
```

```
##                     Jill Steve Susan new_means fahrenheitMeanTemp
## 1. Bluebell Spring 36.25 29.20 30.65  32.03333              89.66
## 2.Opal_Spring      35.40 39.70 29.75  34.95000              94.91
## 3.Riverside Spring 35.30 40.05 32.90  36.08333              96.95
## 4.Too Hot Spring   35.15 38.65 32.50  35.43333              95.78
## 5.Mystery Spring   35.35 31.85 32.80  33.33333              92.00
## 6.Emerald Spring   33.35 31.40 36.80  33.85000              92.93
## 7.Black Spring     30.70 29.30 36.45  32.15000              89.87
## 8.Pearl_Spring     29.65 30.20 33.15  31.00000              87.80
```

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.  
