---
title: "Lab 9 Homework"
author: "Please Add Your Name Here"
date: "2023-02-13"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

```r
library(tidyverse)
library(janitor)
library(here)
library(naniar)
```

For this homework, we will take a departure from biological data and use data about California colleges. These data are a subset of the national college scorecard (https://collegescorecard.ed.gov/data/). Load the `ca_college_data.csv` as a new object called `colleges`.

```r
colleges<- readr::read_csv(here("lab9","data","ca_college_data.csv"))%>%
  clean_names()
```

```
## Rows: 341 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (4): INSTNM, CITY, STABBR, ZIP
## dbl (6): ADM_RATE, SAT_AVG, PCIP26, COSTT4_A, C150_4_POOLED, PFTFTUG1_EF
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

The variables are a bit hard to decipher, here is a key:  

INSTNM: Institution name  
CITY: California city  
STABBR: Location state  
ZIP: Zip code  
ADM_RATE: Admission rate  
SAT_AVG: SAT average score  
PCIP26: Percentage of degrees awarded in Biological And Biomedical Sciences  
COSTT4_A: Annual cost of attendance  
C150_4_POOLED: 4-year completion rate  
PFTFTUG1_EF: Percentage of undergraduate students who are first-time, full-time degree/certificate-seeking undergraduate students  

1. Use your preferred function(s) to have a look at the data and get an idea of its structure. Make sure you summarize NA's and determine whether or not the data are tidy. You may also consider dealing with any naming issues.

```r
str(colleges)
```

```
## spc_tbl_ [341 × 10] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ instnm       : chr [1:341] "Grossmont College" "College of the Sequoias" "College of San Mateo" "Ventura College" ...
##  $ city         : chr [1:341] "El Cajon" "Visalia" "San Mateo" "Ventura" ...
##  $ stabbr       : chr [1:341] "CA" "CA" "CA" "CA" ...
##  $ zip          : chr [1:341] "92020-1799" "93277-2214" "94402-3784" "93003-3872" ...
##  $ adm_rate     : num [1:341] NA NA NA NA NA NA NA NA NA NA ...
##  $ sat_avg      : num [1:341] NA NA NA NA NA NA NA NA NA NA ...
##  $ pcip26       : num [1:341] 0.0016 0.0066 0.0038 0.0035 0.0085 0.0151 0 0.002 0.0021 0.0324 ...
##  $ costt4_a     : num [1:341] 7956 8109 8278 8407 8516 ...
##  $ c150_4_pooled: num [1:341] NA NA NA NA NA ...
##  $ pftftug1_ef  : num [1:341] 0.355 0.541 0.357 0.382 0.275 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   INSTNM = col_character(),
##   ..   CITY = col_character(),
##   ..   STABBR = col_character(),
##   ..   ZIP = col_character(),
##   ..   ADM_RATE = col_double(),
##   ..   SAT_AVG = col_double(),
##   ..   PCIP26 = col_double(),
##   ..   COSTT4_A = col_double(),
##   ..   C150_4_POOLED = col_double(),
##   ..   PFTFTUG1_EF = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```


```r
naniar::add_any_miss(colleges)%>%
  filter(any_miss_all!="complete")
```

```
## # A tibble: 276 × 11
##    instnm      city  stabbr zip   adm_r…¹ sat_avg pcip26 costt…² c150_…³ pftft…⁴
##    <chr>       <chr> <chr>  <chr>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
##  1 Grossmont … El C… CA     9202…      NA      NA 0.0016    7956  NA       0.355
##  2 College of… Visa… CA     9327…      NA      NA 0.0066    8109  NA       0.541
##  3 College of… San … CA     9440…      NA      NA 0.0038    8278  NA       0.357
##  4 Ventura Co… Vent… CA     9300…      NA      NA 0.0035    8407  NA       0.382
##  5 Oxnard Col… Oxna… CA     9303…      NA      NA 0.0085    8516  NA       0.275
##  6 Moorpark C… Moor… CA     9302…      NA      NA 0.0151    8577  NA       0.429
##  7 Skyline Co… San … CA     9406…      NA      NA 0         8580   0.233   0.231
##  8 Glendale C… Glen… CA     9120…      NA      NA 0.002     9181  NA       0.421
##  9 Citrus Col… Glen… CA     9174…      NA      NA 0.0021    9281  NA       0.440
## 10 Fresno Cit… Fres… CA     93741      NA      NA 0.0324    9370  NA       0.366
## # … with 266 more rows, 1 more variable: any_miss_all <chr>, and abbreviated
## #   variable names ¹​adm_rate, ²​costt4_a, ³​c150_4_pooled, ⁴​pftftug1_ef
```


```r
colleges
```

```
## # A tibble: 341 × 10
##    instnm      city  stabbr zip   adm_r…¹ sat_avg pcip26 costt…² c150_…³ pftft…⁴
##    <chr>       <chr> <chr>  <chr>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
##  1 Grossmont … El C… CA     9202…      NA      NA 0.0016    7956  NA       0.355
##  2 College of… Visa… CA     9327…      NA      NA 0.0066    8109  NA       0.541
##  3 College of… San … CA     9440…      NA      NA 0.0038    8278  NA       0.357
##  4 Ventura Co… Vent… CA     9300…      NA      NA 0.0035    8407  NA       0.382
##  5 Oxnard Col… Oxna… CA     9303…      NA      NA 0.0085    8516  NA       0.275
##  6 Moorpark C… Moor… CA     9302…      NA      NA 0.0151    8577  NA       0.429
##  7 Skyline Co… San … CA     9406…      NA      NA 0         8580   0.233   0.231
##  8 Glendale C… Glen… CA     9120…      NA      NA 0.002     9181  NA       0.421
##  9 Citrus Col… Glen… CA     9174…      NA      NA 0.0021    9281  NA       0.440
## 10 Fresno Cit… Fres… CA     93741      NA      NA 0.0324    9370  NA       0.366
## # … with 331 more rows, and abbreviated variable names ¹​adm_rate, ²​costt4_a,
## #   ³​c150_4_pooled, ⁴​pftftug1_ef
```

2. Which cities in California have the highest number of colleges?

```r
city_colleges<-colleges%>%
  count(city)%>%
  arrange(-c(n))
city_colleges
```

```
## # A tibble: 161 × 2
##    city              n
##    <chr>         <int>
##  1 Los Angeles      24
##  2 San Diego        18
##  3 San Francisco    15
##  4 Sacramento       10
##  5 Berkeley          9
##  6 Oakland           9
##  7 Claremont         7
##  8 Pasadena          6
##  9 Fresno            5
## 10 Irvine            5
## # … with 151 more rows
```

3. Based on your answer to #2, make a plot that shows the number of colleges in the top 10 cities.

```r
city_colleges%>%
  top_n(10)%>%
  ggplot(aes(city,n))+geom_col()+coord_flip()
```

```
## Selecting by n
```

![](lab9_hw_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

4. The column `COSTT4_A` is the annual cost of each institution. Which city has the highest average cost? Where is it located?

```r
colleges%>%group_by(city)%>%summarise(costt4_a=mean(costt4_a,na.rm = T))%>%
  arrange(desc(costt4_a))%>%
  top_n(1)
```

```
## Selecting by costt4_a
```

```
## # A tibble: 1 × 2
##   city      costt4_a
##   <chr>        <dbl>
## 1 Claremont    66498
```

5. Based on your answer to #4, make a plot that compares the cost of the individual colleges in the most expensive city. Bonus! Add UC Davis here to see how it compares :>).

```r
cities<-colleges%>%filter(city=="Claremont")
davis<-colleges%>%filter(instnm=="University of California-Davis")
cities<-rbind(cities,davis)

cities%>%ggplot(aes(instnm,costt4_a))+geom_col()+coord_flip()
```

```
## Warning: Removed 2 rows containing missing values (`position_stack()`).
```

![](lab9_hw_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

6. The column `ADM_RATE` is the admissions rate by college and `C150_4_POOLED` is the four-year completion rate. Use a scatterplot to show the relationship between these two variables. What do you think this means?

as admission rate increases, the four year completion rate decreases proportionally

```r
cities%>%ggplot(aes(adm_rate,c150_4_pooled))+geom_point()
```

```
## Warning: Removed 2 rows containing missing values (`geom_point()`).
```

![](lab9_hw_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

7. Is there a relationship between cost and four-year completion rate? (You don't need to do the stats, just produce a plot). What do you think this means?

```r
cities%>%ggplot(aes(costt4_a,c150_4_pooled))+geom_point()
```

```
## Warning: Removed 2 rows containing missing values (`geom_point()`).
```

![](lab9_hw_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

8. The column titled `INSTNM` is the institution name. We are only interested in the University of California colleges. Make a new data frame that is restricted to UC institutions. You can remove `Hastings College of Law` and `UC San Francisco` as we are only interested in undergraduate institutions.

```r
?filter_if
```

```
## starting httpd help server ... done
```

```r
colleges%>%
  filter_all(any_vars(str_detect(., pattern = "University of California")))%>%
  filter(instnm!="University of California-Hastings College of Law"& instnm!="University of California-San Francisco")
```

```
## # A tibble: 8 × 10
##   instnm       city  stabbr zip   adm_r…¹ sat_avg pcip26 costt…² c150_…³ pftft…⁴
##   <chr>        <chr> <chr>  <chr>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
## 1 University … La J… CA     92093   0.357    1324  0.216   31043   0.872   0.662
## 2 University … Irvi… CA     92697   0.406    1206  0.107   31198   0.876   0.725
## 3 University … Rive… CA     92521   0.663    1078  0.149   31494   0.73    0.811
## 4 University … Los … CA     9009…   0.180    1334  0.155   33078   0.911   0.661
## 5 University … Davis CA     9561…   0.423    1218  0.198   33904   0.850   0.605
## 6 University … Sant… CA     9506…   0.578    1201  0.193   34608   0.776   0.786
## 7 University … Berk… CA     94720   0.169    1422  0.105   34924   0.916   0.709
## 8 University … Sant… CA     93106   0.358    1281  0.108   34998   0.816   0.708
## # … with abbreviated variable names ¹​adm_rate, ²​costt4_a, ³​c150_4_pooled,
## #   ⁴​pftftug1_ef
```

Remove `Hastings College of Law` and `UC San Francisco` and store the final data frame as a new object `univ_calif_final`.

```r
univ_calif_final<-colleges%>%
  filter_all(any_vars(str_detect(., pattern = "University of California")))%>%
  filter(instnm!="University of California-Hastings College of Law"& instnm!="University of California-San Francisco")
```

Use `separate()` to separate institution name into two new columns "UNIV" and "CAMPUS".

```r
univ_calif_final<-univ_calif_final%>%
  separate(instnm,into=c("univ", "campus"), sep="-" )
```

9. The column `ADM_RATE` is the admissions rate by campus. Which UC has the lowest and highest admissions rates? Produce a numerical summary and an appropriate plot.

```r
univ_calif_final%>%
  summarise(campus,adm_rate)%>%arrange(adm_rate)
```

```
## # A tibble: 8 × 2
##   campus        adm_rate
##   <chr>            <dbl>
## 1 Berkeley         0.169
## 2 Los Angeles      0.180
## 3 San Diego        0.357
## 4 Santa Barbara    0.358
## 5 Irvine           0.406
## 6 Davis            0.423
## 7 Santa Cruz       0.578
## 8 Riverside        0.663
```


```r
univ_calif_final%>%
  ggplot(aes(campus, adm_rate))+geom_col()
```

![](lab9_hw_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

10. If you wanted to get a degree in biological or biomedical sciences, which campus confers the majority of these degrees? Produce a numerical summary and an appropriate plot.

```r
univ_calif_final%>%
  summarise(campus,pcip26)%>%
  arrange(pcip26)
```

```
## # A tibble: 8 × 2
##   campus        pcip26
##   <chr>          <dbl>
## 1 Berkeley       0.105
## 2 Irvine         0.107
## 3 Santa Barbara  0.108
## 4 Riverside      0.149
## 5 Los Angeles    0.155
## 6 Santa Cruz     0.193
## 7 Davis          0.198
## 8 San Diego      0.216
```


```r
univ_calif_final%>%
  ggplot(aes(campus, pcip26))+geom_col()
```

![](lab9_hw_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

## Knit Your Output and Post to [GitHub](https://github.com/FRS417-DataScienceBiologists)
