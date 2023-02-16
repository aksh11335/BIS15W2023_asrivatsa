---
title: "Lab 10 Homework"
author: "Please Add Your Name Here"
date: "2023-02-16"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above. For any included plots, make sure they are clearly labeled. You are free to use any plot type that you feel best communicates the results of your analysis.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

```r
library(tidyverse)
library(janitor)
library(here)
library(naniar)
```

## Desert Ecology
For this assignment, we are going to use a modified data set on [desert ecology](http://esapubs.org/archive/ecol/E090/118/). The data are from: S. K. Morgan Ernest, Thomas J. Valone, and James H. Brown. 2009. Long-term monitoring and experimental manipulation of a Chihuahuan Desert ecosystem near Portal, Arizona, USA. Ecology 90:1708.

```r
deserts <- read_csv(here("lab10", "data", "surveys_complete.csv"))
```

```
## Rows: 34786 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (6): species_id, sex, genus, species, taxa, plot_type
## dbl (7): record_id, month, day, year, plot_id, hindfoot_length, weight
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

1. Use the function(s) of your choice to get an idea of its structure, including how NA's are treated. Are the data tidy?  

```r
#View(deserts)
str(deserts)
```

```
## spc_tbl_ [34,786 × 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ record_id      : num [1:34786] 1 2 3 4 5 6 7 8 9 10 ...
##  $ month          : num [1:34786] 7 7 7 7 7 7 7 7 7 7 ...
##  $ day            : num [1:34786] 16 16 16 16 16 16 16 16 16 16 ...
##  $ year           : num [1:34786] 1977 1977 1977 1977 1977 ...
##  $ plot_id        : num [1:34786] 2 3 2 7 3 1 2 1 1 6 ...
##  $ species_id     : chr [1:34786] "NL" "NL" "DM" "DM" ...
##  $ sex            : chr [1:34786] "M" "M" "F" "M" ...
##  $ hindfoot_length: num [1:34786] 32 33 37 36 35 14 NA 37 34 20 ...
##  $ weight         : num [1:34786] NA NA NA NA NA NA NA NA NA NA ...
##  $ genus          : chr [1:34786] "Neotoma" "Neotoma" "Dipodomys" "Dipodomys" ...
##  $ species        : chr [1:34786] "albigula" "albigula" "merriami" "merriami" ...
##  $ taxa           : chr [1:34786] "Rodent" "Rodent" "Rodent" "Rodent" ...
##  $ plot_type      : chr [1:34786] "Control" "Long-term Krat Exclosure" "Control" "Rodent Exclosure" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   record_id = col_double(),
##   ..   month = col_double(),
##   ..   day = col_double(),
##   ..   year = col_double(),
##   ..   plot_id = col_double(),
##   ..   species_id = col_character(),
##   ..   sex = col_character(),
##   ..   hindfoot_length = col_double(),
##   ..   weight = col_double(),
##   ..   genus = col_character(),
##   ..   species = col_character(),
##   ..   taxa = col_character(),
##   ..   plot_type = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
deserts %>% 
naniar::add_any_miss() %>% 
  filter(any_miss_all!="complete")
```

```
## # A tibble: 4,110 × 14
##    record…¹ month   day  year plot_id speci…² sex   hindf…³ weight genus species
##       <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>   <chr>   <dbl>  <dbl> <chr> <chr>  
##  1        1     7    16  1977       2 NL      M          32     NA Neot… albigu…
##  2        2     7    16  1977       3 NL      M          33     NA Neot… albigu…
##  3        3     7    16  1977       2 DM      F          37     NA Dipo… merria…
##  4        4     7    16  1977       7 DM      M          36     NA Dipo… merria…
##  5        5     7    16  1977       3 DM      M          35     NA Dipo… merria…
##  6        6     7    16  1977       1 PF      M          14     NA Pero… flavus 
##  7        7     7    16  1977       2 PE      F          NA     NA Pero… eremic…
##  8        8     7    16  1977       1 DM      M          37     NA Dipo… merria…
##  9        9     7    16  1977       1 DM      F          34     NA Dipo… merria…
## 10       10     7    16  1977       6 PF      F          20     NA Pero… flavus 
## # … with 4,100 more rows, 3 more variables: taxa <chr>, plot_type <chr>,
## #   any_miss_all <chr>, and abbreviated variable names ¹​record_id, ²​species_id,
## #   ³​hindfoot_length
```

2. How many genera and species are represented in the data? What are the total number of observations? Which species is most/ least frequently sampled in the study?


```r
deserts %>% 
count(genus,species, sort = T) %>% 
  summarise("Total_number_of_observations"=sum(n),
            max(species),
            min(species))
```

```
## # A tibble: 1 × 3
##   Total_number_of_observations `max(species)` `min(species)`
##                          <int> <chr>          <chr>         
## 1                        34786 viridis        albigula
```

3. What is the proportion of taxa included in this study? Show a table and plot that reflects this count.

```r
deserts %>% 
  group_by(taxa) %>% ggplot(aes(taxa))+geom_bar()+scale_y_log10()+ylab("log10_count")
```

![](lab10_hw_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

4. For the taxa included in the study, use the fill option to show the proportion of individuals sampled by `plot_type.`

```r
deserts %>% 
  ggplot(aes(x = taxa, fill = taxa,plot_type=taxa))+
  geom_bar()+scale_y_log10()+ylab("log10_count")
```

![](lab10_hw_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

5. What is the range of weight for each species included in the study? Remove any observations of weight that are NA so they do not show up in the plot.

```r
deserts %>% filter(!is.na(weight))  %>% ggplot(aes(species,weight))+geom_boxplot()+coord_flip()
```

![](lab10_hw_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

6. Add another layer to your answer from #4 using `geom_point` to get an idea of how many measurements were taken for each species.

```r
deserts %>% filter(!is.na(weight))  %>% ggplot(aes(species,weight, fill(species)))+geom_point()+geom_boxplot()+coord_flip()
```

![](lab10_hw_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

7. [Dipodomys merriami](https://en.wikipedia.org/wiki/Merriam's_kangaroo_rat) is the most frequently sampled animal in the study. How have the number of observations of this species changed over the years included in the study?

```r
I_cant_think_of_a_variable_name<-deserts %>% 
  filter(species_id=="DM") %>% 
    group_by(year) %>% count(species_id)
  
I_cant_think_of_a_variable_name %>% 
  ggplot(aes(year, n))+geom_point()+ylab("Count of the species ID DM")+labs(title = "number of observations versus year")
```

![](lab10_hw_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

8. What is the relationship between `weight` and `hindfoot` length? Consider whether or not over plotting is an issue.
Is an issue, but I used jitter, and now we are sad

```r
deserts %>% 
  ggplot(aes(weight, hindfoot_length))+geom_density2d(na.rm = T)+geom_jitter()
```

```
## Warning: Removed 4048 rows containing missing values (`geom_point()`).
```

![](lab10_hw_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

9. Which two species have, on average, the highest weight? Once you have identified them, make a new column that is a ratio of `weight` to `hindfoot_length`. Make a plot that shows the range of this new ratio and fill by sex.

```r
deserts %>%group_by(species) %>%filter(!is.na(weight)) %>%
  summarise("average_weight"=mean(weight), ) %>% arrange(-average_weight) %>% head(2)
```

```
## # A tibble: 2 × 2
##   species     average_weight
##   <chr>                <dbl>
## 1 albigula              159.
## 2 spectabilis           120.
```

```r
new_plot<-deserts %>%  select(species,sex,weight,hindfoot_length)
  
new_plot<-new_plot %>% 
  mutate("ratio_of_weight_hindfoot_length"=weight/hindfoot_length) 

new_plot %>%filter(species=="albigula"|species=="spectabilis") %>%  ggplot(aes(x=species,y=`ratio_of_weight_hindfoot_length`, fill=sex))+geom_boxplot()
```

```
## Warning: Removed 684 rows containing non-finite values (`stat_boxplot()`).
```

![](lab10_hw_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

10. Make one plot of your choice! Make sure to include at least two of the aesthetics options you have learned.

```r
deserts %>% ggplot(aes(weight,hindfoot_length,fill=sex)) + geom_area()+labs(title = "Area curve of hindfootlength and weight",
       x = " weight",
       y = " Hindfoot Length")
```

```
## Warning: Removed 4048 rows containing non-finite values (`stat_align()`).
```

![](lab10_hw_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences. 
