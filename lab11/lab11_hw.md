---
title: "Lab 11 Homework"
author: "Akshanth Srivatsa"
date: "2023-02-20"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above. For any included plots, make sure they are clearly labeled. You are free to use any plot type that you feel best communicates the results of your analysis.  

**In this homework, you should make use of the aesthetics you have learned. It's OK to be flashy!**

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

```r
library(tidyverse)
library(janitor)
library(here)
library(naniar)
```

## Resources
The idea for this assignment came from [Rebecca Barter's](http://www.rebeccabarter.com/blog/2017-11-17-ggplot2_tutorial/) ggplot tutorial so if you get stuck this is a good place to have a look.  

## Gapminder
For this assignment, we are going to use the dataset [gapminder](https://cran.r-project.org/web/packages/gapminder/index.html). Gapminder includes information about economics, population, and life expectancy from countries all over the world. You will need to install it before use. This is the same data that we will use for midterm 2 so this is good practice.

```r
#install.packages("gapminder")
library("gapminder")
```

## Questions
The questions below are open-ended and have many possible solutions. Your approach should, where appropriate, include numerical summaries and visuals. Be creative; assume you are building an analysis that you would ultimately present to an audience of stakeholders. Feel free to try out different `geoms` if they more clearly present your results.  

**1. Use the function(s) of your choice to get an idea of the overall structure of the data frame, including its dimensions, column names, variable classes, etc. As part of this, determine how NA's are treated in the data.**  

```r
glimpse(gapminder)
```

```
## Rows: 1,704
## Columns: 6
## $ country   <fct> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", …
## $ continent <fct> Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, …
## $ year      <int> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, …
## $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.854, 40.8…
## $ pop       <int> 8425333, 9240934, 10267083, 11537966, 13079460, 14880372, 12…
## $ gdpPercap <dbl> 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 786.1134, …
```

```r
names(gapminder)
```

```
## [1] "country"   "continent" "year"      "lifeExp"   "pop"       "gdpPercap"
```

```r
colSums(is.na(gapminder))
```

```
##   country continent      year   lifeExp       pop gdpPercap 
##         0         0         0         0         0         0
```

**2. Among the interesting variables in gapminder is life expectancy. How has global life expectancy changed between 1952 and 2007?**

```r
years_52To_07<-gapminder %>% 
  filter(between(year, 1952, 2007))
```


```r
years_52To_07 %>% 
  ggplot(aes(group=year,x=year, y=lifeExp, fill=year))+geom_col()
```

![](lab11_hw_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



**3. How do the distributions of life expectancy compare for the years 1952 and 2007?**



```r
gapminder %>% 
  filter(year== 1952|year== 2007) %>% 
  ggplot(aes(group=year,x=year, y=lifeExp))+geom_boxplot()
```

![](lab11_hw_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

**4. Your answer above doesn't tell the whole story since life expectancy varies by region. Make a summary that shows the min, mean, and max life expectancy by continent for all years represented in the data.**


```r
gapminder %>% 
  group_by(continent) %>% 
  summarise("min"=min(lifeExp),"max"=max(lifeExp),"mean"=mean(lifeExp)) %>% 
  arrange()
```

```
## # A tibble: 5 × 4
##   continent   min   max  mean
##   <fct>     <dbl> <dbl> <dbl>
## 1 Africa     23.6  76.4  48.9
## 2 Americas   37.6  80.7  64.7
## 3 Asia       28.8  82.6  60.1
## 4 Europe     43.6  81.8  71.9
## 5 Oceania    69.1  81.2  74.3
```

**5. How has life expectancy changed between 1952-2007 for each continent?**

```r
gapminder %>% 
  select(continent, year, lifeExp) %>%
  group_by(continent) %>% 
  mutate(year=as.factor(year)) %>% 
   ggplot(aes(x=lifeExp, y=year, fill=continent ))+geom_boxplot(alpha=0.5) +facet_grid(continent~.)+
  theme(axis.text.y = element_text(angle = 15,size = 6))
```

![](lab11_hw_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

**6. We are interested in the relationship between per capita GDP and life expectancy; i.e. does having more money help you live longer?**

```r
gapminder %>%  
  mutate(continent=as.factor(continent)) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp,color=continent))+ geom_point()+facet_grid(continent~.)
```

![](lab11_hw_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

**7. Which countries have had the largest population growth since 1952?**

```r
options(scipen=999)


gapminder %>% 
  filter(year>=1952) %>% 
  select(country,pop,year) %>% 
pivot_wider(names_from = year, names_prefix = "year_",values_from = pop) %>% mutate(deltaOfPopGrowth=year_2007-year_1952) %>% 
  arrange(-deltaOfPopGrowth) %>% 
  head(10) %>%
  select(country)
```

```
## # A tibble: 10 × 1
##    country      
##    <fct>        
##  1 China        
##  2 India        
##  3 United States
##  4 Indonesia    
##  5 Brazil       
##  6 Pakistan     
##  7 Bangladesh   
##  8 Nigeria      
##  9 Mexico       
## 10 Philippines
```

**8. Use your results from the question above to plot population growth for the top five countries since 1952.**

```r
gapminder %>% 
  filter(year>=1952) %>% 
  select(country,pop,year) %>% 
pivot_wider(names_from = year, names_prefix = "year_",values_from = pop) %>% mutate(deltaOfPopGrowth=year_2007-year_1952) %>% 
  arrange(-deltaOfPopGrowth) %>% 
  head(5) %>% 
  ggplot(aes(x=country,y=deltaOfPopGrowth, fill=country))+geom_col()+theme(axis.text.x = element_text(angle = 11,size = 10))
```

![](lab11_hw_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

**9. How does per-capita GDP growth compare between these same five countries?**

```r
gapminder %>% 
  filter(country=="Brazil"|country=="China"|
           country=="India"|country=="Indonesia"|country=="United States")%>% 
  select(country,year,gdpPercap) %>% 
  pivot_wider(names_from = year, names_prefix = "year_",values_from = gdpPercap) %>% 
  mutate(deltaOfGDPGrowth=year_2007-year_1952) %>% 
  ggplot(aes(x=country,y=deltaOfGDPGrowth, fill=country))+geom_col()+theme(axis.text.x = element_text(angle = 11,size = 10))
```

![](lab11_hw_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

**10. Make one plot of your choice that uses faceting!**

```r
gapminder %>%  
  mutate(continent=as.factor(continent)) %>% 
  ggplot(aes(x=gdpPercap, y=pop,color=continent))+ geom_point()+facet_grid(continent~.)
```

![](lab11_hw_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences. 
